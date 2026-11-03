estimate_target <- function(target = "value",
                            type,
                            K,
                            id,
                            action_set,
                            actions,
                            policy_actions,
                            policy_actions_natural = NULL,
                            policy_name,
                            policy_input_meta,
                            policy_output_meta = NULL,
                            events,
                            g_values,
                            q_values,
                            c_values,
                            m_values,
                            utility,
                            min_subgroup_size) {

  args <- as.list(environment())
  args[["target"]] <- NULL
  args[["type"]] <- NULL

  ## 'type' is kept as an input to policy_eval() so that future evaluation
  ## variants can be added, but currently only "dr" is implemented.
  ## policy_eval_input_checks() rejects any other value, so by the time
  ## we get here type is always "dr". The stop() below is a defensive
  ## guard in case estimate_target() is called directly.
  if (type != "dr") {
    stop("Only type = 'dr' is implemented.")
  }

  if (target == "value") {
    out <- do.call(what = "dr_value", args)
  } else if (target == "subgroup") {
    out <- do.call(what = "dr_subgroup", args)
  } else {
    stop("unknown target argument.")
  }

  return(out)
}

#' Create event indicator matrix
#'
#' Converts event data to a binary matrix where events in the event_set are coded as 1,
#' all others (including NA) as 0.
#'
#' @param events data.table/data.frame containing id, stage, and event columns
#' @param event_set vector of valid event codes, subset of c(0,1,2,NA)
#'
#' @return matrix with binary indicators (0/1) for each stage. Dimensions (#id X #stage).
#' @noRd
event_matrix <- function(events, event_set) {
  if (!data.table::is.data.table(events)) {
    stop("events must be a data.table")
  }
  required_cols <- c("id", "stage", "event")
  if (!all(required_cols %in% names(events))) {
    stop("events must contain columns: id, stage, and event")
  }
  valid_events <- c(0, 1, 2, NA)
  if (!all(event_set %in% c(valid_events))) {
    stop("event_set must be a subset of c(0,1,2,NA)")
  }

  M <- dcast(events, id ~ stage, value.var = "event")
  id <- NULL
  M[, id := NULL]
  M[, names(M) := lapply(.SD, function(x) as.numeric(x %in% event_set))]

  ## replace NA values with 0:
  setnafill(M, fill = 0)
  M <- as.matrix(M)
  return(M)
}


#' Calculate censoring probability matrix
#'
#' @details the probability for right-censored/missing events is set to 1
#' (in order to avoid division by 0)
#'
#' @param c_values data.table containing id, stage, surv_time, and surv_time2
#' @param M matrix of missing event indicators
#' @param n integer, number of subjects
#' @param K integer, number of stages
#' @param events data.table containing id, stage, and event for all events
#' @param E matrix of terminal event indicators
#'
#' @return matrix of censoring probabilities. Dimensions (n X (K+1)).
#' @noRd
cens_prob_matrix <- function(c_values, M, n, K, events, E) {
  if (!is.null(c_values)){
    if (!data.table::is.data.table(c_values)) {
      stop("c_values must be a data.table")
    }
    required_cols <- c("id", "stage", "surv_time", "surv_time2")
    if (!all(required_cols %in% names(c_values))) {
      stop("c_values must contain columns: ",
           paste(required_cols, collapse = ", "))
    }
    ## expand c_values to all events (fill NA)
    c_values <- merge(events[, c("id", "stage"), with = FALSE],
                      c_values,
                      all.x = TRUE)

    surv_time <- dcast(c_values, id ~ stage,
                       value.var = "surv_time")[, -c("id"), with = FALSE]
    surv_time2 <- dcast(c_values, id ~ stage,
                        value.var = "surv_time2")[, -c("id"), with = FALSE]
    C <- as.matrix(surv_time2 / surv_time)

    ## set probabilities to 1 for missing events:
    C[M == 1] <- 1
    ## set probabilities to 1 for terminal events for stages <= K:
    ## (terminal events before right-censoring)
    C[, 1:K][E[,1:K] == 1] <- 1

    ## check for invalid values:
    if (any(is.na(C)) || any(is.infinite(C))) {
      warning("NA or infinite values detected in probability matrix")
    }

  } else {
    C <- matrix(nrow = n, ncol = K+1, 1)
  }

  ## sanity check: expanding c_values over the full (id x stage) grid is
  ## supposed to guarantee a complete (n x K+1) matrix.
  stopifnot(nrow(C) == n, ncol(C) == K + 1)

  return(C)
}

#' Create policy action indicator matrix
#'
#' @details right-censored/missing and terminal events are coded as 1.
#'
#' @param actions data.table with columns: id, stage, A (observed actions)
#' @param policy_actions data.table with columns: id, stage, d (policy actions)
#' @param id data.table/data.frame with subject IDs
#' @param D matrix of non-censoring/non-missing indicators
#' @param E matrix of terminal event indicators
#'
#' @return matrix of indicators where 1 indicates matching actions. Dimensions (#id X (K+1)).
#' @noRd
policy_action_indicator_matrix <- function(actions, policy_actions, id, D, E) {
  n <- nrow(id)
  K <- ncol(D) - 1

  if (!(is.null(actions) && is.null(policy_actions))) {
    ## input validation:
    if (!data.table::is.data.table(actions) ||
        !data.table::is.data.table(policy_actions)) {
      stop("actions and policy_actions must be data.tables")
    }
    required_cols_actions <- c("id", "stage", "A")
    required_cols_policy <- c("id", "stage", "d")
    if (!all(required_cols_actions %in% names(actions))) {
      stop("actions must contain columns: ",
           paste(required_cols_actions, collapse = ", "))
    }
    if (!all(required_cols_policy %in% names(policy_actions))) {
      stop("policy_actions must contain columns: ",
           paste(required_cols_policy, collapse = ", "))
    }

    wide_actions <- dcast(
      actions,
      id ~ stage,
      value.var = "A"
    )
    wide_actions <- merge(id, wide_actions, all.x = TRUE)
    IIA <- wide_actions[, -c("id"), with = FALSE]
    IIA <- as.matrix(IIA)
    stopifnot(
      nrow(IIA) == n,
      ncol(IIA) == K
    )

    wide_policy_actions <- dcast(
      policy_actions, id ~ stage,
      value.var = "d"
    )
    wide_policy_actions <- merge(id, wide_policy_actions, all.x = TRUE)
    IId <- wide_policy_actions[, -c("id"), with = FALSE]
    IId <- as.matrix(IId)
    stopifnot(
      nrow(IId) == n,
      ncol(IId) == K
    )

    II <- (IIA == IId)
    rm(IIA, IId)
  } else {
    II <- matrix(nrow = n, ncol = K)
  }
  ## set I(A_{K+1} = d_{K+1}) = 1:
  II <- cbind(II, 1)
  ## replace value with 1 for right-censored/missing events (D == 0):
  II[D == 0] <- 1
  ## replace value with 1 for terminal events (E == 1)
  II[E == 1] <- 1

  if (!(is.null(actions) && is.null(policy_actions))) {
    if (any(is.na(II))) {
      warning("Missing values detected in actions or policy actions")
    }
  }

  colnames(II) <- 1:ncol(II)

  return(II)
}

#' Calculate action probability matrix
#'
#' @details Right-censored/mising and terminal events are coded as 1.
#'
#' @param g_values data.table containing the action probability values
#' @param id data.table with subject IDs
#' @param D matrix of non-censoring indicators
#' @param E matrix of terminal event indicators
#' @param action_set vector of possible actions
#'
#' @return matrix of action probabilities. Dimensions (n X (K+1)).
#' @noRd
action_prob_matrix <- function(g_values, actions, id, D, E, action_set) {
  if (!data.table::is.data.table(g_values) ||
      !data.table::is.data.table(actions)) {
    stop("g_values and actions must be data.tables")
  }

  values <- get_a_values(
    a = actions[["A"]],
    action_set = action_set,
    values = g_values,
    na.rm = FALSE
  )
  G <- dcast(values, id ~ stage, value.var = "P")
  G <- merge(id, G, all.x = TRUE)
  id <- NULL
  G[ , id := NULL]
  G <- as.matrix(G)
  G <- cbind(G, 1)
  ## replace value with 1 for right-censoring/missing events:
  G[D == 0] <- 1
  ## replace value with 1 for terminal events:
  G[E == 1] <- 1

  if (any(is.na(G))) {
    warning("Missing values detected in the action probability")
  }

  colnames(G) <- 1:ncol(G)

  return(G)
}

#' Calculate policy action outcome regression matrix
#'
#' Creates an (n X (K+2)) matrix of Q-values for the given policy actions, handling both
#' terminal stages and right-censoring scenarios.
#'
#' @details
#' The matrix structure is as follows:
#' * Columns 1 to K: Auxiliary Q-function values for stages 1 to K. Missing values are replaced with 0.
#' * Column K+1: Final stage outcome regression values (m_values) or the observed
#' utility outcomes if no right-censoring at stage K+1 occur. Missing values are replaced with 0.
#' * Column K+2: Observed utility outcomes. Missing values are replaced with 0.
#'
#' Right-censored outcome regression values are replaced with 0.
#' Missing values due to terminal events are replaced with the observed utility outcomes.
#'
#' @param q_values data.table containing Q-function values
#' @param m_values data.table containing terminal stage values (or NULL)
#' @param U numeric vector of utility outcomes
#' @param policy_actions data.table containing policy decisions
#' @param id data.table with subject IDs
#' @param D matrix of non-censoring indicators
#' @param action_set vector of possible actions
#'
#' @return matrix of dimension (n X (K+2)) containing:
#'   * Q-values for the provided policy actions (cols 1:K)
#'   * Final stage outcome regression values (col K+1)
#'   * Observed utility outcomes (col K+2)
#' @noRd
policy_action_outcome_matrix <- function(q_values, m_values, U, policy_actions, id, D, action_set,
                                         q_d_values = NULL) {
  n <- nrow(id)
  K <- ncol(D) - 1

  if (!is.null(q_values)) {
    if (is.null(q_d_values)) {
      q_d_values <- get_a_values(
        a = policy_actions[["d"]],
        action_set = action_set,
        values = q_values
      )
    }

    if (!all(complete.cases(q_d_values))) {
      stop("The policy dictates actions with incomplete (NA) Q-function values")
    }

    Q <- dcast(q_d_values, id ~ stage, value.var = "P")
    Q <- merge(id, Q, all.x = TRUE)
    Q[, id := NULL]
    Q <- as.matrix(Q)
  } else {
    Q <- matrix(nrow = n, ncol = K)
  }
  colnames(Q) <- 1:ncol(Q)

  ## add (n X 1) vector with values Q_{K+1}:
  ## Q_{K+1} = U if no right-censoring occur at stage K+1,
  ## i.e., when m_values is NULL
  if (is.null(m_values)) {
    Q <- cbind(Q, U)
  } else {
    M <- dcast(m_values, id ~ stage, value.var = "Q")
    M <- merge(id, M, all.x = TRUE)
    M[, id := NULL]
    Q <- cbind(Q, M)
  }
  ## add (n X 1) vector with values Q_{K+2} = U:
  Q <- cbind(Q, U)

  ## replace NA values with the observed utility outcome (possibly NA):
  Q <- apply(
    Q,
    MARGIN = 2,
    function(v) {
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  ## replace values with 0 for missing outcome regression values
  Q[, -1][D == 0] <- 0

  return(Q)
}

## internal: per-subgroup potential outcome means E[U(a)|d] and their
## influence curves. For each column of `subgroup_indicator` (d) the
## two means E[U(a2)|d] and E[U(a1)|d] are computed from the doubly robust
## scores Z and stored as adjacent coefficients. The
## subgroup average treatment effect is the contrast of these two means. Returns
## `coef` (length 2 * ncol(subgroup_indicator)) and `IC` (n x 2 * ncol).
subgroup_means <- function(Z, subgroup_indicator, min_subgroup_size) {
  res <- lapply(
    seq_len(ncol(subgroup_indicator)),
    function(j) {
      si <- subgroup_indicator[, j]
      ps <- mean(si)
      ## E[U(a2)|subgroup] and E[U(a1)|subgroup]:
      m2 <- mean(Z[si, 2])
      m1 <- mean(Z[si, 1])
      ## influence curves for the two means:
      ic2 <- 1 / ps * si * (Z[, 2] - m2)
      ic1 <- 1 / ps * si * (Z[, 1] - m1)
      if (sum(si) < min_subgroup_size) {
        m2 <- m1 <- as.numeric(NA)
        ic2 <- ic1 <- rep(as.numeric(NA), length(si))
      }
      list(coef = c(m2, m1), IC = cbind(ic2, ic1))
    }
  )
  coef <- unlist(lapply(res, function(x) x[["coef"]]))
  IC <- do.call(what = "cbind", lapply(res, function(x) x[["IC"]]))
  IC <- unname(IC)

  return(list(coef = coef, IC = IC))
}

#' Stage-wise weight factors for policies that depend on the natural action
#'
#' The factor at stage k is g^d_k(A_k | H_k) / g_k(A_k | H_k), where
#' g^d_k(a | H_k) = sum_{a': d_k(a', H_k) = a} g_k(a' | H_k) is the
#' probability of the policy action. The factor is 1 for right-censored,
#' terminal and final (K+1) stages.
#'
#' @param g_values data.table with the action probabilities.
#' @param actions data.table with the observed actions (events 0).
#' @param policy_actions_natural data.table with the policy actions for every
#' hypothetical natural action, see natural_policy_actions().
#' @param id data.table with subject IDs.
#' @param D matrix of non-censoring indicators.
#' @param E matrix of terminal event indicators.
#' @param action_set vector of possible actions.
#' @return matrix with dimensions (n X (K+1)).
#' @noRd
natural_ratio_matrix <- function(g_values, actions, policy_actions_natural, id, D, E, action_set) {
  if (is.null(g_values)) {
    stop("g_models (or g_functions) are required for policies that depend on the natural action.")
  }
  keys <- actions[, c("id", "stage"), with = FALSE]
  g_mat <- as.matrix(
    merge(keys, g_values, by = c("id", "stage"))[, -c("id", "stage"), with = FALSE]
  )
  d_nat <- merge(keys, policy_actions_natural, by = c("id", "stage"))
  A <- actions[["A"]]

  ## g_k(A_k | H_k):
  gA <- g_mat[cbind(seq_along(A), match(A, action_set))]
  ## g^d_k(A_k | H_k):
  gd <- rep(0, length(A))
  for (j in seq_along(action_set)) {
    gj <- g_mat[, j]
    gj[is.na(gj)] <- 0
    follows <- (d_nat[[paste0("d_", action_set[j])]] == A)
    follows[is.na(follows)] <- FALSE
    gd <- gd + gj * follows
  }
  r <- gd / gA
  if (any(is.na(r)) || any(is.infinite(r))) {
    stop("The policy dictates actions with fitted probability 0. Unable to calculate inverse probability weights.")
  }

  R <- dcast(data.table(keys, r = r), id ~ stage, value.var = "r")
  R <- merge(id, R, all.x = TRUE)
  id <- NULL
  R[, id := NULL]
  R <- as.matrix(R)
  R <- cbind(R, 1)
  R[D == 0] <- 1
  R[E == 1] <- 1
  colnames(R) <- 1:ncol(R)

  return(R)
}

#' Outcome regression values for policies that depend on the natural action
#'
#' For observed actions the value is Q_k(H_k, d_k(A_k, H_k)). For right-censored
#' events (before the action) the natural action is missing and the value is the
#' integral sum_a g_k(a | H_k) Q_k(H_k, d_k(a, H_k)). The values Q_k(H_k, A_k) at the
#' observed actions are also returned (0 for right-censored events).
#'
#' @param q_values data.table with the Q-function values (events 0, 2).
#' @param g_values data.table with the action probabilities (events 0, 2).
#' @param actions data.table with the observed actions (events 0).
#' @param policy_actions data.table with the policy actions (NA for events 2).
#' @param policy_actions_natural data.table with the policy actions for every
#' hypothetical natural action.
#' @param action_set vector of possible actions.
#' @return list with data.tables Q_d and Q_A with columns id, stage and P.
#' @noRd
natural_policy_q_values <- function(q_values, g_values, actions, policy_actions,
                                    policy_actions_natural, action_set) {
  keys <- q_values[, c("id", "stage"), with = FALSE]
  d_obs <- merge(keys, policy_actions, by = c("id", "stage"))[["d"]]
  A_obs <- merge(keys, actions, by = c("id", "stage"), all.x = TRUE)[["A"]]
  stopifnot(
    length(d_obs) == nrow(keys),
    length(A_obs) == nrow(keys)
  )
  censored <- is.na(A_obs)
  if (any(is.na(d_obs) & !censored)) {
    stop("The policy returned missing actions for observed natural actions.")
  }

  q_mat <- as.matrix(q_values[, -c("id", "stage"), with = FALSE])
  q_at <- function(a) q_mat[cbind(seq_len(nrow(q_mat)), match(a, action_set))]

  Q_d <- q_at(d_obs)
  Q_A <- q_at(A_obs)

  if (any(censored)) {
    if (is.null(g_values)) {
      mes <- paste0(
        "g_models (or g_functions) are required for policies that depend on the ",
        "natural action when right-censoring occurs before the action."
      )
      stop(mes)
    }
    g_mat <- as.matrix(
      merge(keys, g_values, by = c("id", "stage"))[, -c("id", "stage"), with = FALSE]
    )
    d_nat <- merge(keys, policy_actions_natural, by = c("id", "stage"))
    integral <- rep(0, nrow(keys))
    for (j in seq_along(action_set)) {
      gj <- g_mat[, j]
      gj[is.na(gj)] <- 0
      qj <- q_at(d_nat[[paste0("d_", action_set[j])]])
      integral <- integral + ifelse(gj == 0, 0, gj * qj)
    }
    Q_d[censored] <- integral[censored]
  }
  Q_A[censored] <- 0

  if (anyNA(Q_d) || anyNA(Q_A)) {
    stop("The policy dictates actions with incomplete (NA) Q-function values")
  }

  out <- list(
    Q_d = data.table(keys, P = Q_d),
    Q_A = data.table(keys, P = Q_A)
  )
  return(out)
}

dr_subgroup <- function(K,
                        id,
                        action_set,
                        actions,
                        policy_actions,
                        policy_actions_natural = NULL,
                        policy_name,
                        policy_input_meta,
                        policy_output_meta = NULL,
                        events,
                        g_values,
                        q_values,
                        c_values,
                        m_values,
                        utility,
                        min_subgroup_size) {
  if (!is.null(policy_actions_natural)) {
    stop("target = 'subgroup' is not implemented for policies that depend on the natural action.")
  }
  if (K != 1) {
    mes <- paste0(
      "subgroup average treatment effect evaluation",
      " is not implemeted for multiple stages."
    )
    stop(mes)
  }
  if (length(action_set) != 2) {
    mes <- paste0(
      "subgroup average treatment effect evaluation is not ",
      "implemented for more than two actions."
    )
    stop(mes)
  }
  stage <- event <- NULL
  if (nrow(events[stage == 1 & event == 1, ]) > 0) {
    mes <- paste0(
      "subgroup average treatment effect evaluation is not ",
      "implemented for terminal events at stage 1."
    )
    stop(mes)
  }
  rm(stage, event)

  ## calculating the doubly robust score for each treatment

  ## (n X K+1) matrix with missing event indicators
  M <- event_matrix(events = events, event_set = c(NA))

  ## (n X K+1) matrix with terminal event indicators
  E <- event_matrix(events = events, event_set = c(1))

  ## (n X K+1) matrix with entries Delta_k:
  D <- event_matrix(events = events, event_set = c(0, 1))

  ## getting the utility vector U:
  U <- utility[["U"]]

  ## (n) vector indicating right-censored outcomes:
  censored_outcomes <- is.na(U)

  ## replace NA with 0 whenever D[,2] == 0:
  U[D[,2] == 0] <- 0

  ## (n X K+1) matrix with entries C_k(H_k):
  C <- cens_prob_matrix(c_values = c_values,
                        M = M,
                        n = nrow(id),
                        K = K,
                        events = events,
                        E = E)

  ## (n X action_set) matrix with entries  I(A = a)
  II <- matrix(nrow = nrow(id), ncol = length(action_set))
  wide_actions <- dcast(actions, id ~ stage, value.var = "A")
  wide_actions <- merge(id, wide_actions, all.x = TRUE)
  IIA <- wide_actions[, -c("id"), with = FALSE]
  IIA <- as.matrix(IIA)
  for (j in seq_along(action_set)) {
    II[, j] <- (IIA == action_set[j])
  }
  rm(IIA)
  ## replace values with 0 whenever D[,1] == 0:
  II[D[,1] == 0,] <- 0

  ## (n X action_set) matrix with entries  g(a)
  g_values <- merge(policy_actions, g_values, all.x = TRUE)
  set(g_values, j = "d", value = NULL)
  G <- as.matrix(g_values[, -c("id", "stage"), with = FALSE])
  ## replace values with 1 whenever  D[,1] == 0:
  G[ D[,1] == 0,] <- 1

  ## (n X #actions) matrix with entries  Q(a)
  Q <- as.matrix(q_values[, -c("id", "stage"), with = FALSE])

  ## (n X 1) vector with values Q_{K+1}
  ## Q_{K+1} = U if no right-censoring occur at stage K+1,
  ## i.e., m_values is NULL:
  if (is.null(m_values)) {
    M <- U
  } else {
    M <- dcast(m_values, id ~ stage, value.var = "Q")
    M <- merge(id, M, all.x = TRUE)
    M <- M[, -c("id"), with = FALSE]
    M <- as.vector(as.matrix(M))
  }
  ## replace values with 0 whenever  D[,1] == 0:
  M[D[,1] == 0] <- 0

  # calculating the doubly robust (uncentralized) score for each treatment:
  Z <- Q +
    D[,1] / C[,1] * II / G * apply(Q, 2, function(q) M - q) +
    D[,1] / C[,1] * II / G * D[,2] / C[,2] * (U - M)
  colnames(Z) <- paste("Z_", action_set, sep = "")

  ## checks:
  stopifnot(
    !all(is.na(Z))
  )

  ## calculating the subgroup indicator for each treatment:
  subgroup_indicator <- (policy_actions[["d"]] == action_set[2])
  subgroup_indicator <- cbind(subgroup_indicator, !subgroup_indicator)
  subgroup_indicator <- unname(subgroup_indicator)

  ## calculating the per-subgroup potential outcome means E[U(a)|d] and their
  ## influence curves.
  sm <- subgroup_means(
    Z = Z,
    subgroup_indicator = subgroup_indicator,
    min_subgroup_size = min_subgroup_size
  )

  ## naming:
  as <- action_set
  ## the coefficients are the 4 per-subgroup potential outcome means per
  ## policy: [E[U(a2)|d=1], E[U(a1)|d=1], E[U(a2)|d=0], E[U(a1)|d=0]]:
  name <- c(
    paste0("E[U(", as[2], ")|d=", 1, "]"),
    paste0("E[U(", as[1], ")|d=", 1, "]"),
    paste0("E[U(", as[2], ")|d=", 0, "]"),
    paste0("E[U(", as[1], ")|d=", 0, "]")
  )
  ## the two subgroup average treatment effects (reported by default):
  contrast_name <- c(
    paste0("E[U(", as[2], ")-U(", as[1], ")|d=", 1, "]"),
    paste0("E[U(", as[2], ")-U(", as[1], ")|d=", 0, "]")
  )
  ## appending policy names:
  if (!is.null(policy_name)){
    name <- paste0(name, ": d=", policy_name)
    contrast_name <- paste0(contrast_name, ": d=", policy_name)
  }

  ## collecting meta information
  input_meta <- data.table(
    action = c(as[2], as[1], as[2], as[1]),
    subgroup = c(1, 1, 0, 0)
  )
  input_meta <- cbind(input_meta,
                      as.data.table(as.list(policy_input_meta)))
  output_meta <- NULL
  if (!is.null(policy_output_meta)) {
    output_meta <- as.data.table(as.list(policy_output_meta))
  }

  out <- list(
    coef = sm[["coef"]],
    IC = sm[["IC"]],
    Z = Z,
    subgroup_indicator = subgroup_indicator,
    name = name,
    contrast_name = contrast_name,
    input_meta = input_meta,
    output_meta = output_meta
  )

  return(out)
}

## doubly robust score for the policy value.
dr_value <- function(K,
                     id,
                     action_set,
                     actions,
                     policy_actions,
                     policy_actions_natural = NULL,
                     policy_name,
                     policy_input_meta,
                     policy_output_meta = NULL,
                     events,
                     g_values,
                     q_values,
                     c_values,
                     m_values,
                     utility,
                     ...) {
  ## (n) observed utility vector:
  U <- utility[["U"]]

  ## (n) vector indicating right-censored outcomes:
  censored_outcomes <- is.na(U)

  ## (n X K+1) matrix with missing event indicators
  M <- event_matrix(events = events, event_set = c(NA))

  ## (n X K+1) matrix with terminal event indicators
  E <- event_matrix(events = events, event_set = c(1))

  ## (n X K+1) matrix with entries Delta_k (non-censoring/non-missing indicators):
  D <- event_matrix(events = events, event_set = c(0, 1))

  ## (n X K+1) matrix with entries C_k(H_k) (non-censoring probabilities):
  C <- cens_prob_matrix(c_values = c_values,
                        M = M,
                        n = nrow(id),
                        K = K,
                        events = events,
                        E = E)

  if (is.null(policy_actions_natural)) {
    ## (n X K+1) matrix with entries I(d_k(H_k) = A_k)
    ## I(d_{K+1}(H_{K+1}) = A_{K+1}) = 1:
    II <- policy_action_indicator_matrix(actions = actions,
                                         policy_actions = policy_actions,
                                         D = D,
                                         E = E,
                                         id = id)

    ## (n X K+1) matrix with entries g_k(A_k, H_k) (action probabilities):
    ## g_{K+1} = 1
    G <- action_prob_matrix(g_values = g_values,
                            actions = actions,
                            id = id,
                            D = D,
                            E = E,
                            action_set = action_set)

    ## (n X (K+2)) matrix with columns Q_k(H_{k,i}, d_k(H_{k,i}))
    ## (outcome regression values):
    Q <- policy_action_outcome_matrix(q_values = q_values,
                                      m_values = m_values,
                                      U = U,
                                      policy_actions = policy_actions,
                                      id = id,
                                      D = D,
                                      action_set)
  } else {
    ## policy that depends on the natural action:
    ## (n X K+1) matrix with entries g^d_k(A_k | H_k) / g_k(A_k | H_k):
    R <- natural_ratio_matrix(g_values = g_values,
                              actions = actions,
                              policy_actions_natural = policy_actions_natural,
                              id = id,
                              D = D,
                              E = E,
                              action_set = action_set)

    q_d_values <- NULL
    q_A_values <- NULL
    if (!is.null(q_values)) {
      nq <- natural_policy_q_values(q_values = q_values,
                                    g_values = g_values,
                                    actions = actions,
                                    policy_actions = policy_actions,
                                    policy_actions_natural = policy_actions_natural,
                                    action_set = action_set)
      q_d_values <- nq[["Q_d"]]
      q_A_values <- nq[["Q_A"]]
    }
    ## (n X (K+2)) matrix with columns Q_k(H_k, d_k(A_k, H_k)) for observed
    ## actions and sum_a g_k(a | H_k) Q_k(H_k, d_k(a, H_k)) for
    ## right-censored events (before the action):
    Q <- policy_action_outcome_matrix(q_values = q_values,
                                      m_values = m_values,
                                      U = U,
                                      policy_actions = policy_actions,
                                      id = id,
                                      D = D,
                                      action_set = action_set,
                                      q_d_values = q_d_values)
    ## (n X (K+2)) matrix with columns Q_k(H_k, A_k):
    Q_A <- policy_action_outcome_matrix(q_values = q_values,
                                        m_values = m_values,
                                        U = U,
                                        policy_actions = policy_actions,
                                        id = id,
                                        D = D,
                                        action_set = action_set,
                                        q_d_values = q_A_values)
  }

  ## calculating the doubly robust score:
  Zd <- Q[, 1]
  for (k in 1:(K+1)) {
    if (is.null(policy_actions_natural)) {
      Zd <- Zd + ipw_weight(D[, 1:k], C[, 1:k]) * ipw_weight(II[, 1:k], G[, 1:k]) * (Q[, k + 1] - Q[, k])
    } else {
      Zd <- Zd + ipw_weight(D[, 1:k], C[, 1:k]) * colprod(R[, 1:k]) * (Q[, k + 1] - Q_A[, k])
    }
  }

  ## calculating the IPW and OR scores:
  D_ipw <- matrix(1, nrow = nrow(D), ncol = ncol(D))
  if (is.null(policy_actions_natural)) {
    Zd_ipw <- ipw_weight(D_ipw[, 1:(K + 1)], C[, 1:(K + 1)]) *
      ipw_weight(II, G = G) * ifelse(is.na(U), 0, U)
  } else {
    Zd_ipw <- ipw_weight(D_ipw[, 1:(K + 1)], C[, 1:(K + 1)]) *
      colprod(R) * ifelse(is.na(U), 0, U)
  }
  Zd_or <- Q[, 1]

  ## naming:
  name <- "E[U(d)]"

  ## appending policy names:
  if (!is.null(policy_name)){
    name <- paste0(name, ": d=", policy_name)
  }

  ## collecting meta information
  input_meta <- as.data.table(as.list(policy_input_meta))
  output_meta <- NULL
  if (!is.null(policy_output_meta)) {
    output_meta <- as.data.table(as.list(policy_output_meta))
  }

  ## output checks:
  stopifnot(
    all(!is.na(Zd)),
    all(!is.na(Zd_ipw)),
    all(!is.na(Zd_or))
  )

  out <- list(
    coef = mean(Zd),
    IC = Zd - mean(Zd),
    coef_ipw = mean(Zd_ipw),
    coef_or = mean(Zd_or),
    name = name,
    input_meta = input_meta,
    output_meta = output_meta
  )

  return(out)
}
