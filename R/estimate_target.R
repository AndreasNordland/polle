estimate_target <- function(target = "value",
                            type,
                            K,
                            id,
                            action_set,
                            actions,
                            policy_actions,
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

  if (target == "value") {
    if (type == "dr") {
      out <- do.call(what = "dr_value", args)
    } else if (type == "ipw") {
      out <- do.call(what = "ipw_value", args)
    } else if (type == "or") {
      out <- do.call(what = "or_value", args)
    } else {
      stop()
    }
  } else if (target == "subgroup") {
    if (type == "dr") {
      out <- do.call(what = "dr_subgroup", args)
    } else {
      stop("target = 'subgroup' only implemented for type = 'dr'.")
    }
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
#'
#' @return matrix of censoring probabilities. Dimensions (n X (K+1)).
#' @noRd
cens_prob_matrix <- function(c_values, M, n, K) {
  if (!is.null(c_values)){
    if (!data.table::is.data.table(c_values)) {
      stop("c_values must be a data.table")
    }
    required_cols <- c("id", "stage", "surv_time", "surv_time2")
    if (!all(required_cols %in% names(c_values))) {
      stop("c_values must contain columns: ",
           paste(required_cols, collapse = ", "))
    }

    surv_time <- dcast(c_values, id ~ stage,
                       value.var = "surv_time")[, -c("id"), with = FALSE]
    surv_time2 <- dcast(c_values, id ~ stage,
                        value.var = "surv_time2")[, -c("id"), with = FALSE]
    C <- as.matrix(surv_time2 / surv_time)
    ## set probabilities to 1 for missing events:
    C[M == 1] <- 1

    ## check for invalid values:
    if (any(is.na(C)) || any(is.infinite(C))) {
      warning("NA or infinite values detected in probability matrix")
    }

  } else {
    C <- matrix(nrow = n, ncol = K+1, 1)
  }

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
policy_action_outcome_matrix <- function(q_values, m_values, U, policy_actions, id, D, action_set) {
  n <- nrow(id)
  K <- ncol(D) - 1

  if (!is.null(q_values)) {
    q_d_values <- get_a_values(
      a = policy_actions[["d"]],
      action_set = action_set,
      values = q_values
    )

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

dr_subgroup <- function(K,
                        id,
                        action_set,
                        actions,
                        policy_actions,
                        events,
                        g_values,
                        q_values,
                        c_values,
                        m_values,
                        utility,
                        min_subgroup_size) {
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

  ##
  ## calculating the doubly robust score for each treatment
  ##

  ## (n X K+1) matrix with missing event indicators
  M <- event_matrix(events = events, event_set = c(NA))

  ## (n X K+1) matrix with terminal event indicators
  E <- event_matrix(events = events, event_set = c(1))

  ## (n X K+1) matrix with entries Delta_k:
  D <- event_matrix(events = events, event_set = c(0,1))

  ## getting the utility vector U:
  U <- utility[["U"]]

  ## (n) vector indicating right-censored outcomes:
  censored_outcomes <- is.na(U)

  ## replace NA with 0 whenever D[,2] == 0:
  U[D[,2] == 0] <- 0

  ## (n X K+1) matrix with entries C_k(H_k):
  C <- cens_prob_matrix(c_values = c_values, M = M, n = nrow(id), K = K)

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

  ## calculating the doubly robust blip score:
  blip <- Z[, 2] - Z[, 1]

  ## calculating the subgroup indicator for each treatment:
  subgroup_indicator <- (policy_actions[["d"]] == action_set[2])
  subgroup_indicator <- cbind(subgroup_indicator, !subgroup_indicator)
  subgroup_indicator <- unname(subgroup_indicator)

  res <- apply(
    subgroup_indicator,
    MARGIN = 2,
    FUN = function(si){
      ## calculating the subgroup average treatment effect:
      sate <- mean(blip[si])
      ## calculating the influence curve for the subgroup average treatment effect:
      IC <- 1 / mean(si) * si *
        (blip - sate)

      ss <- sum(si)
      if (ss < min_subgroup_size) {
        sate <- as.numeric(NA)
        IC <- rep(as.numeric(NA), length(si))
      } 

      out <- list(sate = sate, IC = IC)
    },
    simplify = FALSE
  )
  sate <- lapply(res, function(x) get_element(x, "sate")) |> unlist()
  IC <- lapply(res, function(x) get_element(x, "IC"))
  IC <- do.call(what = "cbind", IC)

  out <- list(
    coef = sate,
    IC = IC,
    Z = Z,
    subgroup_indicator = subgroup_indicator
  )

  return(out)
}

## doubly robust score for the policy value.
dr_value <- function(K,
                     id,
                     action_set,
                     actions,
                     policy_actions,
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
  D <- event_matrix(events = events, event_set = c(0,1))

  ## test: REMOVE
  test <- dcast(events, id ~ stage, value.var = "event")
  test <- test[ , -c("id"), with = FALSE]
  test <- (test != 2) * 1.0
  ## fill NA values with 0:
  ## (occur under right-censoring or a stochastic number of stages)
  test <- apply(
    test,
    MARGIN = 2,
    function(v) {
      v[is.na(v)] <- 0
      return(v)
    }
  )
  stopifnot(all(D == test))
  rm(test)

  ## (n X K+1) matrix with entries C_k(H_k) (non-censoring probabilities):
  C <- cens_prob_matrix(c_values = c_values, M = M, n = nrow(id), K = K)

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

  ## (n X (K+2)) matrix with columns Q_k(H_{k,i}, d_k(H_{k,i})) (outcome regression values):
  Q <- policy_action_outcome_matrix(q_values = q_values,
                                    m_values = m_values,
                                    U = U,
                                    policy_actions = policy_actions,
                                    id = id,
                                    D = D,
                                    action_set)

  ## calculating the doubly robust score:
  Zd <- Q[, 1]
  for (k in 1:(K+1)) {
    Zd <- Zd + ipw_weight(D[, 1:k], C[, 1:k]) * ipw_weight(II[, 1:k], G[, 1:k]) * (Q[, k + 1] - Q[, k])
  }

  ## calculating the IPW and OR scores:
  Zd_ipw <- ipw_weight(D[, 1:k], C[, 1:k]) * ipw_weight(II, G = G) * ifelse(is.na(U), 0, U)
  Zd_or <- Q[, 1]

  ##
  ## output checks
  ##

  stopifnot(
    all(!is.na(Zd)),
    all(!is.na(Zd_ipw)),
    all(!is.na(Zd_or))
  )

  out <- list(
    coef = mean(Zd),
    IC = Zd - mean(Zd),
    coef_ipw = mean(Zd_ipw),
    coef_or = mean(Zd_or)
  )

  return(out)
}

or_value <- function(K,
                     action_set,
                     actions,
                     policy_actions,
                     q_values,
                     ...) {

  ##
  ## calculating the outcome regression score
  ##

  # (n X K) matrix with entries Q_k(d_k(H_k), H_k)
  q_d_values <- get_a_values(
    a = policy_actions$d,
    action_set = action_set,
    values = q_values
  )
  if (!all(complete.cases(q_d_values))) {
    stop("The policy dictates actions with incomplete (NA) Q-function values")
  }
  Q <- dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE]
  Q <- as.matrix(Q)

  Zd_or <- Q[, 1]

  ##
  ## output checks
  ##

  stopifnot(
    all(!is.na(Zd_or))
  )

  out <- list(
    coef = mean(Zd_or),
    IC = NULL
  )
  return(out)
}

ipw_value <- function(K,
                      action_set,
                      actions,
                      policy_actions,
                      g_values,
                      utility,
                      ...) {
  ##
  ## calculating the inverse probability score
  ##

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- as.matrix(dcast(
    actions, id ~ stage,
    value.var = "A"
  )[, -c("id"), with = FALSE])
  IId <- as.matrix(dcast(
    policy_actions, id ~ stage,
    value.var = "d"
  )[, -c("id"), with = FALSE])
  II <- (IIA == IId)
  rm(IIA, IId)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  g_d_values <- get_a_values(
    a = policy_actions$d,
    action_set = action_set,
    g_values
  )
  G <- as.matrix(dcast(
    g_d_values,
    id ~ stage,
    value.var = "P"
  )[, -c("id"), with = FALSE])

  Zd_ipw <- ipw_weight(II, G = G) * U

  ##
  ## output checks
  ##

  stopifnot(
    !all(is.na(Zd_ipw))
  )

  out <- list(
    coef = mean(Zd_ipw),
    IC = Zd_ipw - mean(Zd_ipw)
  )
  return(out)
}
