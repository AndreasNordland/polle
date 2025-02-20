estimate_target <- function(target = "value",
                            type,
                            K,
                            action_set,
                            actions,
                            policy_actions,
                            g_values,
                            q_values,
                            utility,
                            min_subgroup_size) {
  args <- list(
    K = K,
    action_set = action_set,
    actions = actions,
    policy_actions = policy_actions,
    g_values = g_values,
    q_values = q_values,
    utility = utility,
    min_subgroup_size = min_subgroup_size
  )

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

dr_subgroup <- function(K,
                        action_set,
                        actions,
                        policy_actions,
                        g_values,
                        q_values,
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

  ## (n X action_set) matrix with entries  I(A = a)
  II <- matrix(nrow = nrow(actions), ncol = length(action_set))
  for (j in seq_along(action_set)) {
    IIA <- actions[["A"]]
    II[, j] <- (IIA == action_set[j])
  }
  rm(IIA)

  ## (n X action_set) matrix with entries  g(a)
  G <- as.matrix(g_values[, -c("id", "stage"), with = FALSE])

  ## (n X #actions) matrix with entries  Q(a)
  Q <- as.matrix(q_values[, -c("id", "stage"), with = FALSE])

  # (n) vector with entries U:
  U <- utility$U

  # calculating the doubly robust (uncentralized) score for each treatment:
  Z <- Q + II / G * apply(Q, 2, function(q) U - q)
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

dr_value <- function(K,
                     action_set,
                     actions,
                     policy_actions,
                     g_values,
                     q_values,
                     utility,
                     ...) {
  ##
  ## calculating the doubly robust score for the policy value
  ##

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE]
  IIA <- as.matrix(IIA)
  IId <- dcast(
    policy_actions, id ~ stage,
    value.var = "d"
  )[, -c("id"), with = FALSE]
  IId <- as.matrix(IId)
  II <- (IIA == IId)
  rm(IIA, IId)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  g_d_values <- get_a_values(
    a = policy_actions$d,
    action_set = action_set,
    values = g_values
  )
  G <- dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE]
  G <- as.matrix(G)

  # (n) vector with entries U:
  U <- utility$U

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
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
  Q <- apply(
    Q,
    MARGIN = 2,
    function(v) {
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  Q <- cbind(Q, U)

  # calculating the doubly robust score
  Zd <- Q[, 1]
  for (k in 1:K) {
    Zd <- Zd + ipw_weight(II[, 1:k], G = G[, 1:k]) * (Q[, k + 1] - Q[, k])
  }

  ##
  ## calculating the IPW and OR scores
  ##

  Zd_ipw <- ipw_weight(II, G = G) * U
  Zd_or <- Q[, 1]

  ##
  ## output checks
  ##

  stopifnot(
    !all(is.na(Zd)),
    !all(is.na(Zd_ipw)),
    !all(is.na(Zd_or))
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
