value <- function(target = "value",
                  type,
                  policy_data,
                  policy, g_functions,
                  q_functions) {
  args <- list(
    policy_data = policy_data,
    policy = policy,
    g_functions = g_functions,
    q_functions = q_functions
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
  } else if (target == "sub_effect") {
    if (type == "dr") {
      out <- do.call(what = "dr_sub_effect", args)
    } else {
      stop()
    }
  }

  return(out)
}

dr_sub_effect <- function(policy_data,
                          policy,
                          g_functions,
                          q_functions) {
  # getting the number of stages:
  K <- get_K(policy_data)
  if (K != 1) {
    stop("subgroup effect evaluation is not implemeted for multiple stages.")
  }

  # getting the action set and stage action set:
  action_set <- get_action_set(policy_data)
  if (length(action_set) != 2) {
    mes <- paste0(
      "subgroup effect evaluation is not ",
      "implemented for more than two actions."
    )
    stop(mes)
  }
  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # checking that the policy actions comply with the stage action sets:
  check_actions(
    actions = policy_actions,
    policy_data = policy_data
  )

  # getting the g-function values:
  g_values <- predict(g_functions, policy_data)

  # getting the Q-function values:
  q_values <- predict(q_functions, policy_data)

  ## calculating the doubly robust score for each treatment:

  ## (n X action_set) matrix with entries  I(A = a)
  II <- matrix(nrow = get_n(policy_data), ncol = length(action_set))
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
  U <- get_utility(policy_data)$U

  # calculating the doubly robust (uncentralized) score for each treatment:
  Z <- Q + II / G * apply(Q, 2, function(q) U - q)
  colnames(Z) <- paste("Z_", action_set, sep = "")

  ## checks:
  stopifnot(
    !all(is.na(Z))
  )

  ## calculating the doubly robust blip score:
  blip <- Z[, 2] - Z[, 1]

  ## calculating the subgroup indicator:
  subgroup_indicator <- (policy_actions[["d"]] == action_set[2])

  ## calculating the subgroup blip estimate:
  subgroup_blip_estimate <- mean(blip[subgroup_indicator])

  ## calculating the influence curve for the subgroup blip estimate:
  IC <- 1 / mean(subgroup_indicator) * subgroup_indicator * (blip - subgroup_blip_estimate)

  out <- list(
    value_estimate = subgroup_blip_estimate,
    IC = IC,
    Z = Z,
    subgroup_indicator = subgroup_indicator,
    id = get_id(policy_data),
    policy_actions = policy_actions,
    g_values = g_values,
    q_values = q_values
  )

  return(out)
}


dr_value <- function(policy_data,
                     policy,
                     g_functions,
                     q_functions) {
  # getting the number of stages:
  K <- get_K(policy_data)

  # getting the action set and stage action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # checking that the policy actions comply with the stage action sets:
  check_actions(
    actions = policy_actions,
    policy_data = policy_data
  )

  # getting the g-function values:
  g_values <- predict(g_functions, policy_data)

  # getting the Q-function values:
  q_values <- predict(q_functions, policy_data)

  ### calculating the doubly robust score:

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

  # (n) vector with entries U_i:
  U <- get_utility(policy_data)$U

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

  # calculating the IPW and OR scores
  Zd_ipw <- ipw_weight(II, G = G) * U
  Zd_or <- Q[, 1]

  # output checks:
  stopifnot(
    !all(is.na(Zd)),
    !all(is.na(Zd_ipw)),
    !all(is.na(Zd_or))
  )

  out <- list(
    value_estimate = mean(Zd),
    IC = Zd - mean(Zd),
    value_estimate_ipw = mean(Zd_ipw),
    value_estimate_or = mean(Zd_or),
    id = get_id(policy_data),
    policy_actions = policy_actions,
    g_values = g_values,
    q_values = q_values
  )

  return(out)
}

or_value <- function(policy_data,
                     policy,
                     q_functions,
                     ...) {
  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # checking that the policy actions comply with the stage action sets:
  check_actions(
    actions = policy_actions,
    policy_data = policy_data
  )

  # getting the Q-function values:
  q_values <- predict(q_functions, policy_data)

  ### calculating the score:

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

  # output checks:
  stopifnot(
    all(!is.na(Zd_or))
  )

  out <- list(
    value_estimate = mean(Zd_or),
    IC = NULL,
    id = get_id(policy_data),
    policy_actions = policy_actions,
    q_values = q_values
  )
  return(out)
}

ipw_value <- function(policy_data,
                      policy,
                      g_functions,
                      ...) {
  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # checking that the policy actions comply with the stage action sets:
  check_actions(
    actions = policy_actions,
    policy_data = policy_data
  )

  # getting the g-function values:
  g_values <- predict(g_functions, policy_data)

  ### calculating the score:

  # (n) vector with entries U_i:
  U <- get_utility(policy_data)$U

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

  # output checks:
  stopifnot(
    !all(is.na(Zd_ipw))
  )

  out <- list(
    value_estimate = mean(Zd_ipw),
    IC = Zd_ipw - mean(Zd_ipw),
    id = get_id(policy_data),
    policy_actions = policy_actions,
    g_values = g_values
  )
  return(out)
}
