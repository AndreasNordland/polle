value <- function(type, policy_data, policy, g_functions, q_functions){
  args <- list(
    policy_data = policy_data,
    policy = policy,
    g_functions = g_functions,
    q_functions = q_functions
  )

  if (type == "dr"){
    out <- do.call(what = "dr_value", args)
  } else if (type == "ipw"){
    out <- do.call(what = "ipw_value", args)
  } else if (type == "or") {
    out <- do.call(what = "or_value", args)
  } else{
    stop()
  }

  return(out)
}

dr_value <- function(policy_data,
                     policy,
                     g_functions,
                     q_functions){
  # getting the number of stages:
  K <- get_K(policy_data)

  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # getting the g-function values:
  g_values <- evaluate(g_functions, policy_data)

  # getting the Q-function values:
  q_values <- evaluate(q_functions, policy_data = policy_data)

  ### calculating the doubly robust score:

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  IId <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  II <- (IIA == IId); rm(IIA, IId)
  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = g_values)
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  # (n) vector with entries U_i:
  U <- utility(policy_data)$U
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)
  Q <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  Q <- apply(
    Q,
    MARGIN = 2,
    function(v){
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  Q <- cbind(Q, U)

  # calculating the doubly robust score
  Zd <- Q[, 1]
  for (k in 1:K){
    Zd <- Zd + ipw_weight(II[,1:k], G = G[,1:k]) * (Q[,k+1] - Q[,k])
  }

  # calculating the IPW and OR scores
  Zd_ipw <- ipw_weight(II, G = G) * U
  Zd_or <- Q[, 1]

  out <- list(
    value_estimate = mean(Zd),
    iid=Zd-mean(Zd),
    value_estimate_ipw = mean(Zd_ipw),
    value_estimate_or = mean(Zd_or),
    id = get_id(policy_data)
  )

  return(out)
}

or_value <- function(policy_data,
                     policy,
                     q_functions,
                     ...){

  # getting the number of stages:
  K <- get_K(policy_data)

  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # getting the Q-function values:
  q_values <- evaluate(q_functions, policy_data = policy_data)

  ### calculating the score:

  # (n X K) matrix with entries Q_k(d_k(H_k), H_k)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)
  Q <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  Zd_or <- Q[, 1]

  out <- list(
    value_estimate = mean(Zd_or),
    iid = Zd_or-mean(Zd_or),
    id = get_id(policy_data)
  )
  return(out)
}

ipw_value <- function(policy_data,
                      policy,
                      g_functions,
                      ...){

  # getting the number of stages:
  K <- get_K(policy_data)

  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # getting the g-function values:
  g_values <- evaluate(g_functions, policy_data)

  ### calculating the score:

  # (n) vector with entries U_i:
  U <- utility(policy_data)$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  IId <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  II <- (IIA == IId); rm(IIA, IId)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, g_values)
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  Zd_ipw <- ipw_weight(II, G = G) * U

  out <- list(
    value_estimate = mean(Zd_ipw),
    iid = Zd_ipw-mean(Zd_ipw),
    id = get_id(policy_data)
  )
  return(out)
}
