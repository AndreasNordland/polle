#' @export
dr <- function(object, g_model, g_function, q_model, q_function, policy, g_full_history, q_full_history)
  UseMethod("dr")

#' @export
dr.policy_data <- function(object, g_model = NULL, g_function = NULL, q_model = NULL, q_function = NULL, policy, g_full_history = FALSE, q_full_history = FALSE){
  stopifnot(
    !(is.null(g_model) & is.null(g_function)),
    !(!is.null(g_function) & !is.null(g_model)),
    !(is.null(q_model) & is.null(q_function)),
    !(!is.null(q_function) & !is.null(q_model))
  )

  K <- object$dim$K
  action_set <- object$action_set

  actions <- get_actions(object)
  policy_actions <- policy(policy_data = object)
  policy_actions[actions, A := i.A]
  policy_actions[, dA := d == A]

  # g-function
  if (!is.null(g_model)){
    g_function <- fit_g_model(object, g_model, g_full_history = g_full_history)
  }
  g_function_predictions <- get_function_predictions(object, fun = g_function, full_history = g_full_history)
  g_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, g_function_predictions)

  # Q-function
  if (!is.null(q_model)){
    q_function <- fit_Q_model(object, policy_actions = policy_actions, q_model = q_model, q_full_history = q_full_history)
  }
  q_function_predictions <- get_function_predictions(object, fun = q_function, full_history = q_full_history)
  q_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, predictions = q_function_predictions)

  # D: (n X K) matrix with entries A_{k,i} == d_k(H_{k,i})
  D <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "dA")[, -c("id"), with = FALSE])
  # G: (n X K) matrix with entries P(A_{k,i} = d_k(H_{k,i}) | H_{k,i})
  G <- as.matrix(dcast(g_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  # U: (n) vector with entries U_i
  U <- utility(object)$U
  # V: (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i}))
  V <- as.matrix(dcast(q_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  V <- apply(
    V,
    MARGIN = 2,
    function(v){
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  V <- cbind(V, U)

  phi <- V[, 1]
  for (k in 1:K){
    phi <- phi + ipw_weight(D = D[,1:k], G = G[,1:k]) * (V[,k+1] - V[,k])
  }

  phi_ipw <- ipw_weight(D = D, G = G) * U
  phi_or <- V[, 1]

  out <- list(
    value = mean(phi),
    phi = phi,
    phi_ipw = phi_ipw,
    phi_or = phi_or,
    g_function = g_function,
    q_function = q_function
  )

  return(out)
}
