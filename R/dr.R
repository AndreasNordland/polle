#' @export
dr <- function(object, g_model, g_function, Q_model, Q_function, policy, g_full_history, Q_full_history, policy_full_history)
  UseMethod("dr")

#' @export
dr.policy_data <- function(object, g_model = NULL, g_function = NULL, Q_model = NULL, Q_function = NULL, policy, g_full_history = FALSE, Q_full_history = FALSE, policy_full_history = FALSE){
  stopifnot(
    !(is.null(g_model) & is.null(g_function)),
    !(!is.null(g_function) & !is.null(g_model)),
    !(is.null(Q_model) & is.null(Q_function)),
    !(!is.null(Q_function) & !is.null(Q_model))
  )

  K <- object$dim$K
  action_set <- object$action_set

  actions <- get_actions(object)
  policy_actions <- get_policy_actions(object, policy = policy, policy_full_history = policy_full_history)
  policy_actions[actions, A := i.A]
  policy_actions[, dA := d == A]

  if (!is.null(g_model)){
    g_function <- fit_g_model(object, g_model, g_full_history = g_full_history)
  }
  g_function_predictions <- get_function_predictions(object, fun = g_function, full_history = g_full_history)
  g_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, g_function_predictions)

  if (!is.null(Q_model)){
    Q_function <- fit_Q_model(object, policy_actions = policy_actions, Q_model = Q_model, Q_full_history = Q_full_history)
  }
  Q_function_predictions <- get_function_predictions(object, fun = Q_function, full_history = Q_full_history)
  Q_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, predictions = Q_function_predictions)

  # D: (n X K) matrix
  D <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "dA")[, -c("id"), with = FALSE])
  # G: (n X K) matrix
  G <- as.matrix(dcast(g_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  # U: (n) vector
  U <- utility(object)$U
  # V: (n X K+1) matrix
  V <- as.matrix(dcast(Q_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
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
    Q_function = Q_function
  )

  return(out)
}
