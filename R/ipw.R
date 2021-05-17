#' @export
ipw <- function(object, g_model, g_function, policy, g_full_history, policy_full_history)
  UseMethod("ipw")

#' @export
ipw.policy_data <- function(object, g_model = NULL, g_function = NULL, policy, g_full_history = FALSE, policy_full_history = FALSE){
  stopifnot(
    !(is.null(g_model) & is.null(g_function)),
    !(!is.null(g_function) & !is.null(g_model))
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

  D <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "dA")[, -c("id"), with = FALSE])
  G <- as.matrix(dcast(g_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  U <- utility(object)$U

  phi <- ipw_weight(D = D, G = G) * U

  out <- list(
    value = mean(phi),
    phi = phi,
    g_function = g_function
  )

  return(out)
}
