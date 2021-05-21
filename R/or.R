#' @export
or <- function(object, q_model, q_function, policy, q_full_history)
  UseMethod("or")

#' @export
or.policy_data <- function(object, q_model = NULL, q_function = NULL, policy, q_full_history = FALSE){
  stopifnot(
    !(is.null(q_model) & is.null(q_function)),
    !(!is.null(q_function) & !is.null(q_model))
  )

  K <- object$dim$K
  action_set <- object$action_set

  policy_actions <- get_policy_actions(policy, policy_data = object)

  if (!is.null(q_model)){
    q_function <- fit_Q_model(object, policy_actions = policy_actions, q_model = q_model, q_full_history = q_full_history)
  }
  q_function_predictions <- get_function_predictions(object, fun = q_function, full_history = q_full_history)
  q_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, predictions = q_function_predictions)

  # V: (n X K) matrix
  V <- as.matrix(dcast(q_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi <- V[, 1]
  out <- list(
    value = mean(phi),
    phi = phi,
    q_function = q_function
  )

  return(out)
}
