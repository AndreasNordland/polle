#' @export
or <- function(object, Q_model, Q_function, policy, Q_full_history, policy_full_history)
  UseMethod("or")

#' @export
or.policy_data <- function(object, Q_model = NULL, Q_function = NULL, policy, Q_full_history = FALSE, policy_full_history = FALSE){
  stopifnot(
    !(is.null(Q_model) & is.null(Q_function)),
    !(!is.null(Q_function) & !is.null(Q_model))
  )

  K <- object$dim$K
  action_set <- object$action_set

  policy_actions <- get_policy_actions(object, policy = policy, policy_full_history = policy_full_history)

  if (!is.null(Q_model)){
    Q_function <- fit_Q_model(object, policy_actions = policy_actions, Q_model = Q_model, Q_full_history = Q_full_history)
  }
  Q_function_predictions <- get_function_predictions(object, fun = Q_function, full_history = Q_full_history)
  Q_function_policy_predictions <- get_action_predictions(A = policy_actions$d, action_set = action_set, predictions = Q_function_predictions)

  # V: (n X K) matrix
  V <- as.matrix(dcast(Q_function_policy_predictions, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi <- V[, 1]
  out <- list(
    value = mean(phi),
    phi = phi,
    Q_function = Q_function
  )

  return(out)
}
