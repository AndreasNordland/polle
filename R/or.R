# TODO: profile or

#' @export
or <- function(object, q_models, q_functions, policy, q_full_history)
  UseMethod("or")

#' @export
or.policy_data <- function(object, q_models = NULL, q_functions = NULL, policy, q_full_history = FALSE){
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }

  K <- object$dim$K
  action_set <- object$action_set

  # getting the policy actions
  policy_actions <- policy(policy_data = object)

  # fitting the q-functions
  if (!is.null(q_models)){
    q_functions <- fit_Q_functions(object, policy_actions = policy_actions, q_models = q_models, full_history = q_full_history)
  }
  # else {
  #   if (class(q_function)[[1]] == "list"){
  #     stopifnot(length(q_function) == K)
  #   }
  #   if (class(q_function)[[1]] == "Q_function"){
  #     q_function <- list(q_function)
  #   }
  # }
  q_values <- evaluate(object = q_functions, policy_data = object)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)

  # (n X K) matrix with entries Q_k(d_k(H_k), H_k)
  V <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_or <- V[, 1]
  out <- list(
    value_estimate = mean(phi_or),
    phi_or = phi_or,
    q_functions = q_functions
  )

  return(out)
}
