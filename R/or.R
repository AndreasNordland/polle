#' @export
or <- function(policy_data, q_models = NULL, q_functions = NULL, policy, q_full_history = FALSE){
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }

  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  # getting the policy actions
  policy_actions <- policy(policy_data = policy_data)

  # fitting the q-functions
  if (!is.null(q_models)){
    q_functions <- fit_Q_functions(policy_data, policy_actions = policy_actions, q_models = q_models, full_history = q_full_history)
  }
  q_values <- evaluate(q_functions, policy_data = policy_data)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)

  # (n X K) matrix with entries Q_k(d_k(H_k), H_k)
  V <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_or <- V[, 1]
  out <- list(
    value_estimate = mean(phi_or),
    phi_or = phi_or,
    q_functions = q_functions,
    id = get_id(policy_data)
  )

  return(out)
}
