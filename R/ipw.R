# TODO check robustness if not all actions are represented at each stage

#' @export
ipw <- function(object, g_models, g_functions, policy, g_full_history)
  UseMethod("ipw")

#' @export
ipw.policy_data <- function(object, g_models = NULL, g_functions = NULL, policy, g_full_history = FALSE){
  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")

  K <- object$dim$K
  action_set <- object$action_set

  # getting the observed actions:
  actions <- get_actions(object)

  # getting the policy actions:
  policy_actions <- policy(policy_data = object)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(object, g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, object)
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, g_values)

  # (n) vector with entries U_i:
  U <- utility(object)$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  DA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  Dd <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  D <- (DA == Dd); rm(DA, Dd)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_ipw <- ipw_weight(D = D, G = G) * U

  out <- list(
    value_estimate = mean(phi_ipw),
    phi_ipw = phi_ipw,
    g_functions = g_functions
  )

  return(out)
}
