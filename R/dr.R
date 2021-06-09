#' @export
dr <- function(object, g_models, g_functions, q_models, q_functions, policy, g_full_history, q_full_history)
  UseMethod("dr")

#' @export
dr.policy_data <- function(object, g_models = NULL, g_functions = NULL, q_models = NULL, q_functions = NULL, policy, g_full_history = FALSE, q_full_history = FALSE){
  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }
  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
  }

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
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = g_values)

  # fitting the Q-functions:
  if (!is.null(q_models)){
    q_functions <- fit_Q_functions(object, policy_actions = policy_actions, q_models = q_models, full_history = q_full_history)
  }
  q_values <- evaluate(q_functions, policy_data = object)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  DA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  Dd <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  D <- (DA == Dd); rm(DA, Dd)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  # (n) vector with entries U_i:
  U <- utility(object)$U

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  V <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  V <- apply(
    V,
    MARGIN = 2,
    function(v){
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  V <- cbind(V, U)

  # calculating the doubly robust score
  phi_dr <- V[, 1]
  for (k in 1:K){
    phi_dr <- phi_dr + ipw_weight(D = D[,1:k], G = G[,1:k]) * (V[,k+1] - V[,k])
  }

  # getting the IPW and OR score
  phi_ipw <- ipw_weight(D = D, G = G) * U
  phi_or <- V[, 1]

  out <- list(
    value_estimate = mean(phi_dr),
    phi_dr = phi_dr,
    phi_ipw = phi_ipw,
    phi_or = phi_or,
    g_functions = g_functions,
    q_functions = q_functions
  )

  return(out)
}
