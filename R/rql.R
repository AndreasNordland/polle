#' @export
rql<- function(policy_data, alpha = 0, g_models = NULL, g_functions = NULL, q_models = NULL, q_functions = NULL, q_full_history = FALSE, g_full_history = FALSE){
  if (!(is.numeric(alpha) &  (length(alpha) == 1) & (alpha >=0 & alpha < 0.5))) stop("alpha must be numeric and in [0, 0.5).")
  if (alpha != 0){
    if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
    if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")
    if (!is.null(g_functions)){
      if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
    }
  }
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }

  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  if (!is.null(q_models)){
    if (class(q_models)[[1]] == "list"){
      if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
    }
  }

  # fitting the g-functions if alpha > 0:
  if (alpha != 0){

    if (!is.null(g_models)){
      g_functions <- fit_g_functions(policy_data, models = g_models, full_history = g_full_history)
    }
    g_values <- evaluate(g_functions, policy_data)
  }

  # getting the IDs and the observed (complete) utilities:
  utility <- utility(policy_data)
  id <- utility$id

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  V <- matrix(nrow = n, ncol = K+1)
  V[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  g_cols <- paste("g_", action_set, sep = "")

  if (!is.null(q_models))
    q_functions <- list()
  for (k in K:1){
    # getting the history for the Q-function:
    q_history_k <- get_stage_history(policy_data, stage = k, full_history = q_full_history)

    # getting the IDs and ID-index:
    id_k <- q_history_k$H$id
    idx_k <- (id %in% id_k)

    # fitting the Q-function:
    if (!is.null(q_models)){
      if (class(q_models)[[1]] == "list"){
        q_model_k <- q_models[[k]]
      } else{
        q_model_k <- q_models
      }

      q_function_k <- fit_Q_function(q_history_k, V = V[idx_k, k+1], q_model = q_model_k)
      q_functions[[k]] <- q_function_k
    } else{
      q_function_k <- q_functions[[k]]
    }

    # getting the Q-function values for each action:
    q_values_k <- evaluate(q_function_k, new_history = q_history_k)

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values_k[,..g_cols], MARGIN = 1, function(x) x >= alpha))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the maximal realistic Q-function values:
      q_max_realistic_k <- apply(q_values_k[,..q_cols] * realistic_actions, MARGIN = 1, max, na.rm = TRUE)
    } else {
      q_max_realistic_k <- apply(q_values_k[,..q_cols], MARGIN = 1, max, na.rm = TRUE)
    }

    # inserting the maximal Q-function values in V:
    V[idx_k, k] <- q_max_realistic_k
    V[!idx_k, k] <- V[!idx_k, k+1]
  }
  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- q_full_history

  phi_or <- V[, 1]

  out <- list(
    q_functions = q_functions,
    g_functions = g_functions,
    value_estimate = mean(phi_or),
    phi_or = phi_or,
    action_set = action_set,
    alpha = alpha,
    K = K
  )
  class(out) <- "RQL"

  return(out)
}

#' @export
get_policy.RQL <- function(object){
  g_functions <- object$g_functions
  q_functions <- object$q_functions
  action_set <- object$action_set
  K <- object$K
  alpha <- object$alpha

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  policy <- function(policy_data){
    # evaluating the Q-functions:
    q_values <- evaluate(q_functions, policy_data = policy_data)

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- evaluate(g_functions, policy_data = policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values[,..g_cols], MARGIN = 1, function(x) x >= alpha))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      idx <- apply(q_values[,..q_cols] * realistic_actions, MARGIN = 1, which.max)
      d <- action_set[idx]
    } else{
      # getting the action with the maximal Q-function value:
      idx <- apply(q_values[,..q_cols], MARGIN = 1, which.max)
      d <- action_set[idx]
    }

    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d:= d]

    return(policy_actions)
  }

  return(policy)
}
