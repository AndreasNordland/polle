rql<- function(policy_data, alpha,
               g_models, g_functions, g_full_history,
               q_models, q_full_history,
               ...){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  if (!(is.numeric(alpha) &  (length(alpha) == 1) & (alpha >=0 & alpha < 0.5))) stop("alpha must be numeric and in [0, 0.5).")

  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions"))
      stop("g-functions must be of class 'nuisance_functions'.")
  }

  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
  }

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # (n) vector with entries U_i:
  U <- utility$U

  # fitting the g-functions if alpha > 0:
  if (alpha != 0){
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data, g_models = g_models, full_history = g_full_history)
    }
    g_values <- evaluate(g_functions, policy_data)
  }

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  g_cols <- paste("g_", action_set, sep = "")

  q_functions <- list()
  for (k in K:1){
    # fitting the Q-function:
    q_step_k <- q_step(
      policy_data = policy_data,
      k = k,
      full_history = q_full_history,
      Q = Q[, k+1],
      q_models = q_models
    )
    # getting the Q-function, Q-function values and the ID-index:
    q_functions[[k]] <- q_step_k$q_function
    q_values_k <- q_step_k$q_values
    idx_k <- q_step_k$idx_k

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

    # inserting the maximal Q-function values in Q:
    Q[idx_k, k] <- q_max_realistic_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
  }
  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- q_full_history

  # Zd_or <- Q[, 1]

  out <- list(
    q_functions = q_functions,
    g_functions = g_functions,
    action_set = action_set,
    alpha = alpha,
    K = K
  )
  out <- remove_null_elements(out)
  class(out) <- c("RQL", "policy_object", "list")

  return(out)
}

#' @export
get_policy.RQL <- function(object){
  g_functions <- get_g_functions(object)
  q_functions <- get_q_functions(object)
  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")
  alpha <- getElement(object, "alpha")

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  policy <- function(policy_data){
    # evaluating the Q-functions:
    q_values <- evaluate(q_functions, policy_data = policy_data)

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- evaluate(g_functions, policy_data = policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(
        g_values[ , g_cols, with = FALSE],
        MARGIN = 1,
        function(x) x >= alpha
      ))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      idx <- apply(
        q_values[ , q_cols, with = FALSE] * realistic_actions,
        MARGIN = 1,
        which.max
      )
      d <- action_set[idx]
    } else{
      # getting the action with the maximal Q-function value:
      idx <- apply(
        q_values[ , q_cols, with = FALSE],
        MARGIN = 1,
        which.max
      )
      d <- action_set[idx]
    }

    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d := d]

    return(policy_actions)
  }
  class(policy) <- c("policy", "function")
  return(policy)
}
