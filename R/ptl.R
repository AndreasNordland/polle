#' @export
ptl <- function(policy_data, alpha, g_models = NULL, g_functions = NULL, q_models = NULL, q_functions = NULL, q_full_history = FALSE, g_full_history = FALSE, policy_full_history = FALSE, depth = 2, split.step = 1, min.node.size = 1){
  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")
  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
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

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(policy_data, g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)

  # getting the IDs and the observed (complete) utilities:
  utility <- utility(policy_data)
  id <- utility$id

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  D <- matrix(nrow = n, ncol = K)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k)
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  V <- matrix(nrow = n, ncol = K+1)
  V[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  ptl_objects <- list()
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

    # getting the action matrix for stage k:
    A_k <- actions[stage == k, ]$A
    IA_k <- action_matrix(A_k, action_set)

    # calculating Gamma
    Gamma_1 <- Q_k <- as.matrix(q_values_k[, ..q_cols, with = FALSE])
    Gamma_2 <- (IA_k / G[idx_k, k]) * (V[idx_k, k+1] - Q_k)
    Gamma_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Gamma_3 <- Gamma_3 + ipw_weight(D[idx_k,(k+1):r], G[idx_k,(k+1):r]) * (V[idx_k, r+1] - V[idx_k, r])
      }
      Gamma_3 <- (IA_k / G[idx_k, k]) * Gamma_3
    }
    Gamma <- Gamma_1 + Gamma_2 + Gamma_3

    # getting the policy history
    policy_history_k <- get_stage_history(policy_data, stage = k, full_history = policy_full_history)
    X <- get_X(policy_history_k)
    ptl_k <- policytree::policy_tree(X = X, Gamma = Gamma, depth = depth, split.step = split.step, min.node.size = min.node.size)
    ptl_objects[[k]] <- ptl_k
    dd <- policytree:::predict.policy_tree(ptl_k, X)
    d <- action_set[dd]

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    V[idx_k, k] <- q_d_k
    V[!idx_k, k] <- V[!idx_k, k+1]
    D[idx_k, k] <- (A_k == d)
    D[!idx_k, k] <- TRUE
    G[!idx_k,k] <- TRUE

  }

  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- q_full_history

  out <- list(
    ptl_objects = ptl_objects,
    depth = depth,
    split.step = split.step,
    min.node.size = min.node.size,
    g_functions = g_functions,
    q_functions = q_functions,
    policy_full_history = policy_full_history,
    action_set = action_set,
    K = K
  )
  class(out) <- "PTL"

  return(out)
}

#' @export
get_policy.PTL <- function(object){
  action_set <- object$action_set
  K <- object$K
  policy_full_history <- object$policy_full_history
  ptl_objects <- object$ptl_objects

  stage_policies <- mapply(
    ptl_objects,
    FUN = function(ptl){
      pf <- function(history){
        X <- get_X(history)

        dd <- predict(ptl, newdata = X)
        d <- action_set[dd]

        policy_actions <- get_id_stage(history)
        policy_actions[, d:= d]
        setkey(policy_actions, id, stage)

        return(policy_actions)
      }
      return(pf)
    }
  )

  policy <- new_policy(
    stage_policies = stage_policies,
    full_history = policy_full_history
  )

  return(policy)
}
