#' @export
ptl <- function(
  policy_data,
  alpha,
  g_models,
  q_models,
  q_full_history = FALSE,
  g_full_history = FALSE,
  policy_full_history = FALSE,
  policy_vars = NULL,
  M = NULL,
  depth = 2, split.step = 1,
  min.node.size = 1
){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
  }

  if (policy_full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K)) stop("policy_vars must be a list of length K, when policy_full_history = TRUE")
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs and the observed (complete) utilities:
  utility <- utility(policy_data)
  id <- utility$id

  # constructing the folds for cross-fitting
  if (!is.null(M)){
    folds <- split(sample(1:n, n), rep(1:M, length.out = n))
  } else{
    folds <- NULL
  }

  # cross-fitting the g-functions:
  g_functions <- NULL
  cf_g_functions <- NULL
  if (is.null(folds)){
    g_functions <- fit_g_functions(policy_data, g_models, full_history = g_full_history)
    g_values <- evaluate(g_functions, policy_data)
  } else{
    cf_g <- cf_fit_nuisance_functions(fit_functions = fit_g_functions, policy_data = policy_data, models = g_models, full_history = g_full_history, folds = folds)
    cf_g_functions <- cf_g$functions
    g_values <- cf_g$values
  }
  # getting the observed g-function values:
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  D <- matrix(nrow = n, ncol = K)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k)
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  ptl_objects <- list()
  q_functions <- list()
  for (k in K:1){
    # getting the IDs and ID-index:
    id_k <- get_id_stage(policy_data)[stage == k]$id
    idx_k <- (id %in% id_k)

    if (is.null(folds)){
      # getting the history for the Q-function:
      q_history_k <- get_stage_history(policy_data, stage = k, full_history = q_full_history)

      # fitting the Q-function:
      if (class(q_models)[[1]] == "list"){
        q_model_k <- q_models[[k]]
      } else{
        q_model_k <- q_models
      }

      q_function_k <- fit_Q_function(q_history_k, Q = Q[idx_k, k+1], q_model = q_model_k)
      q_functions[[k]] <- q_function_k


      # getting the Q-function values for each action:
      q_values_k <- evaluate(q_function_k, new_history = q_history_k)
    } else{
      # TODO
      stop()
    }


    # getting the action matrix for stage k:
    A_k <- actions[stage == k, ]$A
    IA_k <- action_matrix(A_k, action_set)

    # calculating Gamma
    Gamma_1 <- Q_k <- as.matrix(q_values_k[, ..q_cols, with = FALSE])
    Gamma_2 <- (IA_k / G[idx_k, k]) * (Q[idx_k, k+1] - Q_k)
    Gamma_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Gamma_3 <- Gamma_3 + ipw_weight(D[idx_k,(k+1):r], G[idx_k,(k+1):r]) * (Q[idx_k, r+1] - Q[idx_k, r])
      }
      Gamma_3 <- (IA_k / G[idx_k, k]) * Gamma_3
    }
    Gamma <- Gamma_1 + Gamma_2 + Gamma_3

    # getting the policy history
    policy_history_k <- get_stage_history(policy_data, stage = k, full_history = policy_full_history)
    if (policy_full_history == TRUE)
      vars <- policy_vars[[k]]
    else
      vars <- policy_vars
    X <- get_X(policy_history_k, vars = vars)

    ptl_k <- policytree::policy_tree(X = X, Gamma = Gamma, depth = depth, split.step = split.step, min.node.size = min.node.size)
    ptl_objects[[k]] <- ptl_k
    dd <- policytree:::predict.policy_tree(ptl_k, X)
    d <- action_set[dd]

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    Q[idx_k, k] <- q_d_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
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
    cf_g_functions = cf_g_functions,
    q_functions = q_functions,
    policy_full_history = policy_full_history,
    policy_vars = policy_vars,
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
  policy_vars <- getElement(object, "policy_vars")

  if (!is.list(policy_vars))
    policy_vars <- list(policy_vars)

  stage_policies <- mapply(
    ptl_objects,
    policy_vars,
    FUN = function(ptl, vars){
      pf <- function(history){
        X <- get_X(history, vars = vars)

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

  policy <- policy_def(
    stage_policies = stage_policies,
    full_history = policy_full_history
  )

  return(policy)
}
