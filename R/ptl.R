#' @export
ptl <- function(policy_data,
                g_models, g_functions, g_full_history,
                q_models, q_full_history,
                policy_vars = NULL, policy_full_history = FALSE,
                L = NULL, alpha = 0,
                depth = 2, split.step = 1, min.node.size = 1,
                ...){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)

  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
  }
  if (policy_full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K)) stop("policy_vars must be a list of length K, when policy_full_history = TRUE")
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- utility(policy_data)

  # constructing the folds for cross-fitting
  if (!is.null(L)){
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
  } else{
    folds <- NULL
  }

  # cross-fitting the g-functions:
  g_functions_cf <- NULL
  if (is.null(folds)){
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data, g_models, full_history = g_full_history)
    }
    g_values <- evaluate(g_functions, policy_data)
  } else{
    g_cf <- fit_g_functions_cf(
      policy_data = policy_data,
      g_models = g_models,
      full_history = g_full_history,
      folds = folds
    )
    g_functions_cf <- g_cf$g_functions_cf
    g_values <- g_cf$g_values
  }

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  II <- matrix(nrow = n, ncol = K)
  # (n X K) matrix with entries g_k(d_k(H_k), H_k)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  ptl_objects <- list()
  q_functions <- list()
  q_functions_cf <- list()
  for (k in K:1){
    if (is.null(folds)){
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
    } else{
      q_step_cf_k <- q_step_cf(
        folds = folds,
        policy_data = policy_data,
        k = k,
        full_history = q_full_history,
        Q = Q[, k+1],
        q_models = q_models
      )
      q_functions_cf[[k]] <- q_step_cf_k$q_function
      q_values_k <- q_step_cf_k$q_values
      idx_k <- q_step_cf_k$idx_k
    }

    # getting the action matrix for stage k:
    A_k <- actions[stage == k, ]$A
    IA_k <- action_matrix(A_k, action_set)

    # calculating Gamma (Z-matrix), see ?policytree::policy_tree
    Gamma_1 <- Q_k <- as.matrix(q_values_k[, ..q_cols, with = FALSE])
    Gamma_2 <- (IA_k / G[idx_k, k]) * (Q[idx_k, k+1] - Q_k)
    Gamma_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Gamma_3 <- Gamma_3 + ipw_weight(II[idx_k,(k+1):r], G[idx_k,(k+1):r]) * (Q[idx_k, r+1] - Q[idx_k, r])
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
    II[idx_k, k] <- (A_k == d)
    II[!idx_k, k] <- TRUE
    G[!idx_k,k] <- TRUE

  }

  if (length(q_functions) > 1){
    class(q_functions) <- "nuisance_functions"
    attr(q_functions, "full_history") <- q_full_history
  } else{
    q_functions <- NULL
  }
  if (length(q_functions_cf) == 0){
    q_functions_cf <- NULL
  }

  Gamma_d <- apply(action_matrix(d, action_set) * Gamma, 1, sum)

  out <- list(
    ptl_objects = ptl_objects,
    depth = depth,
    split.step = split.step,
    min.node.size = min.node.size,
    value_estimate = mean(Gamma_d),
    iid = Gamma_d - mean(Gamma_d),
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    q_functions = q_functions,
    q_functions_cf = q_functions_cf,
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
