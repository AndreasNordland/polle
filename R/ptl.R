#' @export
ptl <- function(policy_data,
                g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                q_models, q_full_history = FALSE,
                policy_vars = NULL, policy_full_history = FALSE,
                L = NULL, cf_models = FALSE, future_args = NULL,
                alpha = 0,
                depth = 2, split.step = 1, min.node.size = 1, hybrid = FALSE, search.depth = 2,
                verbose = FALSE,
                ...){
  if ((is.null(g_models) & is.null(g_functions))) stop("Provide either g-models or g-functions.")

  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)

  if (!((0<=alpha) & (0.5>alpha)))
    stop("alpha must be in [0, 0.5).")
  if (alpha > 0){
    if (length(action_set) != 2)
      stop("realistic policy tree learning is only implemented for binary actions. Use realistic QV-learning (rqvl) instead.")
  }

  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K)
      stop("q_models must either be a list of length K or a single Q-model.")
  }
  if (policy_full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K))
      stop("policy_vars must be a list of length K, when policy_full_history = TRUE.")
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

  g_functions_cf <- NULL
  if (is.null(folds)){
    if (is.null(g_functions)){
      # fitting the g-functions:
      g_functions <- fit_g_functions(policy_data, g_models, full_history = g_full_history)
      if (verbose == TRUE){
        print("Policy tree: g-functions completed.")
      }

    }
    g_values <- evaluate(g_functions, policy_data)
  } else{
    # cross-fitting the g-functions:
    g_cf <- fit_g_functions_cf(
      policy_data = policy_data,
      g_models = g_models,
      full_history = g_full_history,
      folds = folds,
      future_args = future_args
    )
    if (verbose == TRUE)
      print("Policy tree: cross-fitted g-functions completed.")
    if (cf_models == TRUE){
      g_functions_cf <- g_cf$g_functions_cf
    }

    g_values <- g_cf$g_values
    # fitting the non-cross-fitted g-functions
    # for determining future realistic actions:
    if (alpha > 0){
      if (is.null(g_functions)){
        g_functions <- fit_g_functions(policy_data,
                                       g_models = g_models,
                                       full_history = g_full_history)
        if (verbose == TRUE)
          print("Policy tree: g-functions completed.")
      }
    } else{
      g_functions <- NULL
    }
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
  # (n X K) matrix with entries d_k(H_k) (including unrealistic actions)
  D <- matrix(nrow = n, ncol = K)

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")
  ptl_objects <- list()
  q_functions <- list()
  q_functions_cf <- list()
  for (k in K:1){
    if (is.null(folds)){
      # fitting the Q-function
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

      if (verbose == TRUE){
        mes <- paste("Policy tree: Q-function at stage ",k, " completed.", sep = "")
        print(mes)
      }
    } else{
      # cross-fitting the Q-function
      q_step_cf_k <- q_step_cf(
        folds = folds,
        policy_data = policy_data,
        k = k,
        full_history = q_full_history,
        Q = Q[, k+1],
        q_models = q_models,
        future_args = future_args
      )
      if (cf_models == TRUE){
        q_functions_cf[[k]] <- q_step_cf_k$q_function
      }
      q_values_k <- q_step_cf_k$q_values
      idx_k <- q_step_cf_k$idx_k
      if (verbose == TRUE){
        mes <- paste("Policy tree: Cross-fitted Q-function at stage ",k, " completed.", sep = "")
        print(mes)
      }
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
    policy_history_k <- get_history(policy_data, stage = k, full_history = policy_full_history)
    if (policy_full_history == TRUE)
      vars <- policy_vars[[k]]
    else
      vars <- policy_vars
    H <- get_H(policy_history_k, vars = vars)

    if (hybrid == FALSE){
      ptl_k <- policytree::policy_tree(X = H, Gamma = Gamma, depth = depth, split.step = split.step, min.node.size = min.node.size)
    } else if (hybrid == TRUE){
      ptl_k <- policytree::hybrid_policy_tree(X = H, Gamma = Gamma, depth = depth, split.step = split.step, min.node.size = min.node.size, search.depth = search.depth)
    }

    ptl_objects[[k]] <- ptl_k
    dd <- policytree:::predict.policy_tree(ptl_k, H)
    d <- action_set[dd]

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # modifying the policy to only recommend realistic actions
      d[(g_values_k[, g_cols[1], with = FALSE] < alpha)] <- action_set[2]
      d[(g_values_k[, g_cols[2], with = FALSE] < alpha)] <- action_set[1]
    }

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    Q[idx_k, k] <- q_d_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
    II[idx_k, k] <- (A_k == d)
    II[!idx_k, k] <- TRUE
    D[idx_k, k] <- d
    G[!idx_k,k] <- TRUE

    if (verbose == TRUE){
      mes <- paste("Policy tree: stage ",k, " completed.", sep = "")
      print(mes)
    }
  }

  if (length(q_functions) > 0){
    class(q_functions) <- "nuisance_functions"
    attr(q_functions, "full_history") <- q_full_history
    names(q_functions) <- paste("stage_", 1:K, sep = "")
  } else{
    q_functions <- NULL
  }
  if (length(q_functions_cf) == 0){
    q_functions_cf <- NULL
  }

  Gamma_d <- apply(action_matrix(d, action_set) * Gamma, 1, sum)

  names(ptl_objects) <- paste("stage_", 1:K, sep = "")


  out <- list(
    ptl_objects = ptl_objects,
    depth = depth,
    split.step = split.step,
    min.node.size = min.node.size,
    value_estimate = mean(Gamma_d),
    iid = Gamma_d - mean(Gamma_d),
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    g_values = g_values,
    q_functions = q_functions,
    q_functions_cf = q_functions_cf,
    policy_full_history = policy_full_history,
    policy_vars = policy_vars,
    D = D,
    action_set = action_set,
    alpha = alpha,
    K = K,
    folds = folds
  )
  class(out) <- c("PTL", "policy_object")

  return(out)
}

#' @export
get_policy.PTL <- function(object){

action_set <- object$action_set
K <- object$K
policy_full_history <- object$policy_full_history
ptl_objects <- object$ptl_objects
policy_vars <- getElement(object, "policy_vars")
g_functions = object$g_functions
alpha <- object$alpha

policy <- function(policy_data){
  if (get_K(policy_data) != K)
    stop("The policy do not have the same number of stages as the policy data object.")

  # getting the actions recommended by the ptl objects:
  policy_actions <- list()
  for (k in K:1){
    # getting the policy history:
    policy_history_k <- get_history(policy_data, stage = k, full_history = policy_full_history)

    if (policy_full_history == TRUE)
      vars <- policy_vars[[k]]
    else
      vars <- policy_vars

    H <- get_H(policy_history_k, vars = vars)
    dd <- predict(ptl_objects[[k]], newdata = H)
    d <- action_set[dd]

    pa <- get_id_stage(policy_history_k)
    pa[, d:= d]
    policy_actions[[k]] <- pa
    rm(pa, d, dd)
  }
  policy_actions <- rbindlist(policy_actions)
  setkey(policy_actions, id, stage)

  # excluding unrealistic recommendations:
  if (alpha != 0){
    g_cols <- paste("g_", action_set, sep = "")
    # evaluating the g-functions:
    g_values <- evaluate(g_functions, policy_data = policy_data)

    d_ <- policy_actions$d
    d_[(g_values[, g_cols[1], with = FALSE] < alpha)] <- action_set[2]
    d_[(g_values[, g_cols[2], with = FALSE] < alpha)] <- action_set[1]
  } else{
    d_ <- policy_actions$d
  }

  # inserting the modified actions:
  policy_actions[, d:= d_]

  return(policy_actions)
}

return(policy)
}

#' @export
get_policy_functions.PTL <- function(object, stage){
  action_set <- object$action_set
  K <- object$K

  if(!((stage >= 0) & (stage <= K)))
    stop("stage must be smaller than or equal to K.")

  if (!is.null(object$g_functions)){
    g_full_history <- attr(object$g_functions, "full_history")
    if (length(object$g_functions) == K){
      g_function <- object$g_functions[[stage]]
    }
    else{
      g_function <- object$g_functions[[1]]
    }
  }

  ptl_object <- object$ptl_objects[[stage]]

  policy_full_history <- object$policy_full_history

  if(policy_full_history)
    policy_vars <- object$policy_vars[[stage]]
  else
    policy_vars <- object$policy_vars

  alpha <- object$alpha

  if (alpha == 0){
    stage_policy <- function(H, ...){
      if(!all(policy_vars == colnames(H))){
        mes <- "H must have column names "
        mes <- paste(mes, paste(policy_vars, collapse = ", "), " (in that order).", sep = "")
        stop(mes)
      }

      dd <- predict(ptl_object, newdata = H)
      d <- action_set[dd]
      return(d)
    }
  }
  if (alpha > 0){
    stage_policy <- function(H, g_H){
      if(!all(policy_vars == colnames(H))){
        mes <- "H must have column names "
        mes <- paste(mes, paste(policy_vars, collapse = ", "), " (in that order).", sep = "")
        stop(mes)
      }

      dd <- predict(ptl_object, newdata = H)
      d <- action_set[dd]

      # evaluating the g-function:
      if (!all(g_function$H_names == names(g_H))){
        mes <- paste(
          "g_H must contain the columns",
          paste(g_function$H_names, collapse = ","),
          "(in that order)."
        )
        stop(mes)
      }
      g_values <- predict(g_function$g_model, new_H = g_H)
      g_cols <- paste("g", action_set, sep = "_")
      colnames(g_values) <- g_cols

      d[(g_values[, g_cols[1]] < alpha)] <- action_set[2]
      d[(g_values[, g_cols[2]] < alpha)] <- action_set[1]


      return(d)
    }
  }

  return(stage_policy)
}
