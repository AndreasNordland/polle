drb <- function(policy_data,
                 alpha,
                 g_models, g_functions, g_full_history,
                 q_models, q_full_history,
                 blip_models, full_history,
                 L, cross_fit_g_models,
                 save_cross_fit_models, future_args,
                 ...){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)

  browser()

  # input checks:
  if (!is.null(g_functions)){
    if(!inherits(g_functions, what = "g_functions"))
      stop("g-functions must be of class 'g_functions'.")
  }
  if (is.list(q_models)){
    if (length(q_models) != K)
      stop("q_models must either be a list of length K or a single Q-model.")
  }
  if(is.null(blip_models))
    stop("blip_models are missing.")
  if (is.list(blip_models)){
    if (length(blip_models) != K) stop("blip_models must either be a list of length K or a single QV-model.")
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # constructing the folds for cross-fitting:
  if (L > 1){
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
  } else{
    folds <- NULL
  }

  # (cross-)fitting the g-functions:
  g_functions_cf <- NULL
  if (!is.null(folds) & cross_fit_g_models == TRUE){
    g_cf <- fit_g_functions_cf(
      policy_data = policy_data,
      g_models = g_models,
      full_history = g_full_history,
      folds = folds,
      save_cross_fit_models = save_cross_fit_models,
      future_args = future_args
    )
    g_functions_cf <- getElement(g_cf, "functions")
    g_values <- getElement(g_cf, "values")
    rm(g_cf)
  } else {
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
    g_values <- predict(g_functions, policy_data)
  }

  # fitting g-functions for determining new realistic actions:
  if (alpha > 0){
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
  } else{
    # g-functions are not saved if alpha == 0:
    g_functions <- NULL
  }

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  II <- matrix(nrow = n, ncol = K)
  # (n X K) matrix with entries g_k(A_k, H_k)
  g_A_values <- get_a_values(a = actions$A,
                             action_set = action_set,
                             g_values)
  G <- dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE]
  G <- as.matrix(G)
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U
  # (n X K) matrix with entries d_k(H_k) (including unrealistic actions)
  D <- matrix(nrow = n, ncol = K)

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")
  blip_cols <- paste("B_", action_set, sep = "")

  q_functions <- list()
  q_functions_cf <- list()
  blip_functions <- list()

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
        q_models = q_models,
        save_cross_fit_models = save_cross_fit_models,
        future_args = future_args
      )
      q_functions_cf[[k]] <- getElement(q_step_cf_k, "q_functions_cf")
      q_values_k <- getElement(q_step_cf_k, "q_values")
      idx_k <- getElement(q_step_cf_k, "idx_k")
      rm(q_step_cf_k)
    }

    # getting the action matrix for stage k:
    stage <- NULL
    A_k <- actions[stage == k, ]$A
    IA_k <- action_matrix(A_k, action_set)
    rm(stage)

    # calculating the Z-matrix
    Z_1 <- Q_k <- as.matrix(q_values_k[, q_cols, with = FALSE])
    Z_2 <- (IA_k / G[idx_k, k]) * (Q[idx_k, k+1] - Q_k)
    Z_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Z_3 <- Z_3 + ipw_weight(II[idx_k,(k+1):r], G[idx_k,(k+1):r]) *
          (Q[idx_k, r+1] - Q[idx_k, r])
      }
      Z_3 <- (IA_k / G[idx_k, k]) * Z_3
    }
    Z <- Z_1 + Z_2 + Z_3
    colnames(Z) <- action_set

    # getting the history for the QV model:
    blip_history_k <- get_history(policy_data,
                                stage = k,
                                full_history = full_history)
    # fitting the blip-function:
    if (is.list(blip_models)){
      blip_model_k <- blip_models[[k]]
    } else{
      blip_model_k <- blip_models
    }
    blip_function_k <- fit_blip_function(blip_history_k,
                                         Z = Z,
                                         blip_model = blip_model_k)
    qv_functions[[k]] <- qv_function_k
    # getting the QV-function values:
    qv_values_k <- predict(qv_function_k, new_history = qv_history_k)

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values_k[, g_cols, with = FALSE],
                                   MARGIN = 1,
                                   function(x) x >= alpha))
      # checking that a realistic action exists:
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic QV-function value:
      dd <- apply(qv_values_k[, qv_cols, with = FALSE] * realistic_actions, MARGIN = 1, which.max)
    } else {
      dd <- apply(qv_values_k[, qv_cols, with = FALSE], MARGIN = 1, which.max)
    }

    d <- action_set[dd]

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    Q[idx_k, k] <- q_d_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
    II[idx_k, k] <- (A_k == d)
    II[!idx_k, k] <- TRUE
    D[idx_k, k] <- d
    G[!idx_k,k] <- TRUE
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
  class(qv_functions) <- "nuisance_functions"
  attr(qv_functions, "full_history") <- full_history
  names(qv_functions) <- paste("stage_", 1:K, sep = "")

  out <- list(
    qv_functions = qv_functions,
    q_functions = q_functions,
    q_functions_cf = q_functions_cf,
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    alpha = alpha,
    K = K,
    folds = folds
  )
  out <- remove_null_elements(out)
  class(out) <- c("drql","policy_object","list")

  return(out)
}
