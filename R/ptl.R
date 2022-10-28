ptl <- function(policy_data,
                g_models, g_functions, g_full_history,
                q_models, q_full_history,
                policy_vars, full_history,
                L, save_cross_fit_models, future_args,
                alpha,
                depth, split.step, min.node.size, hybrid, search.depth,
                ...
                ){
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

  if (full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K))
      stop("policy_vars must be a list of length K, when full_history = TRUE.")
  }

  if (is.list(depth)){
    depth <- unlist(depth)
  }
  if (length(depth) == 1){
    depth <- rep(depth, K)
  } else{
    if (length(depth) != K | !all(depth%%1==0)){
      stop("depth must be an integer vector of length K.")
    }
  }

  if (is.list(split.step)){
    split.step <- unlist(split.step)
  }
  if (length(split.step) == 1){
    split.step <- rep(split.step, K)
  } else{
    if (length(split.step) != K | !all(split.step%%1==0)){
      stop("split.step must be an integer vector of length K.")
    }
  }

  if (is.list(min.node.size)){
    min.node.size <- unlist(min.node.size)
  }
  if (length(min.node.size) == 1){
    min.node.size <- rep(min.node.size, K)
  } else{
    if (length(min.node.size) != K | !all(min.node.size%%1==0)){
      stop("min.node.size must be an integer vector of length K.")
    }
  }

  if (is.list(search.depth)){
    search.depth <- unlist(search.depth)
  }
  if (length(search.depth) == 1){
    search.depth <- rep(search.depth, K)
  } else{
    if (length(search.depth) != K | !all(search.depth%%1==0)){
      stop("search.depth must be an integer vector of length K.")
    }
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # constructing the folds for cross-fitting
  if (L > 1){
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
  } else{
    folds <- NULL
  }

  g_functions_cf <- NULL
  if (is.null(folds)){
    if (is.null(g_functions)){
      # fitting the g-functions:
      g_functions <- fit_g_functions(policy_data, g_models, full_history = g_full_history)
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
    if (save_cross_fit_models == TRUE){
      g_functions_cf <- getElement(g_cf, "functions")
    }

    g_values <- getElement(g_cf, "values")
    # fitting the non-cross-fitted g-functions
    # for determining future realistic actions:
    if (alpha > 0){
      if (is.null(g_functions)){
        g_functions <- fit_g_functions(policy_data,
                                       g_models = g_models,
                                       full_history = g_full_history)
      }
    } else{
      g_functions <- NULL
    }
  }

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  II <- matrix(nrow = n, ncol = K)
  # (n X K) matrix with entries g_k(A_k, H_k)
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
  ptl_designs <- list()
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
      if (save_cross_fit_models == TRUE){
        q_functions_cf[[k]] <- q_step_cf_k$q_function
      }
      q_values_k <- q_step_cf_k$q_values
      idx_k <- q_step_cf_k$idx_k
    }

    # getting the action matrix for stage k:
    stage <- NULL
    A_k <- unlist(actions[stage == k, "A"])
    IA_k <- action_matrix(A_k, action_set)
    rm(stage)

    # calculating Gamma (Z-matrix), see ?policytree::policy_tree
    Gamma_1 <- Q_k <- as.matrix(q_values_k[, q_cols, with = FALSE])
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
    policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)
    if (full_history == TRUE){
      vars <- policy_vars[[k]]
    } else{
      vars <- policy_vars
    }
    H <- get_H(policy_history_k, vars = vars)
    if (is.null(policy_vars)){
      policy_vars <- names(H)
    }

    # design matrix for policy_tree:
    design_k <- get_design(formula = ~., data = H)
    X <- design_k$x
    design_k$x <- NULL
    ptl_designs[[k]] <- design_k

    # tuning parameters for policy_tree:
    depth_k <- depth[[k]]
    split.step_k <- split.step[[k]]
    min.node.size_k <- min.node.size[[k]]
    search.depth_k <- search.depth[[k]]

    if (hybrid == FALSE){
      ptl_k <- policytree::policy_tree(X = X,
                                       Gamma = Gamma,
                                       depth = depth_k,
                                       split.step = split.step_k,
                                       min.node.size = min.node.size_k)
    } else if (hybrid == TRUE){
      ptl_k <- policytree::hybrid_policy_tree(X = X,
                                              Gamma = Gamma,
                                              depth = depth_k,
                                              split.step = split.step_k,
                                              min.node.size = min.node.size_k,
                                              search.depth = search.depth_k)
    }

    ptl_objects[[k]] <- ptl_k
    dd <- predict(ptl_k, X)
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

  }

  # setting outputs:
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
  names(ptl_objects) <- paste("stage_", 1:K, sep = "")

  out <- list(
    ptl_objects = ptl_objects,
    ptl_designs = ptl_designs,
    full_history = full_history,
    policy_vars = policy_vars,
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    q_functions = q_functions,
    q_functions_cf = q_functions_cf,
    # g_values = g_values,
    # D = D,
    action_set = action_set,
    alpha = alpha,
    K = K,
    folds = folds
  )
  out <- remove_null_elements(out)
  class(out) <- c("PTL", "policy_object")

  return(out)
}

#' @export
get_policy.PTL <- function(object){

  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")
  full_history <- getElement(object, "full_history")
  ptl_objects <- getElement(object, "ptl_objects")
  ptl_designs <- getElement(object, "ptl_designs")
  policy_vars <- getElement(object, "policy_vars")
  g_functions <- getElement(object, "g_functions")
  alpha <- getElement(object, "alpha")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")

    # getting the actions recommended by the ptl objects:
    policy_actions <- list()
    for (k in K:1){
      # getting the policy history:
      policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)

      if (full_history == TRUE){
        vars <- policy_vars[[k]]
      } else{
        vars <- policy_vars
      }
      H <- get_H(policy_history_k, vars = vars)

      des <- ptl_designs[[k]]
      mf <- with(des, model.frame(terms, data=H, xlev = x_levels, drop.unused.levels=FALSE))
      newdata <- model.matrix(mf, data=H, xlev = des$x_levels)

      dd <- predict(ptl_objects[[k]], newdata = newdata)
      d <- action_set[dd]

      pa <- get_id_stage(policy_history_k)
      pa[, d := d]
      policy_actions[[k]] <- pa
      rm(pa, d, dd)
    }
    policy_actions <- rbindlist(policy_actions)
    setkeyv(policy_actions, c("id", "stage"))

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
  class(policy) <- c("policy", "function")
  return(policy)
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.PTL <- function(object, stage){
  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")

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

  ptl_object <- getElement(object, "ptl_objects")[[stage]]
  ptl_design <- getElement(object, "ptl_designs")[[stage]]
  full_history <- getElement(object, "full_history")
  if(full_history){
    policy_vars <- getElement(object, "policy_vars")[[stage]]
  } else{
    policy_vars <- getElement(object, "policy_vars")
  }
  alpha <- getElement(object, "alpha")

  stage_policy <- function(H){
    H <- as.data.table(H)
    if(!all(policy_vars %in% colnames(H))){
      mes <- "H must have column names "
      mes <- paste(mes, paste(policy_vars, collapse = ", "), ".", sep = "")
      stop(mes)
    }
    newdata <- H[, policy_vars, with = FALSE]
    mf <- with(ptl_design, model.frame(terms, data = newdata, xlev = x_levels, drop.unused.levels=FALSE))
    newdata <- model.matrix(mf, data = newdata, xlev = ptl_design$x_levels)
    dd <- predict(ptl_object, newdata = newdata)
    d <- action_set[dd]

    if (alpha >0){
      # evaluating the g-function:
      if (!all(g_function$H_names %in% names(H))){
        mes <- paste(
          "H must contain the columns",
          paste(g_function$H_names, collapse = ","),
          "."
        )
        stop(mes)
      }
      g_values <- predict(g_function$g_model, new_H = H)
      g_cols <- paste("g", action_set, sep = "_")
      colnames(g_values) <- g_cols

      d[(g_values[, g_cols[1]] < alpha)] <- action_set[2]
      d[(g_values[, g_cols[2]] < alpha)] <- action_set[1]
    }

    return(d)
  }

  return(stage_policy)
}
