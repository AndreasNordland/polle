#' @title Control arguments for Policy Tree Learning
#' @description \code{control_ptl} sets the default control arguments
#' for doubly robust policy tree learning, \code{type = "ptl"}.
#' The arguments are passed directly to [policytree::policy_tree()] (or
#' [policytree::hybrid_policy_tree()]) if not specified otherwise.
#' @param policy_vars Character vector/string or list of character
#' vectors/strings. Variable names used to
#' construct the V-restricted policy tree.
#' The names must be a subset of the history names, see get_history_names().
#' Not passed to \code{policy_tree()}.
#' @param hybrid If \code{TRUE}, [policytree::hybrid_policy_tree()] is used to
#' fit a policy tree. Not passed to \code{policy_tree()}.
#' @param depth Integer or integer vector. The depth of the fitted policy
#' tree for each stage.
#' @param search.depth (only used if \code{hybrid = TRUE}) Integer or integer
#' vector. Depth to look ahead when splitting at each stage.
#' @param split.step Integer or integer vector. The number of possible splits
#' to consider when performing policy tree search at each stage.
#' @param min.node.size Integer or integer vector. The smallest terminal node
#' size permitted at each stage.
#' @returns list of (default) control arguments.
#' @export
control_ptl <- function(policy_vars = NULL,
                        hybrid = FALSE,
                        depth = 2,
                        search.depth = 2,
                        split.step = 1,
                        min.node.size = 1) {
  control <- as.list(environment())
  return(control)
}

ptl <- function(policy_data,
                g_models, g_functions, g_full_history,
                q_models, q_full_history,
                policy_vars, full_history,
                L,
                cross_fit_g_models, save_cross_fit_models,
                future_args,
                alpha,
                depth, split.step, min.node.size, hybrid, search.depth,
                ...) {
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)

  # input checks:
  if (alpha > 0) {
    for (k in seq_along(stage_action_sets)) {
      if (length(stage_action_sets[[k]]) != 2) {
        mes <- paste0(
          "realistic policy tree learning is only implemented for ",
          "dichotomous stage action sets. Use ",
          "doubly robust Q-learning (drql) instead."
        )
        stop(mes)
      }
    }
  }
  if ((is.null(g_models) && is.null(g_functions))) {
    stop("Provide either g-models or g-functions.")
  }
  if (is.list(q_models)) {
    if (length(q_models) != K) {
      stop("q_models must either be a list of length K or a single Q-model.")
    }
  }
  if (full_history == TRUE) {
    if ((!is.list(policy_vars)) || (length(policy_vars) != K)) {
      stop("policy_vars must be a list of length K, when full_history = TRUE.")
    }
  }
  if (is.list(depth)) {
    depth <- unlist(depth)
  }
  if (length(depth) == 1) {
    depth <- rep(depth, K)
  } else {
    if (length(depth) != K || !all(depth %% 1 == 0)) {
      stop("depth must be an integer vector of length K.")
    }
  }
  if (is.list(split.step)) {
    split.step <- unlist(split.step)
  }
  if (length(split.step) == 1) {
    split.step <- rep(split.step, K)
  } else {
    if (length(split.step) != K || !all(split.step %% 1 == 0)) {
      stop("split.step must be an integer vector of length K.")
    }
  }
  if (is.list(min.node.size)) {
    min.node.size <- unlist(min.node.size)
  }
  if (length(min.node.size) == 1) {
    min.node.size <- rep(min.node.size, K)
  } else {
    if (length(min.node.size) != K || !all(min.node.size %% 1 == 0)) {
      stop("min.node.size must be an integer vector of length K.")
    }
  }
  if (is.list(search.depth)) {
    search.depth <- unlist(search.depth)
  }
  if (length(search.depth) == 1) {
    search.depth <- rep(search.depth, K)
  } else {
    if (length(search.depth) != K || !all(search.depth %% 1 == 0)) {
      stop("search.depth must be an integer vector of length K.")
    }
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # constructing the folds for cross-fitting
  if (L > 1) {
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
  } else {
    folds <- NULL
  }

  # (cross-)fitting the g-functions:
  g_functions_cf <- NULL
  if (!is.null(folds) && cross_fit_g_models == TRUE) {
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
    if (is.null(g_functions)) {
      g_functions <- fit_g_functions(policy_data,
        g_models = g_models,
        full_history = g_full_history
      )
    }
    g_values <- predict(g_functions, policy_data)
  }

  # fitting g-functions for determining new realistic actions:
  if (alpha > 0) {
    if (is.null(g_functions)) {
      g_functions <- fit_g_functions(policy_data,
        g_models = g_models,
        full_history = g_full_history
      )
    }
  } else {
    # g-functions are not saved if alpha == 0:
    g_functions <- NULL
  }

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  II <- matrix(nrow = n, ncol = K)
  # (n X K) matrix with entries g_k(A_k, H_k)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)
  G <- dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE]
  G <- as.matrix(G)
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U
  # (n X K) matrix with entries d_k(H_k) (including unrealistic actions)
  D <- matrix(nrow = n, ncol = K)

  q_cols <- paste("Q_", action_set, sep = "")
  ptl_objects <- list()
  ptl_designs <- list()
  q_functions <- list()
  q_functions_cf <- list()
  for (k in K:1){
    if (is.null(folds)) {
      q_step_k <- q_step(
        policy_data = policy_data,
        k = k,
        full_history = q_full_history,
        Q = Q[, k + 1],
        q_models = q_models
      )
      # getting the Q-function, Q-function values and the ID-index:
      q_functions[[k]] <- getElement(q_step_k, "q_function")
      q_values_k <- getElement(q_step_k, "q_values")
      idx_k <- getElement(q_step_k, "idx_k")
    } else {
      q_step_cf_k <- q_step_cf(
        folds = folds,
        policy_data = policy_data,
        k = k,
        full_history = q_full_history,
        Q = Q[, k + 1],
        q_models = q_models,
        save_cross_fit_models = save_cross_fit_models,
        future_args = future_args
      )
      q_functions_cf[[k]] <- getElement(q_step_cf_k, "q_functions_cf")
      q_values_k <- getElement(q_step_cf_k, "q_values")
      idx_k <- getElement(q_step_cf_k, "idx_k")
      rm(q_step_cf_k)
    }

    # getting the stage action set
    stage_action_set_k <- stage_action_sets[[k]]

    # getting the action matrix for stage k:
    stage <- NULL
    A_k <- unlist(actions[stage == k, "A"])
    IA_k <- action_matrix(A_k, action_set)
    rm(stage)

    # calculating Gamma (Z-matrix), see ?policytree::policy_tree
    Gamma_1 <- Q_k <- as.matrix(q_values_k[, q_cols, with = FALSE])
    Gamma_2 <- (IA_k / G[idx_k, k]) * (Q[idx_k, k+1] - Q_k)
    Gamma_3 <- 0
    if (k != K) {
      for (r in (k + 1):K) {
        Gamma_3 <- Gamma_3 +
          ipw_weight(II[idx_k, (k + 1):r], G[idx_k, (k + 1):r]) *
          (Q[idx_k, r + 1] - Q[idx_k, r])
      }
      Gamma_3 <- (IA_k / G[idx_k, k]) * Gamma_3
    }
    Gamma <- Gamma_1 + Gamma_2 + Gamma_3
    colnames(Gamma) <- action_set
    Gamma <- Gamma[, stage_action_set_k]

    # getting the policy history
    policy_history_k <- get_history(policy_data,
                                    stage = k,
                                    full_history = full_history)
    if (full_history == TRUE) {
      vars <- policy_vars[[k]]
    } else {
      vars <- policy_vars
    }
    H <- get_H(policy_history_k, vars = vars)
    if (is.null(policy_vars)) {
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

    if (hybrid == FALSE) {
      ptl_k <- policytree::policy_tree(
        X = X,
        Gamma = Gamma,
        depth = depth_k,
        split.step = split.step_k,
        min.node.size = min.node.size_k
      )
    } else if (hybrid == TRUE) {
      ptl_k <- policytree::hybrid_policy_tree(
        X = X,
        Gamma = Gamma,
        depth = depth_k,
        split.step = split.step_k,
        min.node.size = min.node.size_k,
        search.depth = search.depth_k
      )
    }
    ptl_objects[[k]] <- ptl_k
    dd <- predict(ptl_k, X)
    d <- stage_action_set_k[dd]

    if (alpha != 0) {
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      g_stage_cols <- paste("g_", stage_action_set_k, sep = "")
      # modifying the policy to only recommend realistic actions
      d[(g_values_k[, g_stage_cols[1], with = FALSE] < alpha)] <-
        stage_action_set_k[2]
      d[(g_values_k[, g_stage_cols[2], with = FALSE] < alpha)] <-
        stage_action_set_k[1]
    }

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)[["P"]]
    Q[idx_k, k] <- q_d_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
    II[idx_k, k] <- (A_k == d)
    II[!idx_k, k] <- TRUE
    D[idx_k, k] <- d
    G[!idx_k, k] <- TRUE
  }

  # setting outputs:
  if (length(q_functions) > 0) {
    class(q_functions) <- "nuisance_functions"
    attr(q_functions, "full_history") <- q_full_history
    names(q_functions) <- paste("stage_", 1:K, sep = "")
  } else {
    q_functions <- NULL
  }
  if (length(q_functions_cf) == 0) {
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
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    alpha = alpha,
    K = K,
    folds = folds
  )
  out <- remove_null_elements(out)
  class(out) <- c("ptl", "policy_object")

  return(out)
}

#' @export
get_policy.ptl <- function(object, threshold = NULL) {
  if (!(is.null(threshold) || identical(threshold, 0))) {
    stop("threshold is not implemented for ptl.")
  }
  stage_action_sets <- getElement(object, "stage_action_sets")
  K <- getElement(object, "K")
  full_history <- getElement(object, "full_history")
  ptl_objects <- getElement(object, "ptl_objects")
  ptl_designs <- getElement(object, "ptl_designs")
  policy_vars <- getElement(object, "policy_vars")
  g_functions <- getElement(object, "g_functions")
  alpha <- getElement(object, "alpha")

  policy <- function(policy_data) {
    if (get_K(policy_data) != K) {
      stop("The policy do not have the same number of stages as the policy data object.")
    }

    if (alpha != 0) {
      # evaluating the g-functions:
      g_values <- predict(g_functions, policy_data)
    }

    ### getting the actions recommended by the ptl objects:
    policy_actions <- list()
    for (k in K:1) {
      # getting the policy history:
      policy_history_k <- get_history(policy_data,
        stage = k,
        full_history = full_history
      )

      # getting the design matrix:
      if (full_history == TRUE) {
        vars <- policy_vars[[k]]
      } else {
        vars <- policy_vars
      }
      H <- get_H(policy_history_k, vars = vars)
      design <- ptl_designs[[k]]
      newdata <- apply_design(design = design, data = H)

      # getting the stage action set
      stage_action_set <- stage_action_sets[[k]]

      # policy tree predictions:
      dd <- predict(ptl_objects[[k]], newdata = newdata)
      d <- stage_action_set[dd]

      # excluding unrealistic recommendations:
      if (alpha != 0) {
        g_stage_cols <- paste("g_", stage_action_set, sep = "")
        stage <- NULL
        g_values_k <- g_values[stage == k, ]

        d[(g_values_k[, g_stage_cols[1], with = FALSE] < alpha)] <-
          stage_action_set[2]
        d[(g_values_k[, g_stage_cols[2], with = FALSE] < alpha)] <-
          stage_action_set[1]
      }

      pa <- get_id_stage(policy_history_k)
      pa[, d := d]
      policy_actions[[k]] <- pa
      rm(pa, d, dd)
    }
    policy_actions <- rbindlist(policy_actions)
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = "ptl")

  return(policy)
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.ptl <- function(object, stage, ...) {
  K <- getElement(object, "K")
  check_stage(stage = stage, K = K)
  stage_action_sets <- getElement(object, "stage_action_sets")
  g_functions <- getElement(object, "g_functions")

  if (!is.null(g_functions)) {
    if (length(g_functions) == K) {
      g_function <- g_functions[[stage]]
    } else {
      g_function <- g_functions[[1]]
    }
    rm(g_functions)
  }

  ptl_object <- getElement(object, "ptl_objects")[[stage]]
  ptl_design <- getElement(object, "ptl_designs")[[stage]]
  full_history <- getElement(object, "full_history")
  if (full_history) {
    policy_vars <- getElement(object, "policy_vars")[[stage]]
  } else {
    policy_vars <- getElement(object, "policy_vars")
  }
  stage_action_set <- stage_action_sets[[stage]]
  rm(stage_action_sets)
  alpha <- getElement(object, "alpha")

  rm(object)
  stage_policy <- function(H) {
    H <- as.data.table(H)
    if (!all(policy_vars %in% colnames(H))) {
      mes <- "H must have column names "
      mes <- paste(mes, paste(policy_vars, collapse = ", "), ".", sep = "")
      stop(mes)
    }
    newdata <- H[, policy_vars, with = FALSE]
    mf <- with(ptl_design, model.frame(terms,
      data = newdata,
      xlev = xlevels,
      drop.unused.levels = FALSE
      ))
    newdata <- model.matrix(mf, data = newdata, xlev = ptl_design$xlevels)
    idx_inter <- which(colnames(newdata) == "(Intercept)")
    if (length(idx_inter) > 0) {
      newdata <- newdata[, -idx_inter, drop = FALSE]
    }

    dd <- predict(ptl_object, newdata = newdata)
    d <- stage_action_set[dd]

    if (alpha > 0) {
      # evaluating the g-function:
      if (!all(g_function$H_names %in% names(H))) {
        mes <- paste(
          "H must contain the columns",
          paste(g_function$H_names, collapse = ","),
          "."
        )
        stop(mes)
      }
      g_values <- predict(g_function$g_model, H)
      colnames(g_values) <- stage_action_set

      # excluding unrealistic recommendations:
      d[(g_values[, stage_action_set[1]] < alpha)] <- stage_action_set[2]
      d[(g_values[, stage_action_set[2]] < alpha)] <- stage_action_set[1]
    }

    return(d)
  }

  return(stage_policy)
}
