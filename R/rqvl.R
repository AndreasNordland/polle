fit_QV_function <- function(history, Z, qv_model){
  action_set <- getElement(history, "action_set")
  stage_action_set <- getElement(history, "stage_action_set")
  stage <- getElement(history, "stage")
  H <- get_H(history)

  # checking qv_model formula:
  formula <- get("formula", environment(qv_model))
  tt <- terms(formula, data = H)
  formula <- reformulate(attr(tt, "term.labels"), response = NULL)
  tt <- terms(formula, data = H)
  variables <- as.character(attr(tt, "variables"))[-1]
  if(!all(variables %in% colnames(H))){
    mes <- deparse(formula)
    mes <- paste("The QV-model formula", mes, "is invalid.")
    stop(mes)
  }

  # fitting the QV-model:
  qv_model <- apply(
    Z[, stage_action_set],
    MARGIN = 2,
    function(z){
      qv_model(V_res = z, AH = H)
    }
  )

  qv_function <- list(
    qv_model = qv_model,
    action_set = action_set,
    stage_action_set = stage_action_set,
    stage = stage
  )
  class(qv_function) <- "QV_function"

  return(qv_function)
}

evaluate.QV_function <- function(object, new_history){
  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)
  # action set of the new history object
  action_set <- getElement(new_history, "action_set")
  # stage action set of the fitted QV-function
  stage_action_set <- getElement(object, "stage_action_set")
  stage <- getElement(object, "stage")
  qv_model <- getElement(object, "qv_model")

  qv_values <- matrix(NA, nrow = nrow(new_H), ncol = length(action_set))
  colnames(qv_values) <- paste("QV_", action_set, sep = "")
  qv_values[, action_set %in% stage_action_set] <- sapply(
    qv_model,
    function(qvm){
      predict(qvm, new_H)
    }
  )
  qv_values <- data.table(id_stage, qv_values)
  setkeyv(qv_values, c("id", "stage"))

  return(qv_values)
}

#' @title Control arguments for QV-learning
#' @description \code{control_rqvl} sets the default control arguments
#' for doubly robust V-restricted Q-learning, \code{type = "rqvl"}.
#' @param qv_models Single element or list of V-restricted Q-models created
#' by [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' @returns list of (default) control arguments.
#' @export
control_rqvl <- function(qv_models = q_glm(~.)){
  control <- as.list(environment())
  return(control)
}

# note: if L > 1 and alpha > 0 then the g_functions fitted on the complete data
# is used to determine future realistic actions.
rqvl <- function(policy_data,
                 alpha,
                 g_models, g_functions, g_full_history,
                 q_models, q_full_history,
                 qv_models, full_history,
                 L, cross_fit_g_models,
                 save_cross_fit_models, future_args,
                 ...){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)

  # input checks:
  if (!(is.numeric(alpha) &  (length(alpha) == 1) & (alpha >=0 & alpha < 0.5)))
    stop("alpha must be numeric and in [0, 0.5).")
  if (!is.null(g_functions)){
    if(!inherits(g_functions, what = "g_functions"))
      stop("g-functions must be of class 'g_functions'.")
  }
  if (is.list(q_models)){
    if (length(q_models) != K)
      stop("q_models must either be a list of length K or a single Q-model.")
  }
  if(is.null(qv_models))
    stop("qv_models are missing.")
  if (is.list(qv_models)){
    if (length(qv_models) != K) stop("qv_models must either be a list of length K or a single QV-model.")
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
      future_args = future_args
    )
    if (save_cross_fit_models == TRUE){
      g_functions_cf <- getElement(g_cf, "functions")
    }
    g_values <- getElement(g_cf, "values")
    rm(g_cf)
  } else {
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
    g_values <- evaluate(g_functions, policy_data)
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
  qv_cols <- paste("QV_", action_set, sep = "")

  q_functions <- list()
  q_functions_cf <- list()
  qv_functions <- list()

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
        future_args = future_args
      )
      if (save_cross_fit_models == TRUE){
        q_functions_cf[[k]] <- getElement(q_step_cf_k, "q_functions_cf")
      }
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
    qv_history_k <- get_history(policy_data,
                                stage = k,
                                full_history = full_history)
    # fitting the QV-function:
    if (is.list(qv_models)){
      qv_model_k <- qv_models[[k]]
    } else{
      qv_model_k <- qv_models
    }
    qv_function_k <- fit_QV_function(qv_history_k,
                                     Z = Z,
                                     qv_model = qv_model_k)
    qv_functions[[k]] <- qv_function_k
    # getting the QV-function values:
    qv_values_k <- evaluate(qv_function_k, new_history = qv_history_k)

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
  class(out) <- c("RQVL","policy_object","list")

  return(out)
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.RQVL <- function(object, stage){
  stage_action_sets <- getElement(object, "stage_action_sets")
  stage_action_set <- stage_action_sets[[stage]]; rm(stage_action_sets)
  K <- getElement(object, "K")
  if(!((stage >= 0) & (stage <= K)))
    stop("stage must be smaller than or equal to K.")
  alpha <- getElement(object, "alpha")
  g_functions <- getElement(object, "g_functions")
  if (!is.null(g_functions)){
    g_full_history <- attr(g_functions, "full_history")
    if (length(g_functions) == K){
      g_function <- g_functions[[stage]]
    }
    else{
      g_function <- g_functions[[1]]
    }
  }
  qv_functions <- getElement(object, "qv_functions")
  qv_model <- getElement(qv_functions[[stage]], "qv_model")
  full_history <- attr(qv_functions, "full_history")

  stage_policy <- function(H){
    qv_values <- sapply(
      qv_model,
      function(qvm){
        predict(qvm, H)
      }
    )

    if (alpha == 0){
      dd <- apply(qv_values, MARGIN = 1, which.max)
      d <- stage_action_set[dd]
    } else{
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

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values, MARGIN = 1, function(x) x >= alpha))
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      dd <- apply(qv_values * realistic_actions, MARGIN = 1, which.max)
      d <- stage_action_set[dd]
    }

    return(d)
  }

  return(stage_policy)
}

#' @export
get_policy.RQVL <- function(object){
  g_functions <- get_g_functions(object)
  qv_functions <- getElement(object, "qv_functions")

  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")
  alpha <- getElement(object, "alpha")

  g_cols <- paste("g_", action_set, sep = "")
  qv_cols <- paste("QV_", action_set, sep = "")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")
    # evaluating the Q-functions:
    qv_values <- evaluate(qv_functions, policy_data = policy_data)

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- evaluate(g_functions, policy_data = policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(
        g_values[ , g_cols, with = FALSE],
        MARGIN = 1,
        function(x) x >= alpha
      ))
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      dd <- apply(
        qv_values[ , qv_cols, with = FALSE] * realistic_actions,
        MARGIN = 1,
        which.max
      )
      d <- action_set[dd]
    } else{
      # getting the action with the maximal Q-function value:
      dd <- apply(
        qv_values[ , qv_cols, with = FALSE],
        MARGIN = 1,
        which.max
      )
      d <- action_set[dd]
    }

    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d:= d]

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = "rqvl")

  return(policy)
}
