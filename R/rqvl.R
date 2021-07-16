#' @export
fit_QV_function <- function(history, Z, qv_model){

  X <- get_X(history)

  # fitting the QV-model
  qv_model <- apply(
    Z,
    MARGIN = 2,
    function(z){
      qv_model(z, X)
    }
  )

  qv_function <- list(
    qv_model = qv_model
  )
  class(qv_function) <- "QV_function"

  return(qv_function)
}
#' @export
evaluate.QV_function <- function(object, new_history){

  id_stage <- get_id_stage(new_history)
  new_X <- get_X(new_history)
  qv_model <- object$qv_model

  qv_values <- sapply(
    qv_model,
    function(qvm){
      predict(qvm, new_X)
    }
    )

  qv_values <- data.table(id_stage, qv_values)
  setkey(qv_values, id, stage)

  return(qv_values)
}

#' @export
rqvl <- function(
  policy_data,
  alpha,
  g_models,
  q_models,
  q_full_history = FALSE,
  g_full_history = FALSE,
  qv_models,
  qv_full_history = FALSE,
  M = NULL
){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
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
    cf_g <- cf_fit_nuisance_functions(
      fit_functions = fit_g_functions,
      policy_data = policy_data,
      models = g_models,
      full_history = g_full_history,
      folds = folds
    )
    cf_g_functions <- cf_g$functions
    g_values <- cf_g$values
  }
  # getting the observed g-function values:
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  Id <- matrix(nrow = n, ncol = K)

  # (n X K) matrix with entries g_k(A_k, H_k)
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Qd <- matrix(nrow = n, ncol = K+1)
  Qd[, K+1] <- U

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  q_functions <- list()
  qv_functions <- list()
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

      q_function_k <- fit_Q_function(q_history_k, V = Qd[idx_k, k+1], q_model = q_model_k)
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

    # calculating the Z-matrix
    Z_1 <- Q_k <- as.matrix(q_values_k[, ..q_cols, with = FALSE])
    Z_2 <- (IA_k / G[idx_k, k]) * (Qd[idx_k, k+1] - Q_k)
    Z_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Z_3 <- Z_3 + ipw_weight(Id[idx_k,(k+1):r], G[idx_k,(k+1):r]) * (Qd[idx_k, r+1] - Qd[idx_k, r])
      }
      Z_3 <- (IA_k / G[idx_k, k]) * Z_3
    }
    Z <- Z_1 + Z_2 + Z_3

    # getting the history for the QV model
    qv_history_k <- get_stage_history(policy_data, stage = k, full_history = qv_full_history)
    qv_function_k <- fit_QV_function(qv_history_k, Z = Z, qv_model = qv_models[[k]])
    qv_functions[[k]] <- qv_function_k

    qv_values_k <- evaluate(qv_function_k, new_history = qv_history_k)

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values_k[,..g_cols], MARGIN = 1, function(x) x >= alpha))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic QV-function value:
      dd <- apply(qv_values_k[,..q_cols] * realistic_actions, MARGIN = 1, which.max)
    } else {
      dd <- apply(qv_values_k[,..q_cols], MARGIN = 1, which.max)
    }

    d <- action_set[dd]

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    Qd[idx_k, k] <- q_d_k
    Qd[!idx_k, k] <- Qd[!idx_k, k+1]
    Id[idx_k, k] <- (A_k == d)
    Id[!idx_k, k] <- TRUE
    G[!idx_k,k] <- TRUE
  }

  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- q_full_history

  class(qv_functions) <- "nuisance_functions"
  attr(qv_functions, "full_history") <- qv_full_history

  phi_dr <- apply(action_matrix(d, action_set) * Z, 1, sum)

  out <- list(
    qv_functions = qv_functions,
    q_functions = q_functions,
    g_functions = g_functions,
    value_estimate = mean(phi_dr),
    phi_dr = phi_dr,
    action_set = action_set,
    alpha = alpha,
    K = K
  )
  class(out) <- "RQVL"

  return(out)
}

#' @export
get_policy.RQVL <- function(object){
  g_functions <- object$g_functions
  qv_functions <- object$qv_functions
  action_set <- object$action_set
  K <- object$K
  alpha <- object$alpha

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  policy <- function(policy_data){
    # evaluating the Q-functions:
    qv_values <- evaluate(qv_functions, policy_data = policy_data)

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- evaluate(g_functions, policy_data = policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values[,..g_cols], MARGIN = 1, function(x) x >= alpha))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      dd <- apply(qv_values[,..q_cols] * realistic_actions, MARGIN = 1, which.max)
      d <- action_set[dd]
    } else{
      # getting the action with the maximal Q-function value:
      dd <- apply(qv_values[,..q_cols], MARGIN = 1, which.max)
      d <- action_set[dd]
    }

    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d:= d]

    return(policy_actions)
  }

  return(policy)
}
