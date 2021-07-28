#' @export
fit_Q_function <- function(object, Q, q_model)
  UseMethod("fit_Q_function")

#' @export
fit_Q_function.history <- function(object, Q, q_model){

  action_name <- object$action_name
  action_set <- object$action_set
  action_utility_names <- object$action_utility_names

  # getting the action (A) and the model matrix (X):
  A <- get_A(object)
  X <- get_X(object)

  AX <- cbind(A, X)

  # checking that all actions in the actions set occur:
  if (!all(action_set == sort(unique(A)))) stop("An action in the action set does not occur.")

  # getting the historic rewards
  U <- object$U
  # calculating the residual (fitted) values
  U_A <- apply(action_matrix(a = A, action_set = action_set) * U[, ..action_utility_names], MARGIN = 1, sum)
  U[, V_res := Q - U_bar - U_A]
  V_res <- U$V_res

  # fitting the (residual) Q-model
  q_model <- q_model(V_res = V_res, AX = AX)

  q_function <- list(
    q_model = q_model,
    AX_names = colnames(AX)
  )
  class(q_function) <- "Q_function"

  return(q_function)
}

#' @export
evaluate.Q_function <- function(object, new_history){
  q_model <- object$q_model
  action_set <- new_history$action_set
  action_utility_names <- new_history$action_utility_names

  id_stage <- get_id_stage(new_history)
  new_X <- get_X(new_history)

  # getting the historic rewards
  U <- new_history$U

  # getting the residual predictions
  residual_q_predictions <- sapply(action_set, function(a) predict(q_model, new_AX = cbind(A = a, new_X)))
  # adding the historic utilities and deterministic rewards
  q_values <- U$U_bar + U[, ..action_utility_names] + residual_q_predictions
  names(q_values) <- paste("Q", action_set, sep = "_")

  # including the IDs and stage number
  q_values <- data.table(id_stage, q_values)
  setkey(q_values, id, stage)

  return(q_values)
}

Q_step <- function(policy_data, k, full_history, Q, q_models){
  id <- get_id(policy_data)
  id_k <- get_id_stage(policy_data)[stage == k]$id
  idx_k <- (id %in% id_k)

  if (class(q_models)[[1]] == "list"){
    q_model <- q_models[[k]]
  } else{
    q_model <- q_models
  }

  # getting the Q-function history:
  q_history <- get_stage_history(policy_data, stage = k, full_history = full_history)
  # fitting the Q-function:
  q_function <- fit_Q_function(q_history, Q = Q[idx_k], q_model = q_model)
  # getting the Q-function values for each action
  q_values <- evaluate(q_function, new_history = q_history)

  out <- list(
    q_function = q_function,
    q_values = q_values,
    idx_k = idx_k
  )
  return(out)
}

cf_Q_step <- function(folds, policy_data, k, full_history, Q, q_models){
  id <- get_id(policy_data)
  id_k <- get_id_stage(policy_data)[stage == k]$id
  idx_k <- (id %in% id_k)
  K <- policy_data$dim$K

  cf_q_step <- lapply(
    folds,
    function(f){
      train_id <- id[-f]
      train_policy_data <- subset(policy_data, train_id)
      train_Q <- Q[-f]
      if (train_policy_data$dim$K != K) stop("The number of stages K varies across the policy data training folds.")
      train_q_step <- Q_step(train_policy_data, k = k, full_history = full_history, Q = train_Q, q_models = q_models)
      train_q_function <- train_q_step$q_function

      validation_id <- id[f]
      validation_policy_data <- subset(policy_data, validation_id)
      validation_history <- get_stage_history(validation_policy_data, stage = k, full_history = full_history)
      validation_values <- evaluate(train_q_function, validation_history)

      out <- list(
        train_q_function = train_q_function,
        validation_values = validation_values
      )
      return(out)
    }
  )
  cf_q_step <- simplify2array(cf_q_step)

  q_functions <- cf_q_step["train_q_function", ]
  q_values <- cf_q_step["validation_values", ]

  q_values <- rbindlist(q_values)
  setkeyv(q_values, c("id", "stage"))

  out <- list(
    q_functions = q_functions,
    q_values = q_values,
    idx_k = idx_k
  )
  return(out)
}

#' @export
fit_Q_functions <- function(policy_data, policy_actions, q_models, full_history = FALSE){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  # checking q_models: must either be a list of length K or a single Q-model
  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
  }

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utility:
  utility <- utility(policy_data)

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  # fitting the Q-functions:
  q_functions <- list()
  for (k in K:1){
    q_step_k <- Q_step(
      policy_data = policy_data,
      k = k,
      full_history = full_history,
      Q = Q[, k+1],
      q_models = q_models
    )
    # getting the Q-function, Q-function values and the ID-index
    q_functions[[k]] <- q_step_k$q_function
    q_values_k <- q_step_k$q_values
    idx_k <- q_step_k$idx_k

    # getting the Q-function values under the policy
    d_k <- policy_actions[stage == k, ]$d
    q_d_values_k <- get_a_values(a = d_k, action_set = action_set, values = q_values_k)$P

    # inserting the Q-function values under the policy in Q
    Q[idx_k, k] <- q_d_values_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
  }

  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- full_history

  return(q_functions)
}
