#' @export
fit_Q_function <- function(object, V, q_model)
  UseMethod("fit_Q_function")

#' @export
fit_Q_function.history <- function(object, V, q_model){

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
  U[, V_res := V - U_bar - U_A]
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

#' @export
fit_Q_functions <- function(policy_data, policy_actions, q_models, full_history = FALSE){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  # checking q_models: must either be a list of length K or a single Q-model
  if (class(q_models)[[1]] == "list"){
    if (length(q_models) != K) stop("q_models must either be a list of length K or a single Q-model.")
  }

  # getting the IDs and the observed (complete) utility U
  utility <- utility(policy_data)
  id <- utility$id

  # (n) vector with entries U_i
  U <- utility$U

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U
  V <- matrix(nrow = n, ncol = K+1)
  V[, K+1] <- U

  q_functions <- list()
  for (k in K:1){
    # getting the IDs and ID-index:
    id_k <- get_id_stage(policy_data)[stage == k]$id
    idx_k <- (id %in% id_k)

    if (class(q_models)[[1]] == "list"){
      q_model_k <- q_models[[k]]
    } else{
      q_model_k <- q_models
    }
    # getting the Q-function history:
    q_history_k <- get_stage_history(policy_data, stage = k, full_history = full_history)
    # fitting the Q-function:
    q_function_k <- fit_Q_function(q_history_k, V = V[idx_k, k+1], q_model = q_model_k)
    q_functions[[k]] <- q_function_k

    # getting the Q-function values for each action
    q_values_k <- evaluate(q_function_k, new_history = q_history_k)
    # getting the Q-function values under the policy
    d_k <- policy_actions[stage == k, ]$d
    q_d_values_k <- get_a_values(a = d_k, action_set = action_set, values = q_values_k)$P
    # inserting the Q-function values under the policy in V
    V[idx_k, k] <- q_d_values_k
    V[!idx_k, k] <- V[!idx_k, k+1]
  }

  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- full_history

  return(q_functions)
}

# cv_fit_Q_functions <- function(folds, policy_data, policy_actions, q_models, full_history = FALSE){
#   id <- get_id(policy_data)
#
#   lapply(
#     folds,
#     function(fold){
#       browser()
#       train_id <- id[-fold]
#
#       train_policy_data <- new_policy_data(
#         stage_data = policy_data$stage_data[id %in% train_id],
#         baseline_data = policy_data$baseline_data[id %in% train_id]
#       )
#
#       train_policy_actions <- policy_actions[id %in% train_id, ]
#
#       fit_Q_functions(
#         policy_data = train_policy_data,
#         policy_actions = train_policy_actions
#       )
#
#     }
#   )
# }
