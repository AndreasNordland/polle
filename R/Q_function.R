#' @export
Q_function <- function(object, V, Q_model)
  UseMethod("Q_function")

#' @export
Q_function.history <- function(object, V, Q_model){

  action_name <- object$action_name
  action_set <- object$action_set
  action_utility_names <- object$action_utility_names

  # getting the action (A) and the model matrix (X):
  A <- get_A(object)
  X <- get_X(object)

  # getting the historic utilities
  U <- object$U
  # merging the (fitted) values (V)
  U <- merge(U, V, all.x = TRUE, by = c("id")); stopifnot(all(complete.cases(U)))
  # calculating the residual (fitted) values
  U_A <- apply(action_matrix(A = A, action_set = action_set) * U[, ..action_utility_names], MARGIN = 1, sum)
  U[, V_res := V - U_bar - U_A]
  V_res <- U$V_res

  # fitting the (residual) Q-model
  qm <- Q_model(V_res = V_res, A = A, X = X)

  q_function <- list(
    qm = qm,
    X_names = colnames(X)
  )
  class(q_function) <- "q_function"

  return(q_function)
}

predict.q_function <- function(object, new_history){
  X_names <- object$X_names
  q_model <- object$qm
  action_set <- new_history$action_set
  action_utility_names <- new_history$action_utility_names

  id_stage <- get_id_stage(new_history)
  new_X <- get_X(new_history)

  # checking that the model matrix has the correct form (could be an issue if factor levels are missing)
  stopifnot(
    names(new_X) == X_names
  )

  # getting the historic utilities
  U <- new_history$U
  # getting the residual predictions
  residual_q_predictions <- predict(q_model, new_X = new_X, action_set = action_set)
  # adding the historic utilities and deterministic utility contributions
  q_predictions <- U$U_bar + U[, ..action_utility_names] + residual_q_predictions
  names(q_predictions) <- paste("Q", action_set, sep = "_")

  # including the IDs and stage number
  q_predictions <- data.table(id_stage, q_predictions)
  setkey(q_predictions, id, stage)

  return(q_predictions)
}

fit_Q_model <- function(policy_data, policy_actions, Q_model, Q_full_history = TRUE){
  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  # checking g_model: must either be a list of length K.
  stopifnot(
    if(class(Q_model)[[1]] == "list")
      length(Q_model) == K
    else FALSE
  )

  # getting the IDs and the observed (complete) utility
  U <- utility(policy_data)
  id <- U$id

  # constructing a matrix for the (predicted) Q-function values under the policy (note that V_{K+1} = U)
  V <- matrix(nrow= n, ncol = K+1)
  V[, K+1] <- U$U

  out <- list()
  for (k in K:1){
    # getting the history
    q_history <- get_stage_history(policy_data, stage = k, full_history = Q_full_history)
    # fitting the Q-function
    q_function <- Q_function(q_history, V = data.table(id = id, V = V[, k+1]), Q_model = Q_model[[k]])
    out[[k]] <- q_function
    # getting the (predicted) Q-function values for each action
    q_predictions <- predict(q_function, new_history = q_history)
    # getting the Q-function values under the policy
    q_predictions <- get_action_predictions(A = policy_actions[stage == k,]$d, action_set = action_set, predictions = q_predictions)

    # inserting the Q-function values under the policy in V
    qd <- merge(data.table(id = id), q_predictions, all.x = TRUE, by = "id")
    idx <- which(is.na(qd$stage))
    V[ ,k] <- qd$P
    V[idx, k] <- V[idx, k+1]
  }

  return(out)
}
