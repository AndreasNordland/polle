#' @export
Q_learning <- function(policy_data, q_model, q_full_history, ...)
  UseMethod("Q_learning")

#' @export
Q_learning.policy_data <- function(policy_data, q_model = NULL, q_function = NULL, q_full_history){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  stopifnot(
    !(is.null(q_model) & is.null(q_function)),
    !(!is.null(q_function) & !is.null(q_model))
  )

  if (!is.null(q_model)){
    # checking q_model: must either be a list of length K.
    stopifnot(
      if(class(q_model)[[1]] == "list")
        length(q_model) == K
      else FALSE
    )
  }

  # getting the IDs and the observed (complete) utility
  utility <- utility(policy_data)
  id <- utility$id

  # constructing a matrix for the (predicted) Q-function values under the estimated policy (note that V_{K+1} = U)
  V <- matrix(nrow = n, ncol = K+1)
  V[, K+1] <- utility$U

  q_cols <- paste("Q_", action_set, sep = "")

  if (!is.null(q_model))
    q_function <- list()
  for (k in K:1){

    # getting the history
    history <- get_stage_history(policy_data, stage = k, full_history = q_full_history)
    # fitting the Q-function
    if (!is.null(q_model)){
      qf <- Q_function(history, V = data.table(id = id, V = V[, k+1]), q_model = q_model[[k]])
      q_function[[k]] <- qf
    } else{
      qf <- q_function[[k]]
    }
    # getting the (predicted) Q-function values for each action
    q_prediction <- predict(qf, new_history = history)
    # getting the maximum of the Q-function values
    q_prediction[, Q_max := do.call(pmax, .SD), .SDcols = q_cols]
    # inserting the maximum Q-function values in V
    qd <- merge(data.table(id = id), q_prediction[, c("id", "stage", "Q_max")], all.x = TRUE, by = "id")
    idx <- which(is.na(qd$stage))
    V[ ,k] <- qd$Q_max
    V[idx, k] <- V[idx, k+1]
  }

  phi_or <- V[, 1]

  out <- list(
    q_function = q_function,
    value = mean(phi_or),
    phi_or = phi_or,
    q_full_history = q_full_history,
    action_set = action_set
  )
  class(out) <- "Q_learning_function"

  return(out)
}

#' @export
get_policy.Q_learning_function <- function(object){
  q_full_history <- object$q_full_history
  q_function <- object$q_function
  action_set <- object$action_set

  q_cols <- paste("Q_", action_set, sep = "")

  stage_policies <- lapply(
    q_function,
    function(qf){
      pf <- function(history){
        # getting the (predicted) Q-function values for each action
        q_predictions <- predict(qf, new_history = history)
        # getting the action with the highest Q-function value
        idx <- apply(q_predictions[,..q_cols], MARGIN = 1, which.max)
        d <- action_set[idx]
        # collecting the policy actions
        policy_actions <- get_id_stage(history)
        policy_actions[, d:= d]
        setkey(policy_actions, id, stage)
        return(policy_actions)
      }
      return(pf)
    }
  )

  policy <- new_policy(
    stage_policies = stage_policies,
    full_history = q_full_history
  )

  return(policy)
}



