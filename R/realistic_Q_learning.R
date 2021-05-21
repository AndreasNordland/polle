# TODO problem: the policy-function, q-function and the g-function may require different types of histories. Thus, a policy function cannot be a function of a history object.

#' @export
realistic_Q_learning <- function(policy_data, alpha, g_model, q_model, g_full_history, q_full_history, ...)
  UseMethod("realistic_Q_learning")

#' @export
realistic_Q_learning.policy_data <- function(policy_data, alpha, g_model = NULL, g_function = NULL, q_model = NULL, q_function = NULL, q_full_history, g_full_history){
  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set

  stopifnot(
    is.numeric(alpha),
    length(alpha) == 1,
    alpha >=0 & alpha < 0.5
  )

  stopifnot(
    !(is.null(g_model) & is.null(g_function)),
    !(!is.null(g_function) & !is.null(g_model)),
    !(is.null(q_model) & is.null(q_function)),
    !(!is.null(q_function) & !is.null(q_model))
  )

  if (!is.null(q_model)){
    # checking q_model:
    # must be a list of length K.
    stopifnot(
      if(class(q_model)[[1]] == "list")
        length(q_model) == K
      else FALSE
    )
  }
  if (!is.null(q_function)){
    # checking q_function:
    # must be a list of length K.
    stopifnot(
      if(class(q_function)[[1]] == "list")
        length(q_function) == K
      else FALSE
    )
  }

  # g-function
  if (!is.null(g_model)){
    g_function <- fit_g_model(policy_data, g_model, g_full_history = g_full_history)
  }
  # getting the g-function predictions
  g_function_predictions <- get_function_predictions(policy_data, fun = g_function, full_history = g_full_history)

  # getting the IDs and the observed (complete) utility
  utility <- utility(policy_data)
  id <- utility$id

  # constructing a matrix for the (predicted) Q-function values under the estimated policy (note that V_{K+1} = U)
  V <- matrix(nrow = n, ncol = K+1)
  V[, K+1] <- utility$U

  q_cols <- paste("Q_", action_set, sep = "")
  g_cols <- paste("g_", action_set, sep = "")

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
    q_predictions <- predict(qf, new_history = history)
    # getting the (predicted) g-function values for each action
    g_predictions <- g_function_predictions[stage == k, ]
    # calculating the realistic actions
    realistic_actions <- t(apply(g_predictions[,..g_cols], MARGIN = 1, function(x) x >= alpha))
    realistic_actions[which(realistic_actions == FALSE)] <- NA
    # getting the highest Q-function value of the realistic actions
    Q_max_rea <- apply(q_predictions[,..q_cols] * realistic_actions, MARGIN = 1, max, na.rm = TRUE)
    q_predictions[, Q_max_rea := Q_max_rea]
    # inserting the maximum realistic Q-function values in V
    qd <- merge(data.table(id = id), q_predictions[, c("id", "stage", "Q_max_rea")], all.x = TRUE, by = "id")
    idx <- which(is.na(qd$stage))
    V[ ,k] <- qd$Q_max_rea
    V[idx, k] <- V[idx, k+1]
  }

  phi_or <- V[, 1]

  out <- list(
    q_function = q_function,
    g_function = g_function,
    value = mean(phi_or),
    phi_or = phi_or,
    q_full_history = q_full_history,
    g_full_history = g_full_history,
    action_set = action_set,
    alpha = alpha,
    K = K
  )
  class(out) <- "realistic_Q_learning_function"

  return(out)
}

#' @export
get_policy.realistic_Q_learning_function <- function(object){
  g_function <- object$g_function
  q_function <- object$q_function
  g_full_history <- object$g_full_history
  q_full_history <- object$q_full_history
  action_set <- object$action_set
  K <- object$K
  alpha <- object$alpha

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  policy <- function(policy_data){
    # getting the g-function predictions
    g_predictions <- get_function_predictions(policy_data, fun = g_function, full_history = g_full_history)
    # getting the Q-function predictions
    q_predictions <- get_function_predictions(policy_data, fun = q_function, full_history = q_full_history)

    # calculating the realistic actions
    realistic_actions <- t(apply(g_predictions[,..g_cols], MARGIN = 1, function(x) x >= alpha))
    realistic_actions[which(realistic_actions == FALSE)] <- NA

    # getting the action with the highest realistic Q-function value
    idx <- apply(q_predictions[,..q_cols] * realistic_actions, MARGIN = 1, which.max)
    d <- action_set[idx]
    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d:= d]

    return(policy_actions)
  }

  return(policy)
}
