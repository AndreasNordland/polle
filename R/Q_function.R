fit_Q_function <- function(history, Q, q_model){

  action_set <- getElement(history, "action_set")
  deterministic_rewards <- getElement(history, "deterministic_rewards")

  # getting the action (A) and the model matrix (H):
  A <- get_A(history)
  H <- get_H(history)

  AH <- cbind(A, H)

  # checking that all actions in the actions set occur:
  if (!all(action_set == sort(unique(A)))){
    mes <- "Not all actions occur at stage"
    k <- getElement(history, "stage")
    mes <- paste(mes, k)
    mes <- paste(mes, ". Unable to fit Q-function.", sep = "")
    stop(mes)
  }


  # getting the historic rewards
  U <- getElement(history, "U")
  # calculating the residual (fitted) values
  U_A <- apply(action_matrix(a = A, action_set = action_set) * U[, ..deterministic_rewards], MARGIN = 1, sum)
  U[, V_res := Q - U_bar - U_A]
  V_res <- U$V_res

  # fitting the (residual) Q-model
  q_model <- q_model(V_res = V_res, AH = AH)

  q_function <- list(
    q_model = q_model,
    AH_names = colnames(AH)
  )
  class(q_function) <- "Q_function"

  return(q_function)
}

#' @export
print.Q_function <- function(x){
  y <- x$q_model
  attr(y,"class") <- NULL

  print(y)
}

evaluate.Q_function <- function(object, new_history){
  q_model <- getElement(object, "q_model")
  action_set <- getElement(new_history, "action_set")
  deterministic_rewards <- getElement(new_history, "deterministic_rewards")

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  # getting the historic rewards
  U <- new_history$U

  # getting the residual predictions
  residual_q_predictions <- sapply(action_set, function(a) predict(q_model, new_AH = cbind(A = a, new_H)))
  # adding the historic utilities and deterministic rewards
  q_values <- U$U_bar + U[, ..deterministic_rewards] + residual_q_predictions
  names(q_values) <- paste("Q", action_set, sep = "_")

  if (!all(complete.cases(q_values))){
    stage <- unique(id_stage$stage)
    mes <- paste("Evaluation of the Q-function at stage ", stage, " have missing values.", sep = "")
    stop(mes)
  }

  # including the IDs and stage number
  q_values <- data.table(id_stage, q_values)
  setkey(q_values, id, stage)

  return(q_values)
}

q_step <- function(policy_data, k, full_history, Q, q_models){

  if (is.null(q_models))
    stop("Please provide q_models.")

  id <- get_id(policy_data)
  id_k <- get_id_stage(policy_data)[stage == k]$id
  idx_k <- (id %in% id_k)

  if (class(q_models)[[1]] == "list"){
    q_model <- q_models[[k]]
  } else{
    q_model <- q_models
  }

  # getting the Q-function history:
  q_history <- get_history(policy_data, stage = k, full_history = full_history)
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

q_step_cf <- function(folds, policy_data, k, full_history, Q, q_models, future_args){
  id <- get_id(policy_data)
  id_k <- get_id_stage(policy_data)[stage == k]$id
  idx_k <- (id %in% id_k)
  K <- policy_data$dim$K

  future_args <- append(future_args, list(X = folds,
                                          FUN = function(f){
                                            train_id <- id[-f]
                                            train_policy_data <- subset(policy_data, train_id)
                                            train_Q <- Q[-f]
                                            if (train_policy_data$dim$K != K) stop("The number of stages varies accross the training folds.")
                                            train_q_step <- q_step(train_policy_data, k = k, full_history = full_history, Q = train_Q, q_models = q_models)
                                            train_q_function <- train_q_step$q_function

                                            valid_id <- id[f]
                                            valid_policy_data <- subset(policy_data, valid_id)
                                            valid_history <- get_history(valid_policy_data, stage = k, full_history = full_history)
                                            valid_values <- evaluate(train_q_function, valid_history)

                                            out <- list(
                                              train_q_function = train_q_function,
                                              valid_values = valid_values
                                            )
                                            return(out)
                                          }))
  q_step_cf <- do.call(what = future.apply::future_lapply, future_args)
  q_step_cf <- simplify2array(q_step_cf)

  q_functions_cf <- q_step_cf["train_q_function", ]
  q_values <- q_step_cf["valid_values", ]

  q_values <- rbindlist(q_values)
  setkeyv(q_values, c("id", "stage"))

  out <- list(
    q_functions_cf = q_functions_cf,
    q_values = q_values,
    idx_k = idx_k
  )
  return(out)
}

#' Fit Q-functions
#'
#' \code{fit_Q_functions} is used to fit a list of Q-models
#' @noRd
#' @param policy_data Policy data object created by [policy_data()].
#' @param policy_actions Policy actions, see [policy_def].
#' @param q_models Outcome regression models/Q-models created by [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' @param full_history If TRUE, the full history is used to fit each Q-model. If FALSE, the single stage/"Markov type" history is used to fit each Q-model.
#' @examples
#' library("polle")
#' ### Simulating two-stage policy data
#' source(system.file("sim", "two_stage.R", package="polle"))
#' par0 <- c(gamma = 0.5, beta = 1)
#' d <- sim_two_stage(2e3, seed=1, par=par0)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # Defining a static policy
#' pl <- policy_def(static_policy(1), reuse = TRUE)
#'
#' # fitting a Q-model for each stage:
#' q_functions <- fit_Q_functions(policy_data = pd,
#'                                policy_actions = pl(pd),
#'                                q_models = list(q_glm(), q_glm()),
#'                                full_history = TRUE)
#' q_functions
fit_Q_functions <- function(policy_data,
                            policy_actions,
                            q_models,
                            full_history = FALSE){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)

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
    q_step_k <- q_step(
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
  names(q_functions) <- paste("stage_", 1:K, sep = "")
  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- full_history

  return(q_functions)
}
