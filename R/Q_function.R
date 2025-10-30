fit_Q_function <- function(history, Q, q_model) {
  action_set <- getElement(history, "action_set")
  stage_action_set <- getElement(history, "stage_action_set")
  stage <- getElement(history, "stage")
  deterministic_rewards <- getElement(history, "deterministic_rewards")

  # getting the action (A) and the model matrix (H):
  A <- get_A(history)
  H <- get_H(history)
  AH <- cbind(A, H)

  # checking that the dimensions fit:
  if (nrow(AH) != length(Q))
    stop("Unable to fit Q-function.")

  # checking that all actions in the stage action set occur:
  if (!all(stage_action_set == sort(unique(A)))) {
    mes <- "Not all stage actions occur at stage"
    mes <- paste(mes, paste(stage, collapse = ", "))
    mes <- paste(mes, ". Unable to fit Q-function.", sep = "")
    stop(mes)
  }

  ## getting the historic rewards
  U <- getElement(history, "U")
  ## calculating the residual (fitted) values
  U_A <- apply(
    action_matrix(a = A, action_set = action_set) * U[, deterministic_rewards, with = FALSE],
    MARGIN = 1,
    sum
  )
  V_res <- unlist(Q - U[, "U_bar"] - U_A)

  ## removing missing outcomes (censored/coarsened)
  missing_ <- is.na(Q)
  V_res <- V_res[missing_ == FALSE]
  AH <- AH[missing_ == FALSE, ]

  ## fitting the (residual) Q-model
  q_model <- q_model(V_res = V_res, AH = AH)

  q_function <- list(
    q_model = q_model,
    AH_names = colnames(AH),
    action_set = action_set,
    stage_action_set = stage_action_set,
    stage = stage
  )
  class(q_function) <- "Q_function"

  return(q_function)
}

#' @export
print.Q_function <- function(x, ...) {
  y <- x$q_model
  attr(y, "class") <- NULL
  print(y)
}

#' @export
predict.Q_function <- function(object, new_history, ...) {
  q_model <- getElement(object, "q_model")
  AH_names <- getElement(object, "AH_names")
  ## action set of the new history object
  action_set <- getElement(new_history, "action_set")
  ## stage action set of the fitted Q-function
  stage_action_set <- getElement(object, "stage_action_set")
  stage <- getElement(object, "stage")
  ## deterministic rewards of the new history object
  deterministic_rewards <- getElement(new_history, "deterministic_rewards")

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)
  if (ncol(new_H) == 0) {
    ## preventing empty data.table
    new_H <- data.table(dummy = rep(as.numeric(NA), nrow(id_stage)))
  }

  new_AH_names <- c("A", names(new_H))

  ## checks
  if(!all(stage_action_set %in% action_set))
    stop("The fitted stage action set is not a subset of the new action set.")
  if (!all(AH_names %in% new_AH_names))
    stop("new_history does not contain the same column names as the original history.")

  ## getting the historic rewards
  U <- new_history$U

  ## getting the residual predictions for the stage action set
  residual_q_predictions <- sapply(
    stage_action_set,
    function(a) predict(q_model, new_AH = cbind(A = a, new_H))
  )
  tmp <- matrix(data = NA, nrow = nrow(id_stage), ncol = length(action_set))
  colnames(tmp) <- action_set
  tmp[, stage_action_set] <- residual_q_predictions
  q_values <- tmp; rm(tmp)

  # adding the historic utilities and deterministic rewards
  q_values <- U$U_bar + U[, deterministic_rewards, with = FALSE] + q_values
  colnames(q_values) <- paste("Q", action_set, sep = "_")

  if (!all(complete.cases(q_values[, action_set %in% stage_action_set, with = FALSE]))){
    if(!is.null(stage)){
      mes <- paste("The Q-function predictions at stage ",
                   stage,
                   " have missing values.",
                   sep = "")
    } else {
      mes <- "The Q-function predictions have missing values."
    }
    stop(mes)
  }

  # including the IDs and stage number
  q_values <- data.table(id_stage, q_values)
  setkeyv(q_values, c("id", "stage"))

  return(q_values)
}

q_step <- function(policy_data, k, full_history, Q, q_models) {

  if (is.null(q_models))
    stop("Please provide q_models.")

  stage <- NULL
  id <- get_id(policy_data)
  id_k <- get_id_stage(policy_data)[stage == k][["id"]]
  idx_k <- (id %in% id_k)
  rm(stage)

  if (is.list(q_models)){
    q_model <- q_models[[k]]
  } else{
    q_model <- q_models
  }

  ## getting the Q-function history for each action event:
  q_history <- get_history(policy_data,
                           stage = k,
                           full_history = full_history)
  ## fitting the Q-function:
  q_function <- fit_Q_function(q_history, Q = Q[idx_k], q_model = q_model)
  ## getting the Q-function values for each action and right-censoring event:
  q_history <- get_history(policy_data,
                           stage = k,
                           full_history = full_history,
                           event_set = c(0,2))
  q_values <- predict(q_function, new_history = q_history)
  ## getting the ID-index for each action and right-censoring event:
  idx_k <- (id %in% q_values[["id"]])

  out <- list(
    q_function = q_function,
    q_values = q_values,
    idx_k = idx_k
  )
  return(out)
}

q_step_cf <- function(folds,
                      policy_data,
                      k,
                      full_history,
                      Q,
                      q_models,
                      save_cross_fit_models,
                      future_args) {
  id <- get_id(policy_data)
  K <- get_K(policy_data)

  future_args <- append(
    future_args,
    list(
      X = folds,
      FUN = function(f) {
        train_id <- id[-f]
        train_policy_data <- subset_id(policy_data, train_id)
        train_Q <- Q[-f]
        if (train_policy_data$dim$K != K) {
          stop("The number of stages varies accross the training folds.")
        }
        train_q_step <- q_step(train_policy_data,
          k = k,
          full_history = full_history,
          Q = train_Q,
          q_models = q_models
        )
        train_q_function <- train_q_step$q_function

        valid_id <- id[f]
        valid_policy_data <- subset_id(policy_data, valid_id)
        valid_history <- get_history(valid_policy_data,
                                     stage = k,
                                     full_history = full_history,
                                     event_set = c(0,2))
        valid_values <- predict(train_q_function, valid_history)

        if (save_cross_fit_models == FALSE) {
          train_q_function <- NULL
        }

        out <- list(
          train_q_function = train_q_function,
          valid_values = valid_values
        )
        return(out)
      }
    )
  )
  q_step_cf <- do.call(what = future.apply::future_lapply, future_args)
  q_step_cf <- simplify2array(q_step_cf)

  q_functions_cf <- q_step_cf["train_q_function", ]
  q_values <- q_step_cf["valid_values", ]

  q_values <- rbindlist(q_values)
  setkeyv(q_values, c("id", "stage"))

  ## getting the ID-index for each Q-value
  ## (action and right-censoring events):
  idx_k <- (id %in% q_values[["id"]])

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
#' @param m_function Function of class m_function to impute missing outcomes.
#' @examples
#' library("polle")
#' ### Simulating two-stage policy data
#' d <- sim_two_stage(2e3, seed = 1)
#' pd <- policy_data(d,
#'   action = c("A_1", "A_2"),
#'   covariates = list(
#'     L = c("L_1", "L_2"),
#'     C = c("C_1", "C_2")
#'   ),
#'   utility = c("U_1", "U_2", "U_3")
#' )
#' pd
#'
#' # Defining a static policy (A=1)
#' pl <- policy_def(1, reuse = TRUE)
#'
#' # fitting a Q-model for each stage:
#' q_functions <- fit_Q_functions(
#'   policy_data = pd,
#'   policy_actions = pl(pd),
#'   q_models = list(q_glm(), q_glm()),
#'   full_history = TRUE
#' )
#' q_functions
fit_Q_functions <- function(policy_data,
                            policy_actions,
                            q_models,
                            full_history = FALSE,
                            m_function = NULL) {
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)

  # input checks:
  if (is.null(q_models)) {
    stop("Please provide q_models.")
  }

  mes <- "q_models must be a single q_models or a list of K q_models's."
  if (is.list(q_models)) {
    tmp <- all(unlist(lapply(q_models, function(qm) inherits(qm, "q_model"))))
    if (!tmp) {
      stop(mes)
    }
    rm(tmp)
    if (length(q_models) != K) {
      stop(mes)
    }
  } else {
    if (!inherits(q_models, "q_model")) {
      stop(mes)
    }
    if (full_history == TRUE) {
      stop("full_history must be FALSE when a single q-model is provided.")
    }
  }

  # getting the IDs:
  id <- get_id(policy_data)

  ## (n X K+1) matrix with entries
  ## k-column = Q_k(H_{k,i}, d_k(H_{k,i})), k = 1,...,K
  ## K+1-column = U (if no missing final outcomes) or Q_{K+1}(H_{K+1})
  Q <- matrix(nrow = n, ncol = K + 1)
  if (is.null(m_function) == TRUE){
    ## getting the observed (complete) utility:
    utility <- get_utility(policy_data)
    ## (n) vector with entries U_i:
    U <- utility$U
    Q[, K + 1] <- U
  } else {
    Q_K1 <- predict(m_function, new_policy_data = policy_data)
    Q[(id %in% Q_K1[["id"]]), K + 1] <- Q_K1[["Q"]]
  }

  # fitting the Q-functions:
  q_functions <- list()
  for (k in K:1) {
    q_step_k <- q_step(
      policy_data = policy_data,
      k = k,
      full_history = full_history,
      Q = Q[, k + 1],
      q_models = q_models
    )
    # getting the Q-function, Q-function values and the ID-index
    q_functions[[k]] <- q_step_k$q_function
    q_values_k <- q_step_k$q_values
    idx_k <- q_step_k$idx_k

    # getting the Q-function values under the policy for each right-censoring and action event:
    d_k <- merge(q_values_k, policy_actions)[["d"]]
    q_d_values_k <- get_a_values(a = d_k, action_set = action_set, values = q_values_k)$P

    # inserting the Q-function values under the policy in Q:
    Q[idx_k, k] <- q_d_values_k
    Q[!idx_k, k] <- Q[!idx_k, k + 1]
  }

  # setting names, classes and attributes:
  names(q_functions) <- paste("stage_", 1:K, sep = "")
  class(q_functions) <- c("q_functions", "nuisance_functions")
  attr(q_functions, "full_history") <- full_history

  return(q_functions)
}

#' @title Get Q-functions
#'
#' @description \code{get_q_functions()} returns a list of (fitted) Q-functions
#' associated with each stage.
#' @param object Object of class [policy_eval] or [policy_object].
#' @returns List of class [nuisance_functions].
#' @seealso [predict.nuisance_functions]
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed = 1)
#' pd <- policy_data(d,
#'   action = c("A_1", "A_2"),
#'   baseline = c("B"),
#'   covariates = list(
#'     L = c("L_1", "L_2"),
#'     C = c("C_1", "C_2")
#'   ),
#'   utility = c("U_1", "U_2", "U_3")
#' )
#' pd
#'
#' # evaluating the static policy a=1 using outcome regression
#' # based on a GLM model at each stage.
#' pe <- policy_eval(
#'   type = "or",
#'   policy_data = pd,
#'   policy = policy_def(1, reuse = TRUE, name = "A=1"),
#'   q_models = list(q_glm(), q_glm())
#' )
#' pe
#'
#' # getting the Q-functions
#' q_functions <- get_q_functions(pe)
#'
#' # getting the fitted g-function values
#' head(predict(q_functions, pd))
#' @export
get_q_functions <- function(object) {
  UseMethod("get_q_functions")
}
