#' Fit Single g-function
#'
#' \code{fit_g_function} is used to fit a single g-model.
#' @param history History object created by [get_history()]
#' @param g_model Propensity model/g-model created by [g_glm()], [g_rf()], [g_sl()] or similar functions.
#' @returns Object of class "g_function".
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
#' history <- get_history(pd, stage = 1)
#' head(history$H)
#'
#' g_function <- fit_g_function(
#'   history = get_history(pd, stage = 1),
#'   g_model = g_glm()
#' )
#' g_function
#' @noRd
fit_g_function <- function(history, g_model){
  action_set <- getElement(history, "action_set")

  # getting the action (A) and the model matrix (H):
  A <- get_A(history)
  H <- get_H(history)
  stage <- unique(get_id_stage(history)[, "stage"])

  # checking that all actions in the actions set occur:
  if (!all(action_set == sort(unique(A)))){
    mes <- "Not all actions occur at stage"
    mes <- paste(mes, paste(stage, collapse = ", "))
    mes <- paste(mes, ". Unable to fit g-function.", sep = "")
    stop(mes)
  }

  # fitting the model:
  g_model <- g_model(A = A, H = H, action_set = action_set)

  g_function <- list(
    g_model = g_model,
    H_names = colnames(H),
    action_set = action_set
  )
  class(g_function) <- "g_function"

  return(g_function)
}

#' @export
print.g_function <- function(x, ...){
  y <- x$g_model
  y$action_set <- NULL
  attr(y,"class") <- NULL

  print(y)
}

evaluate.g_function <- function(object, new_history){
  g_model <- object$g_model
  H_names <- object$H_names
  action_set <- object$action_set

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  if (!all(names(new_H) %in% H_names))
    stop("new_history does not have the same column names as the original history.")

  g_values <- predict(g_model, new_H = new_H)
  if (!all(g_values >= 0) |
      !all(g_values <= 1)){
    stop("Not all g-model values are within [0,1].")
  }

  colnames(g_values) <- paste("g", action_set, sep = "_")

  if (!all(complete.cases(g_values))){
    stage <- unique(new_history$H$stage)
    mes <- paste("Evaluation of the g-function at stage ", stage, " have missing values.", sep = "")
    stop(mes)
  }

  # including the id's
  g_values <- data.table(id_stage, g_values)
  setkeyv(g_values, c("id", "stage"))

  return(g_values)
}

#' Fit g-functions
#'
#' \code{fit_g_functions} is used to fit a list of g-models
#' @param policy_data Policy data object created by [policy_data()].
#' @param g_models Propensity models/g-models created by [g_glm()], [g_rf()], [g_sl()] or similar functions.
#' @param full_history If TRUE, the full history is used to fit each g-model. If FALSE, the single stage/"Markov type" history is used to fit each g-model.
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
#' # fitting a single g-model across all stages:
#' g_functions <- fit_g_functions(policy_data = pd,
#'                                g_models = g_glm(),
#'                                full_history = FALSE)
#' g_functions
#'
#' # fitting a g-model for each stage:
#' g_functions <- fit_g_functions(policy_data = pd,
#'                                g_models = list(g_glm(), g_glm()),
#'                                full_history = TRUE)
#' g_functions
#' @noRd
fit_g_functions <- function(policy_data, g_models, full_history){
  K <- get_K(policy_data)

  if (is.null(g_models))
    stop("Please provide g_models.")

  # checking the g_models:
  if (is.list(g_models)){
    if (length(g_models) != K)
      stop("g_models must either be a list of length K or a single g-model.")
  } else{
    if (full_history == TRUE)
      stop("full_history must be FALSE when a single g-model is provided.")
  }

  if (is.list(g_models)){
    history <- lapply(1:K,
                      function(s) get_history(policy_data,
                                              stage = s,
                                              full_history = full_history))
    g_functions <- mapply(history,
                          g_models,
                          FUN = function(h, gm) fit_g_function(history = h,
                                                               g_model = gm),
                          SIMPLIFY = FALSE)
    names(g_functions) <- paste("stage_", 1:K, sep = "")
  } else{
    history <- state_history(policy_data)
    g_functions <- list(all_stages = fit_g_function(history, g_models))
  }

  class(g_functions) <- c("g_functions", "nuisance_functions")
  attr(g_functions, "full_history") <- full_history

  return(g_functions)
}

#' Cross-fit g-functions
#'
#' \code{fit_g_functions_cf} is used to cross-fit a list of g-models
#'
#' @param folds List of vectors of indices for each validation fold, see examples.
#' @param policy_data Policy data object created by [policy_data()].
#' @param g_models Propensity models/g-models created by [g_glm()], [g_rf()], [g_sl()] or similar functions.
#' @param full_history If TRUE, the full history is used to fit each g-model. If FALSE, the single stage/"Markov type" history is used to fit each g-model.
#' @param future_args arguments passed to [future.apply::future_lapply].
#' @examples
#' #' library("polle")
#' ### Two stage:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(2e3, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # creating 2 folds (indices for each validation fold):
#' folds <- split(sample(1:get_n(pd), get_n(pd)), rep(1:2, length.out = get_n(pd)))
#'
#' # fitting a single g-model across all stages for each fold:
#' g_functions <- fit_g_functions_cf(folds = folds,
#'                                   policy_data = pd,
#'                                   g_models = g_glm(),
#'                                   full_history = FALSE)
#' g_functions
#' # fitting a g-model for each stage for each fold (in parallel):
#' future::plan("multisession")
#' g_functions <- fit_g_functions_cf(folds = folds,
#'                                   policy_data = pd,
#'                                   g_models = list(g_glm(), g_glm()),
#'                                   full_history = TRUE)
#' future::plan("sequential")
#' g_functions$functions
#' @noRd
fit_g_functions_cf <- function(folds,
                               policy_data,
                               g_models,
                               full_history,
                               future_args = list(future.seed = TRUE)){
  id <- get_id(policy_data)
  K <- policy_data$dim$K

  future_args <- append(future_args, list(X = folds,
                                          FUN = function(f){
                                            train_id <- id[-f]
                                            train_policy_data <- subset(policy_data, train_id)
                                            if (train_policy_data$dim$K != K) stop("The number of stages K varies across the training policy data folds.")
                                            train_g_functions <- fit_g_functions(policy_data = train_policy_data, g_models = g_models, full_history = full_history)

                                            valid_id <- id[f]
                                            valid_policy_data <- subset(policy_data, valid_id)
                                            valid_g_values <- evaluate(train_g_functions, valid_policy_data)

                                            list(
                                              train_g_functions = train_g_functions,
                                              valid_g_values = valid_g_values
                                            )
                                          }))

  fit_cf <- do.call(what = future.apply::future_lapply, future_args)
  fit_cf <- simplify2array(fit_cf)

  functions <- fit_cf["train_g_functions", ]
  values <- fit_cf["valid_g_values", ]

  values <- rbindlist(values)
  setkeyv(values, c("id", "stage"))

  out <- list(
    functions = functions,
    values = values
  )
  return(out)
}

#' @title Get g-functions
#'
#' @description \code{get_g_functions()} returns a list of (fitted) g-functions
#' associated with each stage.
#' @param object Object of class [policy_eval] or [policy_object].
#' @returns List of class [nuisance_functions].
#' @seealso [predict.nuisance_functions]
#' @examples
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d2 <- sim_two_stage(5e2, seed=1)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#'
#' # evaluating the static policy a=1 using inverse propensity weighting
#' # based on a GLM model at each stage
#' pe2 <- policy_eval(type = "ipw",
#'                    policy_data = pd2,
#'                    policy = policy_def(1, reuse = TRUE, name = "A=1"),
#'                    g_models = list(g_glm(), g_glm()))
#' pe2
#'
#' # getting the g-functions
#' g_functions <- get_g_functions(pe2)
#' g_functions
#'
#' # getting the fitted g-function values
#' head(predict(g_functions, pd2))
#' @export
get_g_functions <- function(object)
  UseMethod("get_g_functions")




