#' @title Nuisance Functions
#' @name nuisance_functions
#' @docType class
#'
#' @description The fitted g-functions and Q-functions are stored in an object
#' of class "nuisance_functions". The object is a list with a fitted model
#' object for every stage. Information on whether the full history or the
#' state/Markov-type history is stored as an attribute ("full_history").
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class
#' \code{nuisance_functions}:
#' \itemize{
#'   \item{\code{predict}}{ Predict the values of the g- or Q-functions based
#'   on a [policy_data] object.}
#'  }
#'
#' @examples
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # evaluating the static policy a=1:
#' pe <- policy_eval(policy_data = pd,
#'                   policy = policy_def(1, reuse = TRUE),
#'                   g_models = g_glm(),
#'                   q_models = q_glm())
#'
#' # getting the fitted g-functions:
#' (g_functions <- get_g_functions(pe))
#'
#' # getting the fitted Q-functions:
#' (q_functions <- get_q_functions(pe))
#'
#' # getting the fitted values:
#' head(predict(g_functions, pd))
#' head(predict(q_functions, pd))
NULL

#' Predict g-functions and Q-functions
#'
#' \code{predict()} returns the fitted values of the g-functions and
#' Q-functions when applied to a (new) policy data object.
#' @param object Object of class "nuisance_functions". Either \code{g_functions}
#' or \code{q_functions} as returned by [policy_eval()] or [policy_learn()].
#' @param new_policy_data Policy data object created by [policy_data()].
#' @param ... Additional arguments passed to lower level functions.
#' @returns [data.table] with keys \code{id} and \code{stage} and variables \code{g_a} or \code{Q_a} for
#' each action a in the actions set.
#' @examples
#' library("polle")
#' ### Single stage:
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#' # defining a static policy (A=1):
#' pl1 <- policy_def(1, name = "A=1")
#'
#' # doubly robust evaluation of the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'             policy = pl1,
#'             g_models = g_glm(),
#'             q_models = q_glm())
#' # summarizing the estimated value of the policy:
#' pe1
#'
#' # getting the fitted g-function values:
#' head(predict(get_g_functions(pe1), pd1))
#'
#' # getting the fitted Q-function values:
#' head(predict(get_q_functions(pe1), pd1))
#' @export
predict.nuisance_functions <- function(object, new_policy_data, ...){
  evaluate(object, new_policy_data)
}

evaluate <- function(object, ...)
  UseMethod("evaluate")

evaluate.nuisance_functions <- function(object, policy_data){
  K <- get_K(policy_data)
  action_set <- get_action_set(policy_data)
  full_history <- attr(object, "full_history")

  if (length(object) == K){
    history <- lapply(1:K, function(s) get_history(policy_data, stage = s, full_history = full_history))
    values <- mapply(history, object, FUN = function(h, f) evaluate(f, new_history = h), SIMPLIFY = FALSE)
    values <- rbindlist(values)
    setkeyv(values, c("id", "stage"))
  } else if (length(object) == 1){
    history <- state_history(policy_data)
    values <- evaluate(object[[1]], new_history = history)
  } else{
    stop("Provide either 1 or K nuisance functions for evaluation.")
  }

  return(values)
}
