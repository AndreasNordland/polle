#' @export
evaluate <- function(object, ...)
  UseMethod("evaluate")

#' Evaluate g- and Q-functions
#'
#' \code{evaluate} returns the fitted values of g-functions and
#' Q-functions when applied to a (new) policy data object.
#' @param object Object of class "nuisance_functions". Either \code{g_functions}
#' or \code{q_functions} as returned by [policy_eval()] or [policy_learn()].
#' @param policy_data Policy data object created by [policy_data()].
#' @examples
#' library("polle")
#' ### Single stage:
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#' # defining a static policy:
#' pl1 <- policy_def(static_policy(1))
#' # doubly robust evaluation of the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'             policy = pl1,
#'             g_models = g_glm(),
#'             q_models = q_glm())
#' # summarising the estimated value of the policy:
#' pe1
#' #
#' pe1$g_functions
#' # evaluating the fitted g-function:
#' head(evaluate(pe1$g_functions, pd1))
#' @export
evaluate.nuisance_functions <- function(object, policy_data){
  K <- get_K(policy_data)
  action_set <- get_action_set(policy_data)
  full_history <- attr(object, "full_history")

  if (length(object) == K){
    history <- lapply(1:K, function(s) get_history(policy_data, stage = s, full_history = full_history))
    values <- mapply(history, object, FUN = function(h, f) evaluate(f, new_history = h), SIMPLIFY = FALSE)
    values <- rbindlist(values)
    setkey(values, id, stage)
  } else if (length(object) == 1){
    history <- state_history(policy_data)
    values <- evaluate(object[[1]], new_history = history)
  } else{
    stop("Provide either 1 or K nuisance functions for evaluation.")
  }

  return(values)
}
