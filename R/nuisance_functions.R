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
#' \describe{
#'   \item{\code{predict}}{ Predict the values of the g- or Q-functions based
#'   on a [policy_data] object.}
#'  }
#'
#' @examples
#' ### Two stages:
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
#' @param ... Additional arguments.
#' @returns [data.table] with keys \code{id} and \code{stage} and variables \code{g_a} or \code{Q_a} for
#' each action a in the actions set.
#' @examples
#' library("polle")
#' ### Single stage:
#' d <- sim_single_stage(5e2, seed=1)
#' pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd
#' # defining a static policy (A=1):
#' pl <- policy_def(1, name = "A=1")
#'
#' # doubly robust evaluation of the policy:
#' pe <- policy_eval(policy_data = pd,
#'                   policy = pl,
#'                   g_models = g_glm(),
#'                   q_models = q_glm())
#' # summarizing the estimated value of the policy:
#' pe
#'
#' # getting the fitted g-function values:
#' head(predict(get_g_functions(pe), pd))
#'
#' # getting the fitted Q-function values:
#' head(predict(get_q_functions(pe), pd))
#' @export
predict.nuisance_functions <- function(object, new_policy_data, ...){
  K <- get_K(new_policy_data)
  action_set <- get_action_set(new_policy_data)
  full_history <- attr(object, "full_history")

  if (length(object) == K){
    history <- lapply(1:K, function(s) get_history(new_policy_data,
                                                   stage = s,
                                                   full_history = full_history))
    values <- mapply(history,
                     object,
                     FUN = function(h, f) predict(f, h),
                     SIMPLIFY = FALSE)
    values <- rbindlist(values)
    setkeyv(values, c("id", "stage"))
  } else if (length(object) == 1){
    history <- state_history(new_policy_data)
    values <- predict(object[[1]], history)
  } else{
    stop("Provide either 1 or K nuisance functions for evaluation.")
  }

  return(values)
}

#' @export
print.nuisance_functions <- function(x, ...){
  attr(x, "class") <- NULL
  print(x)
}

