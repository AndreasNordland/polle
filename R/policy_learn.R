#' Create Policy Learner Object:
#'
#' \code{policy_learn} is used to specify a policy learning method (Q-learning,
#' V-restricted (doubly robust) Q-learning and V-restricted policy tree
#' learning). Evaluating the policy learner returns a policy object.
#' @param type Type of policy learner method:
#' \itemize{
#'   \item{} \code{"rql"}: Realistic Quality/Q-learning.
#'   \item{} \code{"rqvl"}: Realistic V-restricted (doubly robust) Q-learning.
#'   \item{} \code{"ptl"}: Policy Tree Learning.
#' }
#' @param alpha Probability threshold for determining realistic actions.
#' @param L (only used if \code{type = "rqvl"} or \code{type = "ptl"}) Number of folds for
#' cross-fitting.
#' @param save_cross_fit_models If \code{TRUE}, the cross-fitted models will be saved.
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @param qv_models (only used if \code{type = "rqvl"}) V-restricted Q-models created
#' by [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' @param qv_full_history (only used if \code{type = "rqvl"}) If \code{TRUE}, the full
#' history is used to fit each QV-model. If FALSE, the single stage/
#' "Markov type" history is used to fit each QV-model.
#' @param policy_vars (only used if \code{type = "ptl"}) Character vector/string or
#' list of character vectors/strings. Variable names used to construct a
#' V-restricted policy tree. The names must be a subset of the history variable
#' names, see [get_history_names()].
#' @param policy_full_history (only used if \code{type = "ptl"}) If \code{TRUE}, the full
#' history is parsed to policy_tree. If FALSE, the single stage/"Markov type"
#' history is parsed to policy_tree.
#' @param depth (only used if \code{type = "ptl"}) The depth of the fitted policy
#' tree, see [policy_tree()].
#' @param split.step (only used if \code{type = "ptl"}) The number of possible splits
#' to consider when performing policy tree search, see [policy_tree()].
#' @param min.node.size (only used if \code{type = "ptl"}) An integer indicating the
#' smallest terminal node size permitted, see [policy_tree()].
#' @param hybrid (only used if \code{type = "ptl"}) If \code{TRUE}, [hybrid_policy_tree()] is
#' used to fit a policy tree.
#' @param search.depth (only used if \code{type = "ptl"} and \code{hybrid = TRUE}) Depth to
#' look ahead when splitting.
#' @returns Function of inherited class \code{"policy_learner"}.
#' Evaluating the function on a [policy_data] object returns an object of
#' class \code{"policy_object"}. A policy object is a list containing all or
#' some of the following elements:
#' \item{\code{q_functions}}{Fitted Q-functions. Object of class "nuisance_functions".}
#' \item{\code{g_functions}}{Fitted g-functions. Object of class "nuisance_functions".}
#' \item{\code{action_set}}{Sorted character vector describing the action set, i.e.,
#'                          the possible actions at each stage.}
#' \item{\code{alpha}}{Numeric. Probability threshold to determine realistic actions.}
#' \item{\code{K}}{Integer. Maximal number of stages.}
#' \item{\code{qv_functions}}{(only if \code{type = "rqvl"}) Fitted V-restricted
#' Q-functions. Contains a fitted model for each stage and action.}
#' \item{\code{ptl_objects}}{(only if \code{type = "ptl"}) Fitted V-restricted
#' policy trees. Contains a [policy_tree] for each stage.}
#' \item{\code{ptl_designs}}{(only if \code{type = "ptl"}) Specification of the
#' V-restricted design matrix for each stage}
#' @seealso [policy_def()], [get_policy_functions()].
#' @examples
#' library("polle")
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' par0 <- c(gamma = 0.5, beta = 1)
#' d <- sim_two_stage(2e3, seed=1, par=par0)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("BB"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' ### Q-learning
#' # specifying the learner:
#' pl <- policy_learn(type = "rql")
#' pl
#'
#' # the policy learner can be used directly:
#' po <- pl(pd, q_models = q_glm())
#' po
#' head(get_policy(po)(pd))
#'
#' # or the policy learner can be evaluated:
#' pe <- policy_eval(policy_data = pd,
#'             policy_learn = pl,
#'             q_models = q_glm())
#' pe
#' pe$policy_object; rm(pl, po, pe)
#'
#' ### V-restricted (Doubly Robust) Q-learning
#'
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    qv_models = q_glm(formula = ~ C))
#'
#' # evaluating the learned policy
#' pe <- policy_eval(policy_data = pd,
#'             policy_learn = pl,
#'             q_models = q_glm(),
#'             g_models = g_glm())
#' pe
#' pe$policy_object
#' pe$policy_object$qv_functions$stage_1
#' head(get_policy(pe$policy_object)(pd)); rm(pl, pe)
#'
#' ### V-restricted Policy Tree Learning
#'
#' # specifying the learner:
#' pl <- policy_learn(type = "ptl",
#'                    policy_vars = list(c("C_1", "BB"),
#'                                       c("L_1", "BB")),
#'                    policy_full_history = TRUE)
#'
#' # evaluating the learned policy:
#' set.seed(1)
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#' pe
#' pe$policy_object$ptl_objects
#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         L = 1,
                         save_cross_fit_models = FALSE,
                         future_args = list(future.seed = TRUE),
                         qv_models = NULL,
                         qv_full_history = FALSE,
                         policy_vars = NULL,
                         policy_full_history = FALSE,
                         depth = 2,
                         split.step = 1,
                         min.node.size = 1,
                         hybrid = FALSE,
                         search.depth = 2){
  type <- tolower(type)

  fm <- formals()
  cl <- match.call()
  for (i in setdiff(names(fm), names(cl)))
    cl[i] <- list(fm[[i]])

  pl_args <- as.list(cl)[-1]
  pl_args[["type"]] <- NULL

  if (type %in% c("rql", "ql", "q_learning", "q-learning")) {
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){
      fm <- formals()
      cl <- match.call()
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      rql_args <- append(pl_args, eval_args)

      do.call(what = "rql", rql_args)
    }
  }
  else if (type %in% c("rqvl", "qvl", "qv_learning", "qv-learning")) {
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){
      fm <- formals()
      cl <- match.call()
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      rqvl_args <- append(pl_args, eval_args)

      do.call(what = "rqvl", rqvl_args)
    }
  } else if (type %in% c("ptl", "policytree", "policy_tree")){
    if (!require("policytree")) {
      stop("The policytree package is required to perform value searching using trees.")
    }
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){

      fm <- formals()
      cl <- match.call()
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      ptl_args <- append(pl_args, eval_args)

      do.call(what = "ptl", ptl_args)
    }
  } else{
    stop("Unknown type of policy learner. Use 'rql', 'rqvl' or 'ptl'")
  }
  class(pl) <- c("policy_learner", "function")
  attr(pl, "type") <- type
  attr(pl, "pl_args") <- pl_args

  return(pl)
}

#' @rdname policy_learn
#' @export
print.policy_object <- function(x){
  cat("Policy object with list elements:")
  cat("\n")
  cp <- paste(names(x), collapse = ", ")
  cat(cp)
  cat("\n")
  cat("Use 'get_policy' to get the associated policy.")
}


#' @rdname policy_learn
#' @export
print.policy_learner <- function(x) {

  cat("Policy learner with arguments:")

  cp <- args(x)
  cp <- capture.output(print.function(cp))
  cp <- capture.output(print.function(x))
  cp <- paste(cp, collapse = "")
  cp <- gsub(" ", "", cp, fixed = TRUE)
  cp <- gsub(",", ", ", cp, fixed = TRUE)
  n_start <- 10
  n_end <- gregexpr(")", cp)[[1]][1]
  cp <- substr(cp,n_start, n_end-1)

  cat("\n")
  cat(cp)

}

#' @rdname policy_learn
#' @export
get_policy <- function(object){
  UseMethod("get_policy")
}

#' @rdname policy_learn
#' @export
get_policy.policy_eval <- function(object){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy(po)
  return(pf)
}

#' Get Policy Functions
#'
#' \code{get_policy_functions()} returns a function defining the policy at
#' the given stage. \code{get_policy_functions()} is useful when implementing
#' the learned policy.
#'
#' @param object Object of class "policy_object" or "policy_eval",
#' see [policy_learn] and [policy_eval].
#' @param stage Integer. Stage number.
#' @returns Functions with arguments:
#' \item{\code{H}}{[data.table] containing the variables needed to evaluate the policy (and g-function).}
#' @examples
#' library("polle")
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(2e3, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = "BB",
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' ### Realistic V-restricted (Doubly Robust) Q-learning
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    qv_full_history = TRUE,
#'                    qv_models = list(q_glm(formula = ~ C_1),
#'                                     q_glm(formula = ~ L_2)),
#'                    alpha = 0.05)
#' # applying the learner:
#' po <- pl(policy_data = pd,
#'          q_models = q_glm(),
#'          g_models = g_glm())
#' po
#'
#' # getting the policy function at stage 2:
#' pf2 <- get_policy_functions(po, stage = 2)
#' args(pf2)
#'
#' # applying the policy function to new data:
#' set.seed(1)
#' L_2 <- rnorm(n = 10)
#' new_H <- data.table(BB = "group1",
#'                     C = rnorm(n = 10),
#'                     L = L_2,
#'                     L_2 = L_2)
#' d2 <- pf2(H = new_H)
#' d2
#'
#' # comparing get_policy_functions() and get_policy() when
#' # used on an object of class "policy_object":
#' new_H <- get_history(pd, stage = 2, full_history = TRUE)$H
#' new_H$L <- new_H$L_2
#' new_H$C <- new_H$C_2
#' all.equal(
#'  pf2(H = new_H),
#'  get_policy(po)(pd)[stage == 2]$d
#' )
#' rm(pl, po, d2, pf2, new_H, L_2)
#'
#' ### Realistic V-restricted Policy Tree Learning
#' # specifying the learner:
#' pl <- policy_learn(type = "ptl",
#'                    policy_vars = list(c("C_1", "BB"),
#'                                       c("L_1", "BB")),
#'                    policy_full_history = TRUE,
#'                    alpha = 0.05)
#'
#' # evaluating the learner:
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#'
#' #' # getting the policy function at stage 2:
#' pf2 <- get_policy_functions(pe, stage = 2)
#' args(pf2)
#'
#' # applying the policy function to new data:
#' set.seed(1)
#' L_1 <- rnorm(n = 10)
#' new_H <- data.table(C = rnorm(n = 10),
#'                     L = L_1,
#'                     L_1 = L_1,
#'                     BB = "group1")
#' d2 <- pf2(H = new_H)
#' d2
#'
#' # comparing get_policy_functions() and get_policy() when
#' # used on an object of class "policy_eval":
#' new_H <- get_history(pd, stage = 2, full_history = TRUE)$H
#' new_H$L <- new_H$L_2
#' new_H$C <- new_H$C_2
#' all.equal(
#'  pf2(H = new_H),
#'  get_policy(pe)(pd)[stage == 2]$d
#' )
#' @export
get_policy_functions <- function(object, stage){
  UseMethod("get_policy_functions")
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.policy_eval <- function(object, stage){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy_functions(po, stage = stage)
  return(pf)
}


