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
#' Evaluating the function returns an object of class \code{"policy_object"}.
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
#' pl <- policy_learn(type = "ptl", policy_vars = c("C", "BB"))
#'
#' # evaluating the learned policy:
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#' pe
#' pe$policy_object$ptl_objects
#'
#' ### Cross-fitted Policy Learning
#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         L = 1,
                         save_cross_fit_models = FALSE,
                         future_args = NULL,
                         qv_models = NULL,
                         qv_full_history = FALSE,
                         policy_vars = NULL, # note that the order of the policy_vars dictates the form of X in policy_tree
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

#' Get Policy Functions
#'
#' \code{get_policy_functions()}
#'
#' @param object Object of class "policy_object", see [policy_learn].
#' @param stage Integer. Stage number.
#' @returns Functions with arguments:
#' \item{\code{H}}{[data.table]}
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
#' ### V-restricted (Doubly Robust) Q-learning
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    qv_models = q_glm(formula = ~ C))
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
#' d2 <- pf2(H = data.table(C = rnorm(n = 10)))
#' d2
#'
#' # comparing get_policy_functions() and get_policy():
#' all.equal(
#'  pf2(H = get_history(pd, stage = 2)$H),
#'  get_policy(po)(pd)[stage == 2]$d
#' )
#' rm(pl, po, d2)
#'
#' ### Realistic V-restricted Policy Tree Learning
#' # specifying the learner:
#' pl <- policy_learn(type = "ptl",
#'                    policy_vars = c("C", "BB"),
#'                    alpha = 0.05)
#'
#' # applying the learner:
#' po <- pl(policy_data = pd,
#'          q_models = q_glm(),
#'          g_models = g_glm())
#'
#' #' # getting the policy function at stage 2:
#' pf2 <- get_policy_functions(po, stage = 2)
#' args(pf2)
#'
#' # applying the policy function to new data:
#' set.seed(1)
#' new_H <- data.table(C = rnorm(n = 10),
#'                     L = rnorm(n = 10),
#'                     BB = "group1")
#' d2 <- pf2(H = new_H, g_H = new_H)
#' d2
#'
#' # comparing get_policy_functions() and get_policy():
#' all.equal(
#'  pf2(H = get_history(pd, stage = 2)$H, g_H = get_history(pd, stage = 2)$H),
#'  get_policy(po)(pd)[stage == 2]$d
#' )
#' @export
get_policy_functions <- function(object, stage){
  UseMethod("get_policy_functions")
}
