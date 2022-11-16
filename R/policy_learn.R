#' Create Policy Learner
#'
#' \code{policy_learn()} is used to specify a policy learning method (Q-learning,
#' V-restricted (doubly robust) Q-learning and V-restricted policy tree
#' learning). Evaluating the policy learner returns a policy object.
#' @param type Type of policy learner method:
#' \itemize{
#'   \item{} \code{"rql"}: Realistic Quality/Q-learning.
#'   \item{} \code{"rqvl"}: Realistic V-restricted (doubly robust) Q-learning.
#'   \item{} \code{"ptl"}: Policy Tree Learning.
#'   \item{} \code{"earl"}: Efficient Augmentation and Relaxation Learning.
#' }
#' @param alpha Probability threshold for determining realistic actions.
#' @param L (only used if \code{type = "rqvl"} or \code{type = "ptl"}) Number of folds for
#' cross-fitting.
#' @param save_cross_fit_models If \code{TRUE}, the cross-fitted models will be saved.
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @param full_history If \code{TRUE}, the full
#' history is used to fit each policy function (e.g. QV-model, policy tree). If FALSE, the single stage/
#' "Markov type" history is used to fit each policy function.
#' @param rqvl_args Arguments used if \code{type = "rqvl"}.
#' \itemize{
#'   \item{} \code{qv_models}: V-restricted Q-models created by [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' }
#' @param ptl_args Arguments used if \code{type = "ptl"}, see [policytree::policy_tree()].
#' \itemize{
#'   \item{} \code{policy_vars}: Character vector/string or
#'   list of character vectors/strings. Variable names used to construct a
#'   V-restricted policy tree. The names must be a subset of the history variable
#'   names, see [get_history_names()].
#'   \item{} \code{depth}: Numeric or numeric vector.
#'   The depth of the fitted policy tree for each stage.
#'   \item{} \code{split.step}:  Numeric or numeric vector.
#'   The number of possible splits to consider when performing policy tree search
#'   at each stage.
#'   \item{} \code{min.node.size}: Numeric or numeric vector.
#'   The smallest terminal node size permitted at each stage.
#'   \item{} \code{hybrid}: If \code{TRUE}, [policytree::hybrid_policy_tree()] is used to fit a policy tree.
#'   \item{} \code{search.depth}: (only used if \code{hybrid = TRUE})
#'   Numeric or numeric vector. Depth to look ahead when splitting at each stage.
#' }
#' @param bowl_args Arguments used if \code{type = "bowl"}, see [DTRlearn2::owl()].
#' \itemize{
#'   \item{} \code{policy_vars}: Character vector/string or
#'   list of character vectors/strings. Variable names used to construct a
#'   V-restricted policy tree. The names must be a subset of the history variable
#'   names, see [get_history_names()].
#'   \item{} \code{reuse_scales}: If \code{TRUE}, the scales of the history matrix will be
#'   saved and reused when applied to (new) test data.
#'   \item{} \code{res.lasso}: If \code{TRUE} a lasso penalty is applied.
#'   \item{} \code{loss}: Loss function.
#'   \item{} \code{kernel}: Type of kernel used by the support vector machine.
#'   \item{} \code{augment}: If \code{TRUE} the
#'   outcomes are augmented at each stage.
#'   \item{} \code{c}: Regularization parameter.
#'   \item{} \code{sigma}: Tuning parameter.
#'   \item{} \code{s}: Slope parameter.
#'   \item{} \code{m}: Number of folds for cross-validation of tuning parameters.
#' }
#' @param earl_args Arguments used if \code{type = "earl"}, see [DynTxRegime::earl()].
#' \itemize{
#'   \item{} \code{moPropen} Propensity model of class "ModelObj", see [modelObj::modelObj].
#'   \item{} \code{moMain} Main effects outcome model of class "ModelObj".
#'   \item{} \code{moCont} Contrast outcome model of class "ModelObj".
#'   \item{} \code{regime} An object of class [formula] specifying the design
#'   of the policy/regime.
#'   \item{} ... see [DynTxRegime::earl()] for additional arguments.
#' }
#' @param x Object of class "policy_object" or "policy_learn".
#' @param ... Additional arguments passed to print.
#' @returns Function of inherited class \code{"policy_learn"}.
#' Evaluating the function on a [policy_data] object returns an object of
#' class [policy_object]. A policy object is a list containing all or
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
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class "policy_object":
#' \itemize{
#' \item{[get_g_functions()]}{ Extract the fitted g-functions.}
#' \item{[get_q_functions()]}{ Extract the fitted Q-functions.}
#' \item{[get_policy()]}{ Extract the fitted policy object.}
#' \item{[get_policy_functions()]}{ Extract the fitted policy function for
#'                                 a given stage.}
#' \item{[get_policy_actions()]}{ Extract the (fitted) policy actions.}
#' }
#' @details
#' For references on V-restricted Q-learning (\code{type = "rqvl"}), see \doi{10.1515/ijb-2015-0052}.
#' For references on policy tree learning (\code{type = "ptl"}), see \doi{10.48550/arXiv.1810.04778}.
#' @seealso [policy_eval()]
#' @examples
#' library("polle")
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("BB"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' ### V-restricted (Doubly Robust) Q-learning
#'
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    rqvl_args = list(qv_models = list(q_glm(formula = ~ C_1 + BB),
#'                                                      q_glm(formula = ~ L_1 + BB))),
#'                    full_history = TRUE)
#'
#' # evaluating the learned policy
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#' pe
#' # getting the policy object:
#' po <- get_policy_object(pe)
#' # inspecting the fitted QV-model for each action strata at stage 1:
#' po$qv_functions$stage_1
#' head(get_policy(pe)(pd))
#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         L = 1,
                         save_cross_fit_models = FALSE,
                         future_args = list(future.seed = TRUE),
                         full_history = FALSE,
                         rqvl_args = list(qv_models = NULL),
                         ptl_args = list(policy_vars = NULL,
                                         depth = 2,
                                         split.step = 1,
                                         min.node.size = 1,
                                         hybrid = FALSE,
                                         search.depth = 2),
                         bowl_args = list(policy_vars = NULL,
                                          reuse_scales = TRUE,
                                          res.lasso = TRUE,
                                          loss = 'hinge',
                                          kernel = 'linear',
                                          augment = FALSE,
                                          c = 2^(-2:2),
                                          sigma = c(0.03,0.05,0.07),
                                          s = 2.^(-2:2),
                                          m = 4),
                         earl_args = list(moPropen = NULL,
                                          moMain = NULL,
                                          moCont = NULL,
                                          regime = NULL,
                                          iter = 0L,
                                          fSet = NULL,
                                          lambdas = 0.5,
                                          cvFolds = 0L,
                                          surrogate = "hinge",
                                          kernel = "linear",
                                          kparam = NULL,
                                          verbose = 0L),
                         rwl_args = list(moPropen = NULL,
                                         moMain = NULL,
                                         regime = NULL,
                                         fSet = NULL,
                                         lambdas = 2,
                                         cvFolds = 0L,
                                         kernel = "linear",
                                         kparam = NULL,
                                         responseType = "continuous",
                                         verbose = 0L)
){

  pl_args <- list(
    alpha = alpha,
    L = L,
    save_cross_fit_models = save_cross_fit_models,
    future_args = future_args,
    full_history = full_history
  )

  if (length(type) != 1 | !is.character(type))
    stop("type must be a character string.")
  type <- tolower(type)
  if (type %in% c("rql", "ql", "q_learning", "q-learning")) {
    call <- "rql"
    args <- pl_args
  } else if (type %in% c("rqvl", "qvl", "qv_learning", "qv-learning")) {
    call <- "rqvl"
    args <- append(pl_args, rqvl_args)
  } else if (type %in% c("ptl", "policytree", "policy_tree")){
    if (!requireNamespace("policytree")) {
      stop("The policytree package is required to perform value searching using trees.")
    }
    call <- "ptl"
    args <- append(pl_args, ptl_args)
  } else if (type %in% c("bowl", "owl")){
    if (!requireNamespace("DTRlearn2")) {
      stop("The DTRlearn2 package is required to perform value searching using outcome-weighted learning.")
    }
    call <- "bowl"
    args <- append(pl_args, bowl_args)
  } else if (type %in% c("earl")){
    call <- "dyntxregime_earl"
    args <- append(pl_args, earl_args)
  } else if (type %in% c("rwl")){
    call <- "dyntxregime_rwl"
    args <- append(pl_args, rwl_args)
  } else{
    stop("Unknown type of policy learner. Use 'rql', 'rqvl', 'ptl', 'bowl', 'earl' or 'rwl'.")
  }
  pl <- pl(call = call, args = args)
  class(pl) <- c("policy_learn", "function")
  attr(pl, "type") <- type
  attr(pl, "pl_args") <- pl_args

  return(pl)
}

pl <- function(call, args){
  function(policy_data,
           g_models = NULL, g_functions = NULL, g_full_history = FALSE,
           q_models, q_full_history = FALSE){
    eval_args <- as.list(environment())
    args <- append(args, eval_args)
    do.call(what = call, args)
  }
}

#' @rdname policy_learn
#' @name policy_object
NULL

#' @rdname policy_learn
#' @export
print.policy_object <- function(x, ...){
  cat("Policy object with list elements:")
  cat("\n")
  cp <- paste(names(x), collapse = ", ")
  cat(cp)
  cat("\n")
  cat("Use 'get_policy' to get the associated policy.")
}


#' @rdname policy_learn
#' @export
print.policy_learn <- function(x, ...) {

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

#' @title Get Policy Object
#'
#' @description Extract the fitted policy object.
#' @param object Object of class [policy_eval].
#' @returns Object of class [policy_object].
#' @examples
#' library("polle")
#' ### Single stage:
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#'
#' # evaluating the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'                    policy_learn = policy_learn(type = "rqvl",
#'                                                rqvl_args = list(qv_models = q_glm(~.))),
#'                    g_models = g_glm(),
#'                    q_models = q_glm())
#'
#' # extracting the policy object:
#' get_policy_object(pe1)
#' @export
get_policy_object <- function(object)
  UseMethod("get_policy_object")

#' @export
get_policy_object.policy_eval <- function(object){
  po <- getElement(object, "policy_object")
  return(po)
}

#' @title Get Policy
#'
#' @description \code{get_policy} extracts the policy from a policy object
#' or a policy evaluation object The policy is a function which take a
#' policy data object as input and returns the policy actions.
#' @param object Object of class [policy_object] or [policy_eval].
#' @returns function of class [policy].
#' @examples
#' library("polle")
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("BB"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' ### V-restricted (Doubly Robust) Q-learning
#'
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    rqvl_args = list(qv_models = q_glm(formula = ~ C)))
#'
#' # fitting the policy (object):
#' po <- pl(policy_data = pd,
#'          q_models = q_glm(),
#'          g_models = g_glm())
#'
#' # getting and applying the policy:
#' head(get_policy(po)(pd))
#'
#' # the policy learner can also be evaluated directly:
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#'
#' # getting and applying the policy again:
#' head(get_policy(pe)(pd))
#' @export
get_policy <- function(object){
  UseMethod("get_policy")
}

#' @export
get_policy.policy_eval <- function(object){
  po <- get_policy_object(object)
  if (is.null(po))
    return(NULL)
  p <- get_policy(po)
  return(p)
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
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = "BB",
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' ### Realistic V-restricted Policy Tree Learning
#' # specifying the learner:
#' pl <- policy_learn(type = "ptl",
#'                    ptl_args = list(policy_vars = list(c("C_1", "BB"),
#'                                       c("L_1", "BB"))),
#'                    full_history = TRUE,
#'                    alpha = 0.05)
#'
#' # evaluating the learner:
#' pe <- policy_eval(policy_data = pd,
#'                   policy_learn = pl,
#'                   q_models = q_glm(),
#'                   g_models = g_glm())
#'
#' # getting the policy function at stage 2:
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
#' head(d2)
#' @export
get_policy_functions <- function(object, stage){
  UseMethod("get_policy_functions")
}

#' @export
get_g_functions.policy_object <- function(object){
  getElement(object, "g_functions")
}

#' @export
get_q_functions.policy_object <- function(object){
  getElement(object, "q_functions")
}


