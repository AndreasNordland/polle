#' Create Policy Learner
#'
#' \code{policy_learn()} is used to specify a policy learning method (Q-learning,
#' V-restricted (doubly robust) Q-learning, V-restricted policy tree
#' learning and outcome weighted learning). Evaluating the policy learner returns a policy object.
#' @param type Type of policy learner method:
#' \itemize{
#'   \item{} \code{"rql"}: Realistic Quality/Q-learning.
#'   \item{} \code{"rqvl"}: Realistic V-restricted (doubly robust) Q-learning.
#'   \item{} \code{"ptl"}: Policy Tree Learning.
#'   \item{} \code{"owl"}: Outcome Weighted Learning.
#'   \item{} \code{"earl"}: Efficient Augmentation and Relaxation Learning (only single stage).
#'   \item{} \code{"rwl"}: Residual Weighted Learning (only single stage).
#' }
#' @param control List of control arguments. Values (and default values) are set using
#' \code{control_{type}()}. Key arguments include:\cr
#' [control_rqvl()]:\cr
#' \itemize{
#'   \item{} \code{qv_models}: Single element or list of V-restricted Q-models created
#'           by [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' }
#' [control_ptl()]: \cr
#' \itemize{
#'   \item{} \code{policy_vars}: Character vector/string or list of character
#' vectors/strings. Variable names used to construct the V-restricted policy tree.
#' The names must be a subset of the history names, see get_history_names().
#'   \item{} \code{hybrid}: If \code{TRUE}, [policytree::hybrid_policy_tree()] is used to
#' fit a policy tree.
#'   \item{} \code{depth}: Integer or integer vector. The depth of the fitted policy
#' tree for each stage.
#' }
#' [control_owl()]: \cr
#' \itemize{
#'   \item{} \code{policy_vars}: As in \code{control_ptl()}.
#'   \item{} \code{loss}: Loss function. The options are \code{"hinge"}, \code{"ramp"},
#' \code{"logit"}, \code{"logit.lasso"}, \code{"l2"}, \code{"l2.lasso"}.
#'   \item{} \code{kernel}: Type of kernel used by the support vector machine. The
#' options are \code{"linear"}, \code{"rbf"}.
#'    \item{} \code{augment}:  If \code{TRUE} the outcomes are augmented.
#' }
#' [control_earl()]/[control_rwl()]: \cr
#' \itemize{
#'   \item{} \code{moPropen}: Propensity model of class "ModelObj", see [modelObj::modelObj].
#'   \item{} \code{moMain}: Main effects outcome model of class "ModelObj".
#'   \item{} \code{moCont} Contrast outcome model of class "ModelObj".
#'   \item{} \code{regime}: An object of class [formula] specifying the design of the policy.
#'   \item{} \code{surrogate}: The surrogate 0-1 loss function. The options are
#' \code{"logit"}, \code{"exp"}, \code{"hinge"}, \code{"sqhinge"}, \code{"huber"}.
#'   \item{} \code{kernel}: The options are \code{"linear"}, \code{"poly"}, \code{"radial"}.
#' }
#' @param alpha Probability threshold for determining realistic actions.
#' @param L Number of folds for cross-fitting nuisance models.
#' @param save_cross_fit_models If \code{TRUE}, the cross-fitted models will be saved.
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @param full_history If \code{TRUE}, the full
#' history is used to fit each policy function (e.g. QV-model, policy tree). If FALSE, the single stage/
#' "Markov type" history is used to fit each policy function.
#' @param name Character string.
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
#' @references
#' V-restricted Q-learning (\code{type = "rqvl"}): Luedtke, Alexander R., and
#' Mark J. van der Laan. "Super-learning of an optimal dynamic treatment rule."
#' The international journal of biostatistics 12.1 (2016): 305-332.
#' \doi{10.1515/ijb-2015-0052}.\cr
#' \cr
#' Policy Tree Learning (\code{type = "ptl"}): Zhou, Zhengyuan, Susan Athey,
#' and Stefan Wager. "Offline multi-action policy learning: Generalization and
#' optimization." Operations Research (2022). \doi{10.1287/opre.2022.2271}.\cr
#' \cr
#' (Augmented) Outcome Weighted Learning: Liu, Ying, et al. "Augmented
#' outcome‐weighted learning for estimating optimal dynamic treatment regimens."
#' Statistics in medicine 37.26 (2018): 3776-3788. \doi{10.1002/sim.7844}.
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
#' pl <- policy_learn(
#'   type = "rqvl",
#'   control = control_rqvl(qv_models = list(q_glm(formula = ~ C_1 + BB),
#'                                           q_glm(formula = ~ L_1 + BB))),
#'   full_history = TRUE
#' )
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
                         control = list(),
                         alpha = 0,
                         L = 1,
                         full_history = FALSE,
                         save_cross_fit_models = FALSE,
                         future_args = list(future.seed = TRUE),
                         name = type
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
  } else if (type %in% c("rqvl", "qvl", "qv_learning", "qv-learning")) {
    call <- "rqvl"
  } else if (type %in% c("ptl", "policytree", "policy_tree")){
    if (!requireNamespace("policytree")) {
      stop("The policytree package is required to perform value searching using trees.")
    }
    call <- "ptl"
  } else if (type %in% c("owl", "bowl")){
    if (!requireNamespace("DTRlearn2")) {
      stop("The DTRlearn2 package is required to perform value searching using outcome-weighted learning.")
    }
    call <- "dtrlearn2_owl"
  } else if (type %in% c("earl")){
    call <- "dyntxregime_earl"
  } else if (type %in% c("rwl")){
    call <- "dyntxregime_rwl"
  } else{
    stop("Unknown type of policy learner. Use 'rql', 'rqvl', 'ptl', 'owl', 'earl' or 'rwl'.")
  }
  args <- append(pl_args, control)
  pl <- pl(call = call, args = args)
  class(pl) <- c("policy_learn", "function")
  attr(pl, "type") <- type
  attr(pl, "pl_args") <- pl_args
  attr(pl, "name") <- name

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
#'                                                control = control_rqvl(qv_models = q_glm(~.))),
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
#'                    control = control_rqvl(qv_models = q_glm(formula = ~ C)))
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
#'                    control = control_ptl(policy_vars = list(c("C_1", "BB"),
#'                                                             c("L_1", "BB"))),
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


