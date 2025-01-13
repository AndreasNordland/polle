#' @rdname policy_learn
#' @export
print.policy_object <- function(x, ...){
  cat("Policy object with list elements:")

  cp <- paste(names(x), collapse = ", ")
  cp <- paste(cp, collapse = "")
  cp <- gsub(" ", "", cp, fixed = TRUE)
  cp <- gsub(",", ", ", cp, fixed = TRUE)

  cat("\n")
  cat(paste(strwrap(cp, 60), collapse="\n"))

    cat("\n")
  cat("Use 'get_policy' to get the associated policy.")
  cat("\n")
}

#' @title Get Policy Object
#'
#' @description Extract the fitted policy object.
#' @param object Object of class [policy_eval].
#' @returns Object of class [policy_object].
#' @examples
#' library("polle")
#' ### Single stage:
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#'
#' # evaluating the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'                    policy_learn = policy_learn(type = "drql",
#'                                                control = control_drql(qv_models = q_glm(~.))),
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
#' @param threshold Numeric vector.
#' Thresholds for the first stage policy function.
#' @returns function of class [policy].
#' @examples
#' library("polle")
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed = 1)
#' pd <- policy_data(d,
#'   action = c("A_1", "A_2"),
#'   baseline = c("BB"),
#'   covariates = list(
#'     L = c("L_1", "L_2"),
#'     C = c("C_1", "C_2")
#'   ),
#'   utility = c("U_1", "U_2", "U_3")
#' )
#' pd
#'
#' ### V-restricted (Doubly Robust) Q-learning
#'
#' # specifying the learner:
#' pl <- policy_learn(
#'   type = "drql",
#'   control = control_drql(qv_models = q_glm(formula = ~C))
#' )
#'
#' # fitting the policy (object):
#' po <- pl(
#'   policy_data = pd,
#'   q_models = q_glm(),
#'   g_models = g_glm()
#' )
#'
#' # getting and applying the policy:
#' head(get_policy(po)(pd))
#'
#' # the policy learner can also be evaluated directly:
#' pe <- policy_eval(
#'   policy_data = pd,
#'   policy_learn = pl,
#'   q_models = q_glm(),
#'   g_models = g_glm()
#' )
#'
#' # getting and applying the policy again:
#' head(get_policy(pe)(pd))
#' @export
get_policy <- function(object, threshold = NULL) {
  UseMethod("get_policy")
}

#' @export
get_policy.policy_eval <- function(object, threshold = NULL) {
  po <- get_policy_object(object)
  if (is.null(po)) {
    return(NULL)
  }
  p <- get_policy(po, threshold = threshold)
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
#' @param threshold  Numeric, threshold for not
#' choosing the reference action at stage 1.
#' @param ... Additional arguments.
#' @param include_g_values If TRUE, the g-values are included as an attribute.
#' @returns Functions with arguments:
#' \describe{
#' \item{\code{H}}{[data.table::data.table] containing the variables needed to evaluate the policy (and g-function).}
#' }
#' @examples
#' library("polle")
#' ### Two stages:
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
#' new_H <- data.frame(C = rnorm(n = 10),
#'                     L = L_1,
#'                     L_1 = L_1,
#'                     BB = "group1")
#' d2 <- pf2(H = new_H)
#' head(d2)
#' @export
get_policy_functions <- function(object, stage, threshold, ...) {
  UseMethod("get_policy_functions")
}

#' @export
get_g_functions.policy_object <- function(object) {
  getElement(object, "g_functions")
}

#' @export
get_q_functions.policy_object <- function(object) {
  getElement(object, "q_functions")
}

check_stage <- function(stage, K) {
  if (missing(stage)) {
    stop("stage argument is missing.")
  }
  is_int <- stage %% 1 == 0
  if (!(is_int && (length(stage) == 1) && (stage >= 1) && (stage <= K))) {
    stop("stage must be a positive integer less than or equal to K.")
  }
}
