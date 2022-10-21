#' @title Policy-class
#' @name policy
#'
#' @description  A function of inherited class "policy" takes a policy
#' data object as input and returns the policy actions for every observation
#' for every (observed) stage.
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class
#' \code{policy}:
#' \itemize{
#'   \item{\code{print}}{Baisc print function}
#' }
#' @details A policy can either be defined directly by the user using
#' [policy_def] or a policy can be fitted using [policy_learn]
#' (or [policy_eval]). [policy_learn] returns a [policy_object] from which
#' the policy can be extracted using [get_policy].
#' @returns [data.table] with keys \code{id} and \code{stage} and
#' action variable \code{d}.
#' @examples
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' # defining a dynamic policy:
#' p <- policy_def(
#'   dynamic_policy(function(L) (L > 0) * 1),
#'   reuse = TRUE
#' )
#' p
#' head(p(pd), 5)
#'
#' # V-restricted (Doubly Robust) Q-learning:
#' # specifying the learner:
#' pl <- policy_learn(type = "rqvl",
#'                    qv_models = q_glm(formula = ~ C))
#'
#' # fitting the policy (object):
#' po <- pl(policy_data = pd,
#'          q_models = q_glm(),
#'          g_models = g_glm())
#'
#' p <- get_policy(po)
#' p
#'
#' head(p(pd))
#'
#' @docType class
NULL

#' Define Policy
#'
#' \code{policy_def} returns a function of inherited class [policy].
#' The function input is a [policy_data] object and it returns a [data.table]
#'  with keys \code{id} and \code{stage} and action variable \code{d}.
#'
#' @param policy_functions A single policy function or a list of policy functions; see [static_policy] and [dynamic_policy].
#' The list must have the same length as the number of stages.
#' @param full_history If TRUE, the full history at each stage is used as input to the policy functions.
#' @param reuse If TRUE, the policy function is reused at every stage.
#' @returns Function of inherited class \code{"policy"}. The function takes a
#' [policy_data] object as input and returns a [data.table]
#' with keys \code{id} and \code{stage} and action variable \code{d}.
#' @seealso [get_history_names.policy_data()], [get_history.policy_data()].
#' @examples
#' library("polle")
#' ### Single stage"
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#' # defining a static policy:
#' p1_static <- policy_def(static_policy(1))
#'
#' # applying the policy:
#' head(p1_static(pd1),5)
#'
#' # defining a dynamic policy:
#' p1_dynamic <- policy_def(
#'   dynamic_policy(fun = function(Z, L) ((3*Z + 1*L -2.5)>0)*1)
#' )
#' head(p1_dynamic(pd1),5)
#'
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d2 <- sim_two_stage(5e2, seed=1)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' # defining a static policy:
#' p2_static <- policy_def(static_policy(0),
#'                         reuse = TRUE)
#' head(p2_static(pd2),5)
#'
#' # defining a repeated dynamic policy:
#' p2_dynamic_reuse <- policy_def(
#'   dynamic_policy(function(L) (L > 0) * 1),
#'   reuse = TRUE
#' )
#' head(p2_dynamic_reuse(pd2), 5)
#'
#' # defining a dynamic policy for each stage based on the full history:
#' # available variable names at each stage:
#' get_history_names(pd2, stage = 1)
#' get_history_names(pd2, stage = 2)
#'
#' p2_dynamic <- policy_def(
#'   policy_functions = list(
#'     dynamic_policy(function(L_1) (L_1 > 0)*1),
#'     dynamic_policy(function(L_1, L_2) (L_1 + L_2 > 0)*1)
#'   ),
#'   full_history = TRUE
#' )
#' head(p2_dynamic(pd2))
#'
#' @export
policy_def <- function(policy_functions, full_history = FALSE, reuse = FALSE){
  force(policy_functions)
  force(full_history)
  force(reuse)

  if (full_history == TRUE & reuse == TRUE)
    stop("full_history must be FALSE when reuse is TRUE.")
  if (reuse == TRUE & class(policy_functions)[[1]] == "list")
    stop("When reuse is TRUE policy_functions must be a single policy function.")

  policy <- function(policy_data){
    if(!any(class(policy_data) == "policy_data")){
      stop("policy input is not of class policy_data.")
    }

    action_set <- get_action_set(policy_data)
    K <- get_K(policy_data)

    if (reuse == TRUE){
      policy_functions <- replicate(K, policy_functions)
    }
    if (class(policy_functions)[[1]] != "list" & reuse == FALSE & K > 1)
      stop("When reuse is FALSE and K>1, policy_functions must be a list of length K.")

    if (class(policy_functions)[[1]] == "list"){
      if (length(policy_functions) != K)
        stop("policy_functions must be a list of length K (or a single policy function).")
      for (k in seq_along(policy_functions)){
        if(!any(class(policy_functions[[k]]) == "function"))
          stop("policy_functions must be a list of functions.")
      }
    } else {
      if(!any(class(policy_functions) == "function"))
        stop("policy_functions must be a single function (or a list of functions of length K).")
    }

    if (class(policy_functions)[[1]] == "list"){
      stage_histories <- lapply(1:K, function(k) get_history(policy_data, stage = k, full_history = full_history))
      policy_actions <- mapply(function(sp, sh) sp(sh), policy_functions, stage_histories, SIMPLIFY = FALSE)
      policy_actions <- rbindlist(policy_actions)
      setkey(policy_actions, id, stage)
    } else{
      history <- state_history(policy_data)
      policy_actions <- policy_functions(history)
    }

    stopifnot(
      any("d" %in% colnames(policy_actions)),
      all(policy_actions$d %in% action_set),
      all(key(policy_actions) == c("id", "stage"))
    )

    return(policy_actions)
  }

  attr(policy, "name") <- attr(policy_functions, "name", exact = TRUE)
  class(policy) <- c("policy", "function")

  return(policy)
}

#' @export
print.policy <- function(x, ...) {

  cat("Policy with argument(s)")

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


#' @title Define Static Policy
#'
#' @description \code{static_policy} defines a static policy with a given
#' action.
#' @param action Character string. The static action to be applied.
#' @param name Name of the policy.
#' @returns function with arguments \code{history}. Specifically,
#' \code{history} is a history object, see [history]. When evaluated, the
#' function returns a [data.table] with keys \code{id} and \code{stage} and
#' action variable \code{d}.
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
#' # getting a history object for stage 2:
#' his <- get_history(pd, stage = 2)
#'
#' # applying the static policy at the given stage:
#' head(static_policy(action = 1)(his))
#' @export
static_policy <- function(action, name=paste0("a=",action)) {
  action <- as.character(action)
  if (length(action) != 1)
    stop("the action argument in static_policy must be a single character or string.")

  f <- function(history) {
    pol <- get_id_stage(history)
    pol[, d := action]
    return(pol)
  }
  return(structure(f, name=name))
}

#' @title Define Dynamic Policy
#'
#' @description \code{dynamic_policy} defines a dynamic policy given by a
#' user-specifed function.
#' @param fun Function with arguments associated with variables a given history
#' object, see example. The function must return a vector of character strings.
#' @returns function with arguments \code{history}. Specifically,
#' \code{history} is a history object, see [history]. When evaluated, the
#' function returns a [data.table] with keys \code{id} and \code{stage} and
#' action variable \code{d}.
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
#' # getting the state/Markov-type history object for stage 2:
#' his <- get_history(pd, stage = 2)
#'
#' # avaible variable names:
#' get_history_names(pd)
#' colnames(his$H)
#'
#' # applying the dynamic policy at stage 2:
#' head(dynamic_policy(fun = function(C) C>1)(his))
#'
#' # getting the full history object for stage 2:
#' his <- get_history(pd, stage = 2, full_history = TRUE)
#'
#' # avaible variable names:
#' get_history_names(pd, stage = 2)
#' colnames(his$H)
#'
#' # applying the dynamic policy at stage 2:
#' head(dynamic_policy(fun = function(C_1, C_2) (C_1>1) & (C_2>1))(his))
#' @export
dynamic_policy <- function(fun){
  if (!any(class(fun) == "function"))
    stop("the fun argument in dynamic_policy must be a function.")

  if (!"..." %in% names(formals(fun))) {
    formals(fun) <- c(formals(fun), alist(...=))
  }

  f <- function(history){
    pol <- get_id_stage(history)
    action <- do.call(what = "fun", args = get_H(history))
    if (is.logical(action))
      action <- action * 1
    action <- as.character(action)
    d <- NULL
    pol[, d := action]
    return(pol)
  }
  return(f)
}

