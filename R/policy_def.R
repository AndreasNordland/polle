#' Define Policy
#'
#' \code{policy_def} returns a function taking a [policy_data] object as input.
#' The function returns a [data.table] with keys id and stage and variable d
#' (action given by the policy).
#'
#' @param policy_functions A single policy function or a list of policy functions; see [static_policy] and [dynamic_policy].
#' A list of policy functions must have the same length as the number of stages.
#' @param full_history If TRUE, the full history at each stage is used as input to the policy functions.
#' @param reuse If TRUE, the policy function is reused at every stage.
#' @returns Function with argument \code{policy_data}.
#' @examples
#' library("polle")
#' ### Single stage
#' # simulating single-stage policy data
#' source(system.file("sim", "single_stage.R", package="polle"))
#' par0 <- c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, s = 1)
#' d1 <- sim_single_stage(5e2, seed=1, par=par0); rm(par0)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#' # defining a static policy:
#' p1_static <- policy_def(static_policy(1))
#' head(p1_static(pd1),5)
#' # defining a dynamic policy:
#' p1_dynamic <- policy_def(dynamic_policy(fun = function(Z, L) ((3*Z + 1*L -2.5)>0)*1))
#' head(p1_dynamic(pd1),5)
#'
#' ### Multiple stages
#' source(system.file("sim", "two_stage.R", package="polle"))
#' par0 <- c(gamma = 0.5, beta = 1)
#' d2 <- sim_two_stage(5e2, seed=1, par=par0); rm(par0)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' # defining a static policy:
#' p2_static <- policy_def(static_policy(0),
#'                         reuse = TRUE)
#' head(p2_static(pd2),5)
#'
#' # defining a repeated dynamic policy:
#' p2_dynamic_reuse <- policy_def(dynamic_policy(function(L) (L > 0) * 1), reuse = TRUE)
#' head(p2_dynamic_reuse(pd2), 5)
#'
#' # defining dynamic policy for each stage based on the full history:
#' get_history_names(pd2, stage = 1) # function arguments which can be used in the first stage
#' get_history_names(pd2, stage = 2) # function arguments which can be used in the second stage
#'
#' p2_dynamic <- policy_def(
#'  policy_functions = list(
#'   dynamic_policy(function(L_1) (L_1 > 0)*1),
#'   dynamic_policy(function(L_1, L_2) (L_1 + L_2 > 0)*1)
#'  ),
#'  full_history = TRUE
#' )
#' p2_dynamic(pd2)
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
print.policy <- function(x) {

  cat("Policy with arguments")

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

##' @export
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

##' @export
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
    pol[, d := action]
    return(pol)
  }
  return(f)
}

