new_policy <- function(fun, name){
  attr(fun, "name") <- name
  class(fun) <- c("policy", "function")

  return(fun)
}

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
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' # defining a dynamic policy:
#' p <- policy_def(
#'   function(L) (L>0)*1,
#'   reuse = TRUE
#' )
#' p
#' head(p(pd), 5)
#'
#' # V-restricted (Doubly Robust) Q-learning:
#' # specifying the learner:
#' pl <- policy_learn(type = "drql",
#'                    control = control_drql(qv_models = q_glm(formula = ~ C)))
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
#' @docType class
NULL

#' Define Policy
#'
#' \code{policy_def} returns a function of class [policy].
#' The function input is a [policy_data] object and it returns a [data.table]
#'  with keys \code{id} and \code{stage} and action variable \code{d}.
#'
#' @param policy_functions A single function/character string or a list of
#' functions/character strings. The list must have the same length as the number
#' of stages.
#' @param full_history If TRUE, the full history at each stage is used as
#' input to the policy functions.
#' @param reuse If TRUE, the policy function is reused at every stage.
#' @param name Character string.
#' @returns Function of class \code{"policy"}. The function takes a
#' [policy_data] object as input and returns a [data.table]
#' with keys \code{id} and \code{stage} and action variable \code{d}.
#' @seealso [get_history_names()], [get_history()].
#' @examples
#' library("polle")
#' ### Single stage"
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#' # defining a static policy (A=1):
#' p1_static <- policy_def(1)
#'
#' # applying the policy:
#' p1_static(pd1)
#'
#' # defining a dynamic policy:
#' p1_dynamic <- policy_def(
#'   function(Z, L) ((3*Z + 1*L -2.5)>0)*1
#' )
#' p1_dynamic(pd1)
#'
#' ### Two stages:
#' d2 <- sim_two_stage(5e2, seed = 1)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' # defining a static policy (A=0):
#' p2_static <- policy_def(0,
#'                         reuse = TRUE)
#' p2_static(pd2)
#'
#' # defining a reused dynamic policy:
#' p2_dynamic_reuse <- policy_def(
#'   function(L) (L > 0)*1,
#'   reuse = TRUE
#' )
#' p2_dynamic_reuse(pd2)
#'
#' # defining a dynamic policy for each stage based on the full history:
#' # available variable names at each stage:
#' get_history_names(pd2, stage = 1)
#' get_history_names(pd2, stage = 2)
#'
#' p2_dynamic <- policy_def(
#'   policy_functions = list(
#'     function(L_1) (L_1 > 0)*1,
#'     function(L_1, L_2) (L_1 + L_2 > 0)*1
#'   ),
#'   full_history = TRUE
#' )
#' p2_dynamic(pd2)
#' @export
policy_def <- function(policy_functions, full_history = FALSE, reuse = FALSE, name = NULL){
  force(policy_functions)
  force(full_history)
  force(reuse)

  # input checks
  if (!(is.logical(full_history) & (length(full_history) == 1)))
    stop("full_history must be TRUE or FALSE")
  if (!(is.logical(reuse) & (length(reuse) == 1)))
    stop("reuse must be TRUE or FALSE")
  if (full_history == TRUE & reuse == TRUE)
    stop("full_history must be FALSE when reuse is TRUE.")
  if (!is.null(name)){
    name <- as.character(name)
    if (length(name) != 1)
      stop("name must be a character string.")
  }
  if (reuse == TRUE &
      inherits(policy_functions, what = c("list",
                                          "numeric",
                                          "character",
                                          "logical",
                                          "factor",
                                          "integer"))){
    if (length(policy_functions) != 1)
      stop("When reuse is TRUE, policy_functions must be a single function or a constant.")
  }

  policy <- function(policy_data){
    if(!inherits(policy_data, what = "policy_data")){
      stop("The policy input is not of inherited class policy_data.")
    }
    action_set <- get_action_set(policy_data)
    K <- get_K(policy_data)

    if (reuse == TRUE){
      # policy_functions as a list:
      policy_functions <- replicate(K, policy_functions)
    }
    # policy_functions as list (or vector):
    policy_functions <- c(policy_functions)

    if (length(policy_functions) != K)
      stop("policy_functions must be a list of length K.")

    policy_functions <- lapply(
      policy_functions,
      function(pf){
        if (inherits(pf, what = "function")){
          pf <- dynamic_policy(fun = pf)
        } else {
          pf <- static_policy(action = pf)
        }
        return(pf)
      }
    )

    stage_histories <- lapply(
      1:K,
      function(k) get_history(policy_data,
                              stage = k,
                              full_history = full_history)
    )
    policy_actions <- mapply(
      function(sp, sh) sp(sh),
      policy_functions,
      stage_histories,
      SIMPLIFY = FALSE
    )
    policy_actions <- rbindlist(policy_actions)
    setkeyv(policy_actions, c("id", "stage"))

    if (!all(unlist(policy_actions[, "d"]) %in% action_set)){
      mes <- "The policy actions does not comply with the action set of the policy data object."
      warning(mes)
    }

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = name)

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
  cat("\n")

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
#' @noRd
static_policy <- function(action, name=paste0("a=",action)) {
  action <- as.character(action)
  if (length(action) != 1)
    stop("the action argument in static_policy must be a single character or string.")

  f <- function(history) {
    d <- NULL
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
#' @noRd
dynamic_policy <- function(fun){

  if (!"..." %in% names(formals(fun))) {
    formals(fun) <- c(formals(fun), alist(...=))
  }

  f <- function(history){
    pol <- get_id_stage(history)
    action <- do.call(what = "fun", args = get_H(history))
    action <- as.character(action)
    d <- NULL
    pol[, d := action]
    return(pol)
  }
  return(f)
}

get_cum_rewards <- function(policy_data, policy=NULL) {
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  A <- U <- id <- stage <- NULL  # R-check glob. var.
  dt <- policy_data$stage_data[, c("id", "stage", "A", "U")]
  setkeyv(dt, c("id", "stage"))
  dt[, U:=cumsum(U), by=id]

  count <- 0
  policy_group <- pol_ind <- NULL # R-check glob. var.
  dt[, policy_group:=0]
  if (!is.null(policy)) {
    if (!is.list(policy)) policy <- list(policy)
    for (pol in policy) {
      count <- count+1
      d <- merge(dt, pol(policy_data), all.x = TRUE)
      d[, pol_ind := all(d == A, na.rm = TRUE), by = "id"]
      dt[d[["pol_ind"]] == TRUE,
         policy_group:= count,
         by=id]
    }
    dt <- subset(dt, policy_group>0)

    default_lab <- paste("policy_", 1:length(policy), sep = "")
    lab <- lapply(policy, function(pol) attributes(pol)[["name"]])
    for (j in seq_along(lab)){
      if(is.null(lab[[j]]))
        lab[[j]] <- default_lab[[j]]
    }
    lab <- unlist(lab)
    dt[,policy_group:=lab[policy_group]]
  }
  dt[, policy_group := as.character(policy_group)]
  dt[policy_group == "0", policy_group := "all"]
  return(dt)
}

policy_g_functions <- function(g_functions, name = "pgf"){
  force(g_functions)

  policy <- function(policy_data){
    action_set <- get_action_set(policy_data)
    g_cols <- paste("g_", action_set, sep = "")
    g_values <- predict.nuisance_functions(g_functions, policy_data)

    dd <- apply(
      g_values[ , g_cols, with = FALSE],
      MARGIN = 1,
      which.max
    )
    d <- action_set[dd]

    policy_actions <- cbind(get_id_stage(policy_data), d = d)
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }

  policy <- new_policy(fun = policy, name = name)

  return(policy)
}
