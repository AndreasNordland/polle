# a policy should take a policy data object as input and return a policy action data.table with id, stage and d (d: policy action)
# a stage policy should take a history object as input and return a data.table with id, stage and d (d: policy action)

#' @export
policy_def <- function(stage_policies, full_history = FALSE, reuse = FALSE){
  force(stage_policies)
  force(full_history)
  force(reuse)

  if (full_history == TRUE & reuse == TRUE)
    stop("full_history must be FALSE when reuse is TRUE.")
  if (reuse == TRUE & class(stage_policies)[[1]] == "list")
    stop("When reuse is TRUE stage_policies must be a single policy function.")

  policy <- function(policy_data){
    if(!any(class(policy_data) == "policy_data")){
      stop("policy input is not of class policy_data.")
    }

    action_set <- get_action_set(policy_data)
    K <- get_K(policy_data)

    if (reuse == TRUE){
      stage_policies <- replicate(K, stage_policies)
    }
    if (class(stage_policies)[[1]] != "list" & reuse == FALSE & K > 1)
      stop("When reuse is FALSE and K>1, stage_policies must be a list of length K.")

    if (class(stage_policies)[[1]] == "list"){
      if (length(stage_policies) != K)
        stop("stage_policies must be a list of length K (or a single policy function).")
      for (k in seq_along(stage_policies)){
        if(!any(class(stage_policies[[k]]) == "function"))
          stop("stage_policies must be a list of functions.")
      }
    } else {
      if(!any(class(stage_policies) == "function"))
        stop("stage_policies must be a single function (or a list of functions of length K).")
    }

    if (class(stage_policies)[[1]] == "list"){
      stage_histories <- lapply(1:K, function(k) get_history(policy_data, stage = k, full_history = full_history))
      policy_actions <- mapply(function(sp, sh) sp(sh), stage_policies, stage_histories, SIMPLIFY = FALSE)
      policy_actions <- rbindlist(policy_actions)
      setkey(policy_actions, id, stage)
    } else{
      history <- state_history(policy_data)
      policy_actions <- stage_policies(history)
    }

    stopifnot(
      any("d" %in% colnames(policy_actions)),
      all(policy_actions$d %in% action_set),
      all(key(policy_actions) == c("id", "stage"))
    )

    return(policy_actions)
  }

  attr(policy, "name") <- attr(stage_policies, "name", exact = TRUE)

  return(policy)
}

#' @export
get_policy <- function(object)
  UseMethod("get_policy")

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

