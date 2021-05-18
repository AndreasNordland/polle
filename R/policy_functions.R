new_policy <- function(policy_functions, full_history = FALSE){
  object <- list(
    policy_functions = policy_functions,
    full_history = full_history
  )
  class(object) <- "policy"

  return(object)
}

#' @export
get_policy_actions <- function(object, policy_data)
  UseMethod("get_policy_actions")

#' @export
get_policy_actions.policy <- function(object, policy_data){
  stopifnot(
    class(policy_data)[[1]] == "policy_data"
  )

  K <- policy_data$dim$K
  policy_functions <- object$policy_functions
  full_history <- object$full_history

  # checking the policy_functions input: must be a list of K policy functions or a single policy function
  stopifnot(
    if(class(policy_functions)[[1]] == "list")
      length(policy_functions) == K
    else
      class(policy_functions)[[1]] == "function" & (K == 1)
  )

  pa <- function(stage){
    his <- get_stage_history(policy_data, stage = stage, full_history = full_history)
    out <- policy_functions[[stage]](his)
    return(out)
  }

  if (class(policy_functions)[[1]] == "list"){
    policy_actions <- lapply(1:K, pa)
    policy_actions <- rbindlist(policy_actions)
    setkey(policy_actions, id, stage)
  } else{
    his <- get_stage_history(policy_data, stage = 1, full_history = full_history)
    policy_actions <- policy_functions(his)
  }

  return(policy_actions)
}
