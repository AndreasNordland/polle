# TODO implement the single stage policy

# a policy should take a policy data object as input and return a policy action data.table with id, stage and d (d: policy action)
# a stage policy should take a history object as input and return a data.table with id, stage and d (d: policy action)

#' @export
new_policy <- function(stage_policies, full_history = FALSE, replicate = FALSE){
  force(stage_policies)
  force(full_history)
  force(replicate)

  stopifnot(
    !(full_history == TRUE & replicate == TRUE),
    !(class(stage_policies)[[1]] == "list" & replicate == TRUE)
  )

  policy <- function(policy_data){
    action_set <- policy_data$action_set
    K <- policy_data$dim$K

    if (replicate == TRUE){
      stage_policies <- replicate(K, stage_policies)
    }

    # checking the stage_policies input:
    # must be a list of K stage policies or a single stage policy
    stopifnot(
      if(class(stage_policies)[[1]] == "list")
        length(stage_policies) == K
      else
        class(stage_policies)[[1]] == "function" & (K == 1)
    )

    if (class(stage_policies)[[1]] == "list"){
    stage_histories <- lapply(1:K, function(k) get_stage_history(policy_data, stage = k, full_history = full_history))
    policy_actions <- mapply(function(sp, sh) sp(sh), stage_policies, stage_histories, SIMPLIFY = FALSE)
    policy_actions <- rbindlist(policy_actions)
    setkey(policy_actions, id, stage)
    } else{
      stop()
    }

    stopifnot(
      any("d" %in% colnames(policy_actions)),
      all(policy_actions$d %in% action_set),
      all(key(policy_actions) == c("id", "stage"))
    )

    return(policy_actions)
  }

  return(policy)
}

#' @export
# get_policy_actions <- function(object, policy_data)
#   UseMethod("get_policy_actions")

#' @export
# get_policy_actions.policy <- function(object, policy_data){
#   stopifnot(
#     class(policy_data)[[1]] == "policy_data"
#   )
#
#   K <- policy_data$dim$K
#   stage_policies <- object$stage_policies
#   full_history <- object$full_history
#
#   # checking the stage_policies input: must be a list of K policy functions or a single policy function
#   stopifnot(
#     if(class(stage_policies)[[1]] == "list")
#       length(stage_policies) == K
#     else
#       class(stage_policies)[[1]] == "function" & (K == 1)
#   )
#
#   pa <- function(stage){
#     his <- get_stage_history(policy_data, stage = stage, full_history = full_history)
#     out <- stage_policies[[stage]](his)
#     return(out)
#   }
#
#   if (class(stage_policies)[[1]] == "list"){
#     policy_actions <- lapply(1:K, pa)
#     policy_actions <- rbindlist(policy_actions)
#     setkey(policy_actions, id, stage)
#   } else{
#     his <- get_stage_history(policy_data, stage = 1, full_history = full_history)
#     policy_actions <- stage_policies(his)
#   }
#
#   return(policy_actions)
# }

#' @export
get_policy <- function(object)
  UseMethod("get_policy")
