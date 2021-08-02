# a policy should take a policy data object as input and return a policy action data.table with id, stage and d (d: policy action)
# a stage policy should take a history object as input and return a data.table with id, stage and d (d: policy action)

#' @export
policy_def <- function(stage_policies, full_history = FALSE, replicate = FALSE){
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
  attr(policy, "name") <- attr(stage_policies, "name")

  return(policy)
}

#' @export
get_policy <- function(object)
  UseMethod("get_policy")
