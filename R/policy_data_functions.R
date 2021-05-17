#' @export
copy_policy_data <- function(object){

  object$stage_data <- copy(object$stage_data)
  object$baseline_data <- copy(object$baseline_data)

  return(object)
}

#' @export
partial <- function(object, K)
  UseMethod("partial")

#' @export
partial.policy_data <- function(object, K){
  # copy object to avoid references in data.table
  object <- copy_policy_data(object)

  if(K >= object$dim$K)
    return(object)

  # required column names in stage_data:
  action_utility_names <- object$colnames$action_utility_names
  required_names <- c("id", "stage", "event", "A", "U")

  # filtering stage_data rows up till stage K:
  stage_data_K <- object$stage_data[stage <= K, ]

  # filtering stage_data rows for stages above K and the required columns:
  stage_data_res <- object$stage_data[stage > K, ..required_names]

  # summarizing stage_data_res as a single row:
  stage_data_res_sum <- stage_data_res[
    ,
    .(
      stage = min(stage), # min(stage) is K + 1
      event = max(event), # max(event) is the event at stage K*, which is either 1 or 2
      U = sum(U) # sum(U) is the sum of the utility contributions from stage K+1 to K*
    ),
    id
  ]
  stage_data_res_sum[, (action_utility_names) := NA]
  # binding stage_data_K with stage_data_res_sum
  stage_data <- rbindlist(list(stage_data_K, stage_data_res_sum), fill = TRUE, use.names = TRUE)

  # setting keys and index
  setkey(stage_data, id, stage)
  setindex(stage_data, event)

  object$stage_data <- stage_data
  object$dim$K <- K

  return(object)
}

#' @export
utility <- function(object)
  UseMethod("utility")

#' @export
utility.policy_data <- function(object){
  stage_data <- object$stage_data

  U <- stage_data[, .(U = sum(U)), id]

  return(U)
}

get_stage_history <- function(policy_data, stage, full_history){
  if (full_history == TRUE){
    his <- full_stage_history(policy_data, stage = stage)
  } else{
    his <- markov_stage_history(policy_data, stage = stage)
  }
  return(his)
}

get_actions <- function(policy_data){
  actions <- policy_data$stage_data[event == 0, c("id", "stage", "A"), with = FALSE]

  return(actions)
}

get_policy_actions <- function(policy_data, policy, policy_full_history = TRUE){
  K <- policy_data$dim$K

  # checking policy input: must be a list of length K
  stopifnot(
    if(class(policy)[[1]] == "list")
      length(policy) == K
    else FALSE
  )

  pa <- function(stage){
    his <- get_stage_history(policy_data, stage = stage, full_history = policy_full_history)
    return(policy[[stage]](his))
  }

  policy_actions <- lapply(1:K, pa)
  policy_actions <- rbindlist(policy_actions)
  setkey(policy_actions, id, stage)

  return(policy_actions)
}

