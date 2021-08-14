#' @export
copy_policy_data <- function(object){

  object$stage_data <- copy(object$stage_data)
  object$baseline_data <- copy(object$baseline_data)

  return(object)
}

partial_stage_data <- function(stage_data, K, action_utility_names){
  if (is.data.frame(stage_data))
    stage_data <- as.data.table(stage_data)

  required_names <- c("id", "stage", "event", "A", "U")

  # filtering stage_data rows up till stage K:
  stage_data_K <- stage_data[stage <= K, ]

  # filtering stage_data rows for stages above K and the required columns:
  stage_data_res <- stage_data[stage > K, ..required_names]

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
  if (!is.null(action_utility_names))
    stage_data_res_sum[, (action_utility_names) := NA]
  # binding stage_data_K with stage_data_res_sum
  stage_data <- rbindlist(list(stage_data_K, stage_data_res_sum), fill = TRUE, use.names = TRUE)

  # setting keys and index
  setkey(stage_data, id, stage)
  setindex(stage_data, event)

  return(stage_data)
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

  # column names of the deterministic rewards:
  action_utility_names <- object$colnames$action_utility_names

  object$stage_data <- partial_stage_data(stage_data = object[["stage_data"]], K = K, action_utility_names = action_utility_names)
  object$dim$K <- K

  return(object)
}

#' @export
subset.policy_data <- function(x, id){
  if (!all(id %in% get_id(x))) stop("Invalid subset of IDs.")
  id_ <- id; rm(id)

  spd <- new_policy_data(
    stage_data = x$stage_data[id %in% id_],
    baseline_data = x$baseline_data[id %in% id_]
  )

  return(spd)
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

#' @export
get_actions <- function(object)
  UseMethod("get_actions")

#' @export
get_actions.policy_data <- function(object){
  actions <- object$stage_data[event == 0, c("id", "stage", "A"), with = FALSE]
  return(actions)
}

#' @export
get_id <- function(object)
  UseMethod("get_id")

#' @export
get_id.policy_data <- function(object){
  id <- unique(object$stage_data$id)
  return(id)
}

#' @export
get_id_stage.policy_data <- function(object){
  stage_data <- object$stage_data

  id_stage_names <- c("id", "stage")

  id_stage <- stage_data[event == 0, ][, ..id_stage_names]

  return(id_stage)
}

#' @export
get_K <- function(object)
  UseMethod("get_K")
#' @export
get_K.policy_data <- function(object){
  K <- object$dim$K
  return(K)
}

#' @export
get_n <- function(object)
  UseMethod("get_n")
#' @export
get_n.policy_data <- function(object){
  n <- object$dim$n
  return(n)
}

#' @export
get_action_set <- function(object)
  UseMethod("get_action_set")
#' @export
get_action_set.policy_data <- function(object){
  action_set <- object$action_set
  return(action_set)
}



