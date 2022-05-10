#' @export
full_stage_history <- function(object, stage)
  UseMethod("full_stage_history")

#' @export
full_stage_history.policy_data <- function(object, stage){

  if (stage > object$dim$K)
    stop("The stage number must be lower or equal to maximal number of stages observed.")

  stage_data <- object$stage_data
  stage_data_names <- object$colnames$stage_data_names
  baseline_data <- object$baseline_data
  baseline_data_names <- object$colnames$baseline_data_names
  action_set <- object$action_set
  deterministic_reward_names <- object$colnames$deterministic_reward_names
  stage_ <- stage; rm(stage)

  # getting stage specific history names:
  AH_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  AH <- stage_data[event == 0, ]
  # filtering rows up till the given stage number:
  AH <- AH[stage <= stage_, ..AH_names]
  # filtering observations with an action at the given stage:
  AH <- AH[, if(any(stage == stage_)) .SD, id]
  # transforming the data from long to wide format:
  AH <- dcast(AH, id ~ stage, value.var = AH_names[-c(1,2)])
  # inserting stage column:
  AH[, stage := stage_]
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    AH[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }
  # setting key and column order
  setkey(AH, id, stage)
  setcolorder(AH, neworder = c("id", "stage"))

  # getting the accumulated utility and deterministic utility contributions
  U <- stage_data[stage <= stage_][, U_bar := sum(U), id]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", deterministic_reward_names)
  U <- U[, ..U_names]

  action_name <- paste("A", stage_, sep = "_")
  # id_names <- c("id", "stage")
  # AH_names <- names(AH)[!(names(AH) %in% c(id_names, action_name))]

  history <- list(
    AH = AH,
    U = U,
    action_name = action_name,
    deterministic_reward_names = deterministic_reward_names,
    action_set = action_set,
    stage = stage_
  )
  class(history) <- "history"

  return(history)
}

#' @export
state_stage_history <- function(object, stage)
  UseMethod("state_stage_history")

#' @export
state_stage_history.policy_data <- function(object, stage){

  if (stage > object$dim$K)
    stop("The stage number must be lower or equal to maximal number of stages observed.")

  stage_data <- object$stage_data
  stage_data_names <- object$colnames$stage_data_names
  baseline_data <- object$baseline_data
  baseline_data_names <- object$colnames$baseline_data_names
  action_set <- object$action_set
  deterministic_reward_names <- object$colnames$deterministic_reward_names
  stage_ <- stage

  # getting stage specific history names:
  AH_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  AH <- stage_data[event == 0, ]
  # filtering observations with an action at the given stage:
  AH <- AH[stage == stage_, ..AH_names]
  # setting new names:
  # new_names <- paste(c("A", stage_data_names), stage_, sep = "_")
  # setnames(AH, old = c("A", stage_data_names), new = new_names)
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    AH[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }

  # getting the accumulated utility and deterministic utility contributions
  U <- stage_data[stage <= stage_][, U_bar := sum(U), id]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", deterministic_reward_names)
  U <- U[, ..U_names]

  id_names <- c("id", "stage")
  # action_name <- paste("A", stage_, sep = "_")
  # AH_names <- names(AH)[!(names(AH) %in% c(id_names, action_name))]

  history <- list(
    AH = AH,
    U = U,
    action_name = "A",
    deterministic_reward_names = deterministic_reward_names,
    action_set = action_set,
    stage = stage
  )
  class(history) <- "history"

  return(history)
}

#' @export
state_history <- function(object)
  UseMethod("state_history")

#' @export
state_history.policy_data <- function(object){
  stage_data <- object$stage_data
  stage_data_names <- object$colnames$stage_data_names
  baseline_data <- object$baseline_data
  baseline_data_names <- object$colnames$baseline_data_names
  action_set <- object$action_set

  # getting stage specific history names:
  AH_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  AH <- stage_data[event == 0, ]
  AH <- AH[, ..AH_names]
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    AH[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }

  # id_names <- c("id", "stage")
  action_name <- "A"
  # AH_names <- names(history)[!(names(history) %in% c(id_names, action_name))]

  history <- list(
    AH = AH,
    action_name = action_name,
    action_set = action_set
  )
  class(history) <- "history"

  return(history)
}

##' @export
get_H <- function(history, vars = NULL){
  AH <- history$AH
  action_name <- history$action_name

  # collecting the data:
  H_names <- names(AH)[!(names(AH) %in% c("id", "stage", action_name))]
  if (!is.null(vars)){
    if (is.character(vars)){
      if (!all(vars %in% H_names)){
        stop("Invalid selection of variables.")
      }
    } else
      stop("Selection of variables must be of type character.")
  } else
    vars <- H_names

  # H <- AH[, names(AH) %in% vars , with = FALSE] # keeps the original ordering of columns
  H <- AH[, ..vars] # vars dictates the ordering of the selected columns

  return(H)
}

#' @export
get_history_names <- function(policy_data, stage = NULL){
  if (is.null(stage)){
    history <- get_history(policy_data, stage = stage, full_history = FALSE)
  } else{
    history <- get_history(policy_data, stage = stage, full_history = TRUE)
  }
  AH <- history$AH
  action_name <- history$action_name
  history_names <- names(AH)[!(names(AH) %in% c("id", "stage", action_name))]
  return(history_names)
}

##' @export
get_A <- function(object)
  UseMethod("get_A")

##' @export
get_A.history <- function(object){
  AH <- object$AH
  action_name <- object$action_name

  A <- AH[[action_name]]

  return(A)
}

##' @export
get_id.history <- function(object){
  id <- object$AH$id
  return(id)
}

##' @export
get_id_stage <- function(object)
  UseMethod("get_id_stage")

##' @export
get_id_stage.history <- function(object){
  AH <- object$AH
  id_stage_names <- c("id", "stage")

  id_stage <- AH[, ..id_stage_names]

  return(id_stage)
}

#' @export
get_history <- function(object, stage = NULL, full_history = FALSE)
  UseMethod("get_history")

#' @export
get_history.policy_data <- function(object, stage = NULL, full_history = FALSE){
  if (full_history == TRUE){
    if (is.null(stage)) stop("Please provide a stage number.")
    his <- full_stage_history(object, stage = stage)
  } else{
    if (is.null(stage)){
      his <- state_history(object)
    } else{
      his <- state_stage_history(object, stage = stage)
    }
  }
  return(his)
}
