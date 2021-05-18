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
  action_utility_names <- object$colnames$action_utility_names
  stage_ <- stage; rm(stage)

  # getting stage specific history names:
  H_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  H <- stage_data[event == 0, ]
  # filtering rows up till the given stage number:
  H <- H[stage <= stage_, ..H_names]
  # filtering observations with an action at the given stage:
  H <- H[, if(any(stage == stage_)) .SD, id]
  # transforming the data from long to wide format:
  H <- dcast(H, id ~ stage, value.var = H_names[-c(1,2)])
  # inserting stage column:
  H[, stage := stage_]
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    H[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }
  # setting key and column order
  setkey(H, id, stage)
  setcolorder(H, neworder = c("id", "stage"))

  # getting the accumulated utility and deterministic utility contributions
  U <- stage_data[stage <= stage_][, U_bar := sum(U), id]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", action_utility_names)
  U <- U[, ..U_names]


  action_name <- paste("A", stage_, sep = "_")
  # id_names <- c("id", "stage")
  # H_names <- names(H)[!(names(H) %in% c(id_names, action_name))]

  history <- list(
    H = H,
    U = U,
    action_name = action_name,
    action_utility_names = action_utility_names,
    action_set = action_set
  )
  class(history) <- "history"

  return(history)
}

#' @export
markov_stage_history <- function(object, stage)
  UseMethod("markov_stage_history")

#' @export
markov_stage_history.policy_data <- function(object, stage){

  if (stage > object$dim$K)
    stop("The stage number must be lower or equal to maximal number of stages observed.")

  stage_data <- object$stage_data
  stage_data_names <- object$colnames$stage_data_names
  baseline_data <- object$baseline_data
  baseline_data_names <- object$colnames$baseline_data_names
  action_set <- object$action_set
  action_utility_names <- object$colnames$action_utility_names
  stage_ <- stage

  # getting stage specific history names:
  H_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  H <- stage_data[event == 0, ]
  # filtering observations with an action at the given stage:
  H <- H[stage == stage_, ..H_names]
  # setting new names:
  new_names <- paste(c("A", stage_data_names), stage_, sep = "_")
  setnames(H, old = c("A", stage_data_names), new = new_names)
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    H[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }

  # getting the accumulated utility and deterministic utility contributions
  U <- stage_data[stage <= stage_][, U_bar := sum(U), id]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", action_utility_names)
  U <- U[, ..U_names]

  id_names <- c("id", "stage")
  action_name <- paste("A", stage_, sep = "_")
  # H_names <- names(H)[!(names(H) %in% c(id_names, action_name))]

  history <- list(
    H = H,
    U = U,
    action_name = action_name,
    action_utility_names = action_utility_names,
    action_set = action_set
  )
  class(history) <- "history"

  return(history)
}

#' @export
markov_history <- function(object)
  UseMethod("markov_history")

#' @export
markov_history.policy_data <- function(object){
  stage_data <- object$stage_data
  stage_data_names <- object$colnames$stage_data_names
  baseline_data <- object$baseline_data
  baseline_data_names <- object$colnames$baseline_data_names
  action_set <- object$action_set

  # getting stage specific history names:
  H_names <- c("id", "stage", "A", stage_data_names)
  # filtering rows which have an action (event = 0):
  H <- stage_data[event == 0, ]
  H <- H[, ..H_names]
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_data_names) > 0){
    H[baseline_data, (baseline_data_names) := mget(paste0('i.', baseline_data_names))]
  }

  # id_names <- c("id", "stage")
  action_name <- "A"
  # H_names <- names(history)[!(names(history) %in% c(id_names, action_name))]

  history <- list(
    H = H,
    action_name = action_name,
    action_set = action_set
  )
  class(history) <- "history"

  return(history)
}

get_X <- function(object)
  UseMethod("get_X")

get_X.history <- function(object){
  H <- object$H
  action_name <- object$action_name

  # collecting the data:
  X_names <- names(H)[!(names(H) %in% c("id", "stage", action_name))]
  X <- H[, ..X_names]

  # dropping character columns with 1 unique element (1 level)
  character_cols <- names(X)[sapply(X, is.character)]
  dn <- character_cols[(sapply(X[, ..character_cols], function(x) length(unique(x))) == 1)]
  if (length(dn) > 0)
    X[, (dn) := NULL]

  # dropping numeric columns with 1 unique value
  numeric_cols <- names(X)[sapply(X, is.numeric)]
  dn <- numeric_cols[(sapply(X[, ..numeric_cols], function(x) length(unique(x))) == 1)]
  if (length(dn) > 0)
    X[, (dn) := NULL]

  # constructing the model matrix (without an intercept)
  X <- model.matrix(~., X)[, -1]

  return(X)
}

get_A <- function(object)
  UseMethod("get_A")

get_A.history <- function(object){
  H <- object$H
  action_name <- object$action_name

  A <- H[[action_name]]

  return(A)
}

get_id_stage <- function(object)
  UseMethod("get_id_stage")

get_id_stage.history <- function(object){
  H <- object$H
  id_names <- c("id", "stage")

  id <- H[, ..id_names]

  return(id)
}

#' @export
get_stage_history <- function(object, stage, full_history)
  UseMethod("get_stage_history")

#' @export
get_stage_history.policy_data <- function(object, stage, full_history){
  if (full_history == TRUE){
    his <- full_stage_history(object, stage = stage)
  } else{
    his <- markov_stage_history(object, stage = stage)
  }
  return(his)
}
