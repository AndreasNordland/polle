#' Get the history matrix H from a history object
#'
#' @param history Object of class "history".
#' @param vars Character vector. Subset of the history matrix.
#' @return [data.table] H
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

#' Get the action vector A from a history object
#'
#' @param history Object of class "history".
#' @return Character vector.
get_A <- function(history){
  AH <- history$AH
  action_name <- history$action_name

  A <- AH[[action_name]]

  return(A)
}

#' Get the ID vector from a history object
#'
#' @param history Object of class "history".
#' @return Character vector.
get_id.history <- function(object){
  id <- object$AH$id
  return(id)
}

#' Get the IDs of the history object
#'
#' @param object Object of class "history".
#' @return [data.table] with variables id and stage.
get_id_stage.history <- function(object){
  AH <- object$AH
  id_stage_names <- c("id", "stage")

  id_stage <- AH[, ..id_stage_names]

  return(id_stage)
}
