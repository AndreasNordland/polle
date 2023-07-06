#' Get the history matrix H from a history object
#'
#' @param history Object of class "history".
#' @param vars Character vector. Subset of the history matrix.
#' @return [data.table] H
#' @noRd
get_H <- function(history, vars = NULL){

  H <- getElement(history, "H")
  stopifnot(!is.null(H))

  # collecting the data:
  H_names <- names(H)[!(names(H) %in% c("id", "stage"))]
  if (!is.null(vars)){
    if (is.character(vars)){
      if (!all(vars %in% H_names)){
        stop("Invalid selection of variables.")
      }
    } else
      stop("Selection of variables must be of type character.")
  } else
    vars <- H_names

  # vars dictates the ordering of the selected columns
  H <- H[, vars, with = FALSE]

  return(H)
}

#' Get the action vector A from a history object
#'
#' @param history Object of class "history".
#' @return Character vector.
#' @noRd
get_A <- function(history){
  A <- getElement(history, "A")
  stopifnot(!is.null(A))
  action_name <- getElement(history, "action_name")
  stopifnot(!is.null(action_name))

  A <- A[[action_name]]

  return(A)
}

#' @export
get_id.history <- function(object){
  H <- getElement(object, "H")
  stopifnot(!is.null(H))
  id <- H[, "id"]
  stopifnot(!is.null(id))
  return(id)
}

#' @export
get_id_stage.history <- function(object){
  H <- getElement(object, "H")
  stopifnot(!is.null(H))
  id_stage <- c("id", "stage")
  id_stage <- H[, id_stage, with = FALSE]

  return(id_stage)
}
