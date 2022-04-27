

# \code{policy_data} creates an object of class policy_data.
#
# @param stage_data A data.table on long format with required columns:
# \itemize{
#  \item{id}
#  \item{stage: }{stage number of type integer.}
#  \item{event: }{0 indicating an action stage, 1 indicating a terminal stage and 2 indicating a censoring stage.}
#  \item{A: }{action variable of type character.}
#  \item{U: }{reward variable of type numeric.}
# }
# It is possible to add deterministic reward variables U_a for every action a in the action set, see details.
# The remaining columns are considered state variables (X).
#

#'
#' Create Policy Data Object
#'
#' \code{policy_data} creates an object of class "policy_data".
#'
#' @param data [data.frame] or [data.table]; see Examples.
#' @param baseline_data [data.frame] or [data.table]; see Examples.
#' @param type Character string. If "wide", \code{data} is considered to be on wide format.
#' If "long", \code{data} is considered to be on long format; see Examples.
#' @param action Action variable name. Character string or character vector (one for each stage).
#' @param covariates Stage specific covariate name(s). Character vector or named list of character vectors.
#' \itemize{
#'   \item{} A vector is only valid for single stage wide data or long data.
#'   \item{} A named list is valid for multiple stage wide data. Each element of the list must be a character vector corresponding to the number of stages (K).
#' }
#' @param utility  Utility/reward variable name(s). Character string or vector.
#' \itemize{
  #'   \item{} A string is valid for long data and wide data with a single final utility/reward.
  #'   \item{} A vector is valid for wide data with incremental utility/rewards. Must be of length K+1; see Examples.
#' }
#'
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item{} B is a vector of baseline information.
#'  \item{} U_k is the reward at stage k.
#'  \item{} X_k is a vector summarizing the state at stage k.
#'  \item{} A_k is the categorical action at stage k.
#' }
#' The utility is given by the sum of the rewards, i.e., \eqn{U = sum_{k = 1}^{K+1} U_k}.
#' @examples
#'
#' @export
policy_data <- function(data, baseline_data,
                        type="wide",
                        action, covariates, utility,
                        id = NULL, deterministic_utility = NULL,
                        verbose = FALSE) {
  if (is.data.frame(data)) data <- as.data.table(data)
  type <- tolower(type)
  if (type %in% c("wide")) {
    pd <- wide_stage_data_to_long(data,
                                  action = action,
                                  covariates = covariates,
                                  utility = utility,
                                  id = id,
                                  deterministic_utility = deterministic_utility)
    res <- new_policy_data(pd, baseline_data = NULL, verbose = verbose) # ADD BASELINE
  } else {
    res <- new_policy_data(data, baseline_data = baseline_data, verbose = verbose)
  }
  return(res)
}

new_policy_data <- function(stage_data, baseline_data = NULL, verbose){

  # checking and processing stage_data:
  {
    if (missing(stage_data)) stage_data <- NULL
    if (is.null(stage_data)) stop("stage_data is missing or NULL.")
    if (is.data.frame(stage_data)) stage_data <- as.data.table(stage_data)
    if (!is.data.table(stage_data)) stop("stage_data must be a data.table or a data.frame.")
    if (!all(c("id", "stage", "event", "A", "U") %in% colnames(stage_data))) stop("stage_data must contain id, stage, event, A (action) and U (utility/reward).")

    # copying stage_data in order to avoid reference issues:
    stage_data <- copy(stage_data)
    setcolorder(stage_data, c("id", "stage", "event", "A"))

    # setting and checking keys:
    setkey(stage_data, id, stage)
    if (any(is.na(stage_data$id))) stop("id has missing values.")
    if (any(is.na(stage_data$stage))) stop("stage has missing values.")
    if (anyDuplicated(stage_data, by = key(stage_data)) > 0) stop("The combination of id & stage must be unique.")
    if (!all(stage_data[ , .(check = all(stage == 1:.N)), by = id]$check)) stop("stage must be on the form 1, 2, ..., k.")

    # checking the event variable
    if (any(is.na(stage_data$event))) stop("event have missing values.")
    if (!all(stage_data[, .(check = all(event == c(rep(0, times = (.N-1)), 1) | event == c(rep(0, times = (.N-1)), 2))), id]$check)) stop("event must be on the form 0,0,...,0,j (j in {1,2}).")

    # checking the action variable (A):
    if (!is.character(stage_data$A)){
      message("Coercing A to type character.")
      stage_data$A <- as.character(stage_data$A)
      }

    # getting the set of actions (A):
    action_set <- sort(unique(stage_data$A))

    # checking the utility variable (U):
    if (!all(is.numeric(stage_data$U))) stop("The utility (U) must be numeric.")
    if(any(is.na(stage_data$U))) stop("The utility (U) has missing values")

    # checking the deterministic reward variables (U_A[.]):
    # action_utility_names
    deterministic_reward_names <- paste("U_A", action_set, sep = "")
    # missing_action_utililty_names
    missing_deterministic_reward_names <- deterministic_reward_names[!(deterministic_reward_names %in% names(stage_data))]
    if (length(missing_deterministic_reward_names) > 0){
      mes <- paste(missing_deterministic_reward_names, collapse = ", ")
      mes <- paste("Setting the deterministic reward '", mes, "' in the stage data to default value 0.", sep = "")
      if (verbose == TRUE){
        message(mes)
      }
      stage_data[, (missing_deterministic_reward_names) := 0]
    }
    if (!all(sapply(stage_data[, ..deterministic_reward_names], function(col) is.numeric(col)))){
      mes <- paste(deterministic_reward_names, collapse = ", ")
      mes <- paste("'",mes, "'must be numeric.", sep = " ")
      stop(mes)
    }

    # checking missing values for action stages (event == 0):
    sd <- stage_data[event == 0, -c("id", "stage", "event", "A"), with = FALSE]
    sd_names <- colnames(sd)
    sdm <- sapply(sd, function(x) any(is.na(x)))
    if (any(sdm)){
      mes <- sd_names[sdm]
      mes <- paste(mes, collapse = ", ")
      mes <- paste(mes, " has missing values.", sep = "")
      stop(mes)
    }
    rm(sd, sd_names, sdm)


    # coercing columns of type factor to type character in stage_data:
    sdf <- sapply(stage_data, function(x) is.factor(x))
    if (any(sdf)){
      f_names <- colnames(stage_data)[sdf]
      mes <- f_names
      mes <- paste(mes, collapse = ", ")
      mes <- paste("Coercing '", mes, "' to type character.", sep = "")
      if (verbose == TRUE){
      message(mes)
      }
      stage_data[, (f_names) := lapply(.SD, as.character), .SDcols = f_names]
      rm(mes, f_names)
    }
    rm(sdf)

    # getting the names of the state data (X_k):
    rn <- c("id", "stage", "event", "A", "U", deterministic_reward_names)
    stage_data_names <- names(stage_data)[!(names(stage_data) %in% rn)]
  }

  # checking and processing baseline_data:
  baseline_data_names <- NULL
  if (!is.null(baseline_data)){
    if (is.data.frame(baseline_data)) baseline_data <- as.data.table(baseline_data)

    # copying baseline_data in order to avoid reference issues:
    baseline_data <- copy(baseline_data)

    # checking id:
    if (!all(c("id") %in% names(baseline_data))) stop("baseline_data must contain id.")
    if (any(is.na(baseline_data$id))) stop("baseline_data id has missing values")
    if (anyDuplicated(baseline_data, by = key(baseline_data)) > 0) stop("baseline id contains duplicates.")
    if (!all(unique(stage_data$id) == baseline_data$id)) stop("baseline_data id must match stage_data id.")

    # setting id as key:
    setkey(baseline_data, id)

    # Coercing columns of type factor to type character in baseline_data:
    bdf <- sapply(baseline_data, function(x) is.factor(x))
    if (any(bdf)){
      f_names <- colnames(baseline_data)[bdf]
      mes <- f_names
      mes <- paste(mes, collapse = ", ")
      mes <- paste("Coercing ", mes, " to type character.", sep = "")
      if (verbose == TRUE)
        message(mes)
      baseline_data[, (f_names) := lapply(.SD, as.character), .SDcols = f_names]
      rm(mes, f_names)
    }
    rm(bdf)

    # getting the names of the baseline state data:
    baseline_data_names <- names(baseline_data)[!(names(baseline_data) %in% c("id"))]
  } else {
    baseline_data <- stage_data[, .(id =unique(id))]
  }

  # getting the dimensions:
  n <- length(unique(stage_data$id))
  K <- stage_data[event == 0, .(max(stage))][[1]]

  # checking that all actions in the action set are observed at every stage:
  # if (!all(stage_data[event == 0, .(check = all(sort(unique(A)) == action_set)), stage]$check))
  #   stop("Every action in the action must be observed at every stage.")

  object <- list(
    stage_data = stage_data,
    baseline_data = baseline_data,
    colnames = list(
      stage_data_names = stage_data_names,
      deterministic_reward_names = deterministic_reward_names,
      baseline_data_names = baseline_data_names
    ),
    action_set = action_set,
    dim = list(
      n = n,
      K = K
    )
  )

  class(object) <- "policy_data"

  return(object)
}

# long_stage_data <- function(stage_data, id = NULL, A_cols, X_cols, U_cols, U_A_cols = NULL){
#
# }

wide_stage_data_to_long <- function(wide_data, id, action, covariates, utility, deterministic_utility){
  # number of stages:
  K <- length(action)

  # converting to data.table:
  if (is.data.frame(wide_data)) wide_data <- as.data.table(wide_data)
  wide_data <- copy(wide_data)

  # checking the ID variable:
  if (!is.null(id) & !is.character(id))
    stop("'id' is not a character.")
  if (any("id" %in% colnames(wide_data)) && !is.null(id))
    stop("The data has a variable called 'id', but id = NULL. Please set id = 'id' or change the name of the variable.")
  if (is.null(id)){
    # creating an ID variable:
    wide_data[, id := 1:.N]
    id <- "id"
  }

  # checking the action variable:
  if (is.list(action))
    action <- unlist(action)
  if (!is.vector(action) | !is.character(action))
    stop("action must be a vector or a list of type character.")
  if (!all(action %in% names(wide_data))){
    stop("One or more values in 'action' is invalid.")
  }

  # checking the covariates variable:
  if (!(is.list(covariates) | is.vector(covariates)))
    stop("covariates must be a character vector or a list of character vectors.")
  if (!all(unlist(covariates) %in% names(wide_data))){
    stop("One or more values in 'covariates' is invalid.")
  }
  covariates <- lapply(covariates, function(covar){
    if (is.list(covar))
      covar <- unlist(covar)
    if (!is.vector(covar) | !is.character(covar))
      stop("covariates must be a character vector or a list of character vectors.")

    if(length(covar) != K)
      stop("Each element in 'covariates' must have the same length as 'action'.")
    return(covar)
  })
  if (is.null(names(covariates))){
    if (K == 1)
      names(covariates) <- covariates
    else
      stop("covariates must be a named list when K >1.")
  }

  # checking the utility variable
  if (is.list(utility))
    utility <- unlist(utility)
  if (!is.vector(utility) | !is.character(utility))
    stop("'utility' must be a vector or a list of type character.")
  if (!all(utility %in% names(wide_data))){
    stop("One or more values in 'utility' is invalid.")
  }
  if (length(utility) != 1){
    if (length(utility) != (K+1)){
      mes <- "'utility' must either be a character string or a character vector of length "
      mes <- paste(mes, (K+1), ".", sep = "")
      stop(mes)
    }
  }
  # setting the rewards to 0 if only the final utility is provided.
  if (length(utility)==1) {
    for (i in seq(K)) {
      u <- paste0("_", utility[i], "_", i)
      wide_data[, (u) := 0]
      utility <- c(u, utility)
    }
  }

  # checking the deterministic utility variable:
  deterministic_utility <- lapply(deterministic_utility, function(du){
    if (is.list(du))
      du <- unlist(du)
    if (!is.vector(du) | !is.character(du))
      stop("deterministic_utility must be a character vector or a list of character vectors.")
    return(du)
  })

  # checking for overlapping values:
  if (length(intersect(action, unlist(covariates)))>0)
    stop("'action' and 'covariates' have overlapping values.")

  # checking for duplicate values:
  if (length(unique(action)) != length(action))
    stop("'action' has duplicate values.")
  if (length(unique(unlist(covariates))) != length(unlist(covariates)))
    stop("'covariates' has duplicate values.")
  if (length(unique(unlist(deterministic_utility))) != length(unlist(deterministic_utility)))
    stop("'deterministic_utility' has duplicate values.")
  if (length(unique(unlist(utility))) != length(unlist(utility)))
    stop("'utility' has duplicate values.")

  measure <- append(list(A = action), covariates)
  measure <- append(measure, list(U = utility))
  if (!is.null(deterministic_utility)){
    measure <- append(measure, deterministic_utility)
  }

  # subset data:
  sel <- c(id, action, unlist(covariates), unlist(utility), unlist(deterministic_utility))
  wide_data <- subset(wide_data, select = sel)

  # convert to long data:
  long_stage_data <- melt(wide_data, id.vars = id, measure = measure, variable.name = "stage")
  setnames(long_stage_data, id, "id")
  long_stage_data[ , stage := as.numeric(as.character(stage))]
  long_stage_data[, A := as.character(A)]

  long_stage_data[!is.na(A), event := 0]
  long_stage_data <- long_stage_data[!(is.na(A) & is.na(U)),]
  long_stage_data[is.na(A), event := 1]

  setkey(long_stage_data, id, stage)

  return(long_stage_data)
}

#' @export
print.policy_data <- function(x, digits = 2, ...){
  K <- get_K(x)
  n <- get_n(x)

  cat(
    paste("Object of class policy_data with n = ", n, " observations and maximum K = ", K, " stages.", sep = "")
  )
  cat("\n")

  action_set <- get_action_set(x)

  cat("\n")
  st <- x$stage_data[event == 0,][, c("stage", "A"), with = FALSE]
  colnames(st) <- c("stage", "action")
  stable <- addmargins(table(st), 2, FUN = list(n = sum))

  print(stable)

  cat("\n")
  bc <- paste(x$colnames$baseline_data_names, collapse = ", ")
  cat(
    paste("Baseline covariates: ", bc, sep = "")
  )
  cat("\n")
  sc <- paste(x$colnames$stage_data_names, collapse = ", ")
  cat(
    paste("State covariates: ", sc, sep = "")
  )
  cat("\n")
  mean_utility <- mean(utility(x)$U)
  mean_utility <- round(mean_utility, digits = digits)

  cat(
    paste("Average utility: ", mean_utility, sep = "")
  )
  cat("\n")
}
