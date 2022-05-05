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
    deterministic_reward_names <- paste("U_A", action_set, sep = "")
    missing_deterministic_reward_names <- deterministic_reward_names[!(deterministic_reward_names %in% names(stage_data))]
    if (length(missing_deterministic_reward_names) > 0){
      mes <- paste(missing_deterministic_reward_names, collapse = ", ")
      mes <- paste("Setting the deterministic reward(s) '", mes, "' to default value 0.", sep = "")
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
    if (!all(c("id") %in% names(baseline_data))) stop("'baseline_data' must contain id.")
    if (any(is.na(baseline_data$id))) stop("'baseline_data' id has missing values")
    if (anyDuplicated(baseline_data, by = key(baseline_data)) > 0) stop("'baseline_data' id contains duplicates.")

    # setting id as key:
    setkey(baseline_data, id)

    # comparing id:
    if (!all(unique(stage_data$id) == baseline_data$id)) stop("baseline_data id must match stage_data id.")

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
#' Otherwise, \code{data} is considered to be on long format; see Examples.
#' @param action Action variable name(s). Character vector or character string.
#' \itemize{
#'   \item{} A vector is valid for wide data. The length of the vector determines the number of stages (K).
#'   \item{} A string is valid for single stage wide data or long data.
#' }
#' @param covariates Stage specific covariate name(s). Character vector or named list of character vectors.
#' \itemize{
#'   \item{} A vector is valid for single stage wide data or long data.
#'   \item{} A list is valid for multiple stage wide data. Each element of the list must be a character vector corresponding to the number of stages (K).
#' }
#' @param utility  Utility/Reward variable name(s). Character string or vector.
#' \itemize{
  #'   \item{} A string is valid for long data and wide data with a single final utility.
  #'   \item{} A vector is valid for wide data with incremental rewards. Must be of length K+1; see Examples.
#' }
#' @param deterministic_rewards Deterministic reward variable name(s). Character vector or named list of character vectors.
#' \itemize{
#'   \item{}
#'   \item{}
#' }
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
                        baseline = NULL,
                        deterministic_rewards = NULL,
                        id = NULL, stage = NULL, event = NULL,
                        verbose = FALSE) {
  data <- check_data(data)
  if (!missing(baseline_data)){
    baseline_data <- check_data(baseline_data)
  }

  type <- tolower(type)
  if (any(type %in% c("wide"))) {
    if (!missing(baseline_data)){
      stop("When 'type'=wide' set 'baseline_data' to NULL and use 'baseline' instead.")
    }

    # formatting the wide data:
    md <- melt_wide_data(
      data,
      id = id,
      action = action,
      covariates = covariates,
      utility = utility,
      baseline = baseline,
      deterministic_rewards = deterministic_rewards
    )

    # constructing new policy_data object:
    pd <- new_policy_data(
      stage_data = md$stage_data,
      baseline_data = md$baseline_data,
      verbose = verbose
    )
  } else if (any(type %in% "long")) {

    ld <- format_long_data(
      data,
      baseline_data = baseline_data,
      id = id,
      action = action,
      stage = stage,
      event = event,
      utility = utility,
      verbose = verbose
    )

    # constructing policy data object
    pd <- new_policy_data(
      stage_data = ld$stage_data,
      baseline_data = ld$baseline_data,
      verbose = verbose)

  } else{
    stop("'type' must be either \"wide\ or \"long\".")
  }
  return(pd)
}

check_data <- function(data){
  name <- deparse(substitute(data))

  if (is.data.frame(data) | is.matrix(data)){
    data <- as.data.table(data)
  }
  if (!is.data.table(data)){
    mes <- paste("'", name, "' must be a data.table.", sep = "")
    stop(mes)
  }
  if (any(duplicated(names(data)))){
    mes <- paste("'", name, "' has duplicated variable names.", sep = "")
    stop(mes)
  }

  return(data)
}


# Wide data ---------------------------------------------------------------

melt_wide_data <- function(wide_data, id, action, covariates, baseline, utility, deterministic_rewards){
  wide_data <- copy(wide_data)

  ### checking the form of the variable inputs
  # id:
  if (!is.null(id)){
    if (!(is.character(id) & (length(id) == 1))){
      mes <- "'id' must be a character string."
      stop(mes)
    }
  }
  # action:
  if (is.list(action))
    action <- unlist(action)
  if (!is.vector(action) | !is.character(action))
    stop("'action' must be a vector or a list of type character.")
  # getting the number of stages:
  K <- length(action)
  # getting the action set:
  action_set <- sort(unique(unlist(wide_data[,..action])))
  # covariates:
  if (!(is.list(covariates) | is.vector(covariates)))
    stop("'covariates' must be a character vector or a list of character vectors.")
  covariates <- lapply(covariates, function(covar){
    if (is.list(covar))
      covar <- unlist(covar)
    if (!is.vector(covar) | !is.character(covar))
      stop("'covariates' must be a character vector or a list of character vectors.")
    if(length(covar) != K){
      mes <- paste("Each element in 'covariates' must have length ", K, ".", sep = "")
      stop(mes)
    }
    return(covar)
  })
  if (is.null(names(covariates))){
    if (K == 1)
      names(covariates) <- covariates
    else
      stop("'covariates' must be a named list in case of multiple actions.")
  }
  # baseline:
  if (!is.null(baseline)){
    baseline <- unlist(baseline)
    if (!is.character(baseline)){
      stop("'baseline' must be a character string or vector.")
    }
  }
  # utility:
  if (is.list(utility))
    utility <- unlist(utility)
  if (!is.vector(utility) | !is.character(utility))
    stop("'utility' must be a vector or a list of type character.")
  if (length(utility) != 1){
    if (length(utility) != (K+1)){
      mes <- "'utility' must either be a character string or a character vector of length "
      mes <- paste(mes, (K+1), ".", sep = "")
      stop(mes)
    }
  }
  # deterministic_rewards:
  if (!is.null(deterministic_rewards)){
    deterministic_rewards <- lapply(deterministic_rewards, function(du){
      if (is.list(du))
        du <- unlist(du)
      if (!is.vector(du) | !is.character(du))
        stop("'deterministic_rewards' must be a character vector or a list of character vectors.")
      return(du)
    })
    deterministic_reward_names <- paste("U_A", action_set, sep = "")
    if ((!all(names(deterministic_rewards) %in% deterministic_reward_names)) | (is.null(deterministic_rewards) & is.null(names(deterministic_rewards)))) {
      mes <- paste(deterministic_reward_names, collapse = "', '")
      mes <- paste("'deterministic_rewards' must be a named list with names in the set '", mes, "'.", sep = "")
      stop(mes)
    }
  }

  ### checking if 'data' contains all variable names:
  tmp <- unlist(c(id, action, covariates, baseline, utility, deterministic_rewards))
  if (!all(tmp %in% names(wide_data))){
    mes <- tmp[!(tmp %in% names(wide_data))]
    mes <- paste(mes, collapse = "\", \"")
    mes <- paste("Not found in data: \"", mes, "\".", sep = "")
    stop(mes)
  }
  rm(tmp)

  ### checking for duplicates
  tmp <- unlist(c(id, action, covariates, baseline, utility, deterministic_rewards))
  if (anyDuplicated(tmp)>0){
    mes <- tmp[anyDuplicated(tmp)]
    mes <- paste(mes, collapse = "\", \"")
    mes <- paste("Duplicated variables: \"", mes, "\".", sep = "")
    stop(mes)
  }
  rm(tmp)

  ### checking for invalid variable names:
  if (any(c("event", "stage") %in% names(covariates))){
    stop("'covariates' can not have named elements \"event\" or \"stage\".")
  }

  ### setting default variables if missing:
  # setting id if missing
  if (is.null(id)){
    if (any("id" %in% colnames(wide_data))){
      stop("'data' has a variable id, but 'id' = NULL. Please set 'id' = \"id\" or change the name of the id variable.")
    }
    # setting id:
    wide_data[, id := 1:.N]
    id <- "id"
  }
  # setting the rewards to 0 in case the final utility is provided.
  if (length(utility)==1) {
    for (i in seq(K)) {
      u <- paste0("_", utility[i], "_", i)
      stopifnot(!any(u %in% names(wide_data)))
      wide_data[, (u) := 0]
      utility <- c(u, utility)
    }
  }

  ### melting stage data:
  measure <- append(list(A = action), covariates)
  measure <- append(measure, list(U = utility))
  if (!is.null(deterministic_rewards)){
    measure <- append(measure, deterministic_rewards)
  }
  # selecting subset:
  sel <- unlist(c(id, action, covariates, utility, deterministic_rewards))
  stage_data <- subset(wide_data, select = sel)
  # converts to long data:
  stage_data <- melt(stage_data, id.vars = id, measure.vars = measure, variable.name = "stage")
  setnames(stage_data, id, "id")
  stage_data[ , stage := as.numeric(as.character(stage))]
  stage_data[, A := as.character(A)]
  # setting the event variable:
  stage_data[!is.na(A), event := 0]
  stage_data <- stage_data[!(is.na(A) & is.na(U)),]
  stage_data[is.na(A), event := 1]
  # setting keys:
  setkey(stage_data, id, stage)

  ### getting baseline data:
  if (!is.null(baseline)){
    sel <- c(id, baseline)
    baseline_data <- subset(wide_data, select = sel)
    setnames(baseline_data, id, "id")
    setkey(baseline_data, id)
  } else{
    baseline_data <- NULL
  }

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}


# Long data ---------------------------------------------------------------

# sets default by reference:
set_default_name <- function(data, variable_name, default_name){
  parsed_name <- deparse(substitute(variable_name))

  if (is.null(variable_name)){
    variable_name <- default_name
  }

  if (!(is.character(variable_name) & (length(variable_name)==1))){
    mes <- parsed_name
    mes <- paste("'", mes, "' must be a character string.", sep = "")
    stop(mes)
  }

  if (!(variable_name %in% names(data))){
    mes <- parsed_name
    mes <- paste("'", mes, "' is invalid.", sep = "")
    stop(mes)
  }

  if ((default_name %in% names(data)) & (variable_name != default_name)){
    mes <- paste("'data' has a variable called \"", default_name, "\", but '", sep = "")
    mes <- paste(mes, parsed_name, "' = \"", variable_name, "\".", sep = "")
    mes <- paste(mes,  " Please remove or rename '", parsed_name, "'.", sep ="")
    stop(mes)
  }

  setnames(data, variable_name, default_name)
}

format_long_data <- function(long_data, baseline_data, id, action, stage, event, utility, verbose){

  ### dealing with missing values
  if (missing(action)){
    action <- NULL
  }
  if (missing(utility)){
    utility <- NULL
  }
  if (missing(baseline_data)){
    baseline_data <- NULL
  }

  ### checking for duplicates
  tmp <- unlist(c(id, action, stage, event, utility))
  if (anyDuplicated(tmp)>0){
    mes <- tmp[anyDuplicated(tmp)]
    mes <- paste(mes, collapse = "\", \"")
    mes <- paste("Duplicated variables: \"", mes, "\".", sep = "")
    stop(mes)
  }
  rm(tmp)

  ### setting default variable names by reference in long_data:
  long_data <- copy(long_data)
  set_default_name(long_data, variable_name = id, default_name = "id")
  set_default_name(long_data, variable_name = action, default_name = "A")
  set_default_name(long_data, variable_name = stage, default_name = "stage")
  set_default_name(long_data, variable_name = event, default_name = "event")
  set_default_name(long_data, variable_name = utility, default_name = "U")

  ### searching for deterministic reward variables in long_data:
  action_set <- sort(unique(unlist(long_data[,"A", with = FALSE])))
  deterministic_reward_names <- paste("U_A", action_set, sep = "")
  drn <- names(long_data)[names(long_data) %in% deterministic_reward_names]
  if ((length(drn) > 0) & (verbose == TRUE)){
    mes <- paste("The variable(s) ", paste(drn, collapse = ", "), " in 'data' are considered deternistic rewards.", sep = "")
    message(mes)
  }

  if (!is.null(baseline_data)){
    baseline_data <- copy(baseline_data)
    set_default_name(baseline_data, variable_name = id, default_name = "id")
  }

  out <- list(
    stage_data = long_data,
    baseline_data = baseline_data
  )
  return(out)
}

# policy_data S3 functions -----------------------------------------------

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
