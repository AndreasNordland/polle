new_policy_data <- function(stage_data, baseline_data = NULL, action_set = NULL, stage_action_sets = NULL, verbose){

  # checking and processing stage_data:
  {
    if (missing(stage_data)) stage_data <- NULL
    if (is.null(stage_data)) stop("'stage_data' is missing or NULL.")
    if (is.data.frame(stage_data)) stage_data <- as.data.table(stage_data)
    if (!is.data.table(stage_data))
      stop("'stage_data' must be a data.table or a data.frame.")
    if (!all(c("id", "stage", "event", "A", "U") %in% colnames(stage_data)))
      stop("'stage_data' must contain id, stage, event, A (action) and U (utility/reward).")
    setcolorder(stage_data, c("id", "stage", "event", "A"))

    # setting and checking keys:
    setkeyv(stage_data, c("id", "stage"))
    if (any(is.na(stage_data[, "id"]))) stop("id has missing values.")
    if (any(is.na(stage_data[, "stage"]))) stop("stage has missing values.")
    if (anyDuplicated(stage_data, by = key(stage_data)) > 0)
      stop("The combination of id & stage must be unique.")
    stage <- NULL
    if (!all(stage_data[ , list("check" = all(stage == 1:.N)), by = "id"][,"check"]))
      stop("stage must be on the form 1, 2, ..., k.")
    rm(stage)

    # checking the event variable:
    if (any(is.na(stage_data[,"event"]))) stop("event have missing values.")
    event <- NULL
    if (!all(stage_data[,list("check" = all(event == c(rep(0, times = (.N-1)), 1) | event == c(rep(0, times = (.N-1)), 2))),by = "id"][, "check"]))
      stop("event must be on the form 0,0,...,0,j (j in {1,2}).")
    rm(event)

    # checking the action variable (A):
    event <- NULL
    A <- NULL
    if (any(is.na(stage_data[event == 0, ][,"A"])))
      stop("'action' (A) has missing values.")
    if (!is.character(stage_data[,"A"])){
      stage_data[, A := as.character(A)]
    }
    rm(event, A)

    # getting the global set of actions (A):
    obs_actions <- sort(unlist(unique(stage_data[,"A"])))
    if (!is.null(action_set)){
      action_set <- as.character(action_set)
      if (!all(obs_actions %in% action_set))
        stop("The given action set does not include all observed actions.")
      action_set <- sort(action_set)
    } else{
      action_set <- obs_actions
    }
    action_set <- unname(action_set)

    # getting the action set at each stage:
    event <- NULL
    tmp <- stage_data[event == 0, list(set = list(sort(unique(A)))), stage]
    obs_stage_action_sets <- lapply(tmp$set, c)
    names(obs_stage_action_sets) <- paste("stage_", tmp$stage, sep = "")
    rm(event, tmp)
    if (!is.null(stage_action_sets)){
      if(length(stage_action_sets) != length(obs_stage_action_sets))
        stop("the given stage action set does not comply with the observed stage action sets.")
      for (k in seq_along(stage_action_sets)){
        if (!all(obs_stage_action_sets[[k]] %in% stage_action_sets[[k]]))
          stop("The given stage action sets do not include all observed stage actions.")
        stage_action_sets <- lapply(stage_action_sets, sort)
      }

    } else {
      stage_action_sets <- obs_stage_action_sets
    }

    # checking the utility variable (U):
    if (!all(is.numeric(unlist(stage_data[,"U"]))))
      stop("'utility' U must be numeric.")
    if(any(is.na(stage_data[,"U"])))
      stop("The utility varible U has missing values")

    # checking the deterministic reward variables (U_A[.]):
    deterministic_reward_names <- paste("U_A", action_set, sep = "")
    missing_deterministic_reward_names <-
      deterministic_reward_names[!(deterministic_reward_names %in% names(stage_data))]
    if (length(missing_deterministic_reward_names) > 0){
      mes <- paste(missing_deterministic_reward_names, collapse = ", ")
      mes <- paste("Setting the deterministic rewards '", mes, "' to default value 0.", sep = "")
      if (verbose == TRUE){
        message(mes)
      }
      stage_data[, (missing_deterministic_reward_names) := 0]
    }
    if (!all(sapply(stage_data[, deterministic_reward_names, with = FALSE], function(col) is.numeric(col)))){
      mes <- paste(deterministic_reward_names, collapse = ", ")
      mes <- paste("'",mes, "'must be numeric.", sep = " ")
      stop(mes)
    }

    # coercing variables of type factor to type character in stage_data:
    sdf <- sapply(stage_data, function(x) is.factor(x))
    if (any(sdf)){
      f_names <- colnames(stage_data)[sdf]
      stage_data[, (f_names) := lapply(.SD, as.character), .SDcols = f_names]
      rm(f_names)
    }
    rm(sdf)

    # getting the names of the state data (X_k):
    rn <- c("id", "stage", "event", "A", "U", deterministic_reward_names)
    state_names <- names(stage_data)[!(names(stage_data) %in% rn)]
  }

  # checking and processing baseline_data:
  {
    baseline_names <- NULL
    if (!is.null(baseline_data)){
      if (is.data.frame(baseline_data))
        baseline_data <- as.data.table(baseline_data)

      # checking id:
      if (!all(c("id") %in% names(baseline_data))) stop("'baseline_data' must contain id.")
      if (any(is.na(baseline_data$id))) stop("'baseline_data' id has missing values")
      if (anyDuplicated(baseline_data, by = key(baseline_data)) > 0) stop("'baseline_data' id contains duplicates.")

      # setting id as key:
      setkeyv(baseline_data, "id")

      # comparing id:
      if (!all(unique(unlist(stage_data[, "id"])) == unlist(baseline_data[,"id"])))
          stop("'baseline_data' id must match 'stage_data' id.")

      # coercing columns of type factor to type character in baseline_data:
      bdf <- sapply(baseline_data, function(x) is.factor(x))
      if (any(bdf)){
        f_names <- colnames(baseline_data)[bdf]
        baseline_data[, (f_names) := lapply(.SD, as.character), .SDcols = f_names]
        rm(f_names)
      }
      rm(bdf)

      # getting the names of the baseline state data:
      baseline_names <- names(baseline_data)[!(names(baseline_data) %in% c("id"))]
    } else {
      id <- NULL
      baseline_data <- stage_data[, list(id = unique(id))]
      rm(id)
    }
  }

  # getting the dimensions:
  n <- length(unique(unlist(stage_data[,"id"])))
  stage <- NULL
  K <- stage_data[event == 0, list(max(stage))][[1]]
  rm(stage)

  object <- list(
    stage_data = stage_data,
    baseline_data = baseline_data,
    colnames = list(
      state_names = state_names,
      deterministic_rewards = deterministic_reward_names,
      baseline_names = baseline_names
    ),
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    dim = list(
      n = n,
      K = K
    )
  )

  class(object) <- "policy_data"

  return(object)
}

#' @title Create Policy Data Object
#'
#' @description  \code{policy_data()} creates a policy data object which
#' is used as input to [policy_eval()] and [policy_learn()] for policy
#' evaluation and data adaptive policy learning.
#'
#' @param data [data.frame] or [data.table]; see Examples.
#' @param baseline_data [data.frame] or [data.table]; see Examples.
#' @param type Character string. If "wide", \code{data} is considered to be on wide format.
#' If "long", \code{data} is considered to be on long format; see Examples.
#' @param action Action variable name(s). Character vector or character string.
#' \itemize{
#'   \item{} A vector is valid for wide data. The length of the vector determines the number of stages (K).
#'   \item{} A string is valid for single stage wide data or long data.
#' }
#' @param covariates Stage specific covariate name(s). Character vector or named list of character vectors.
#' \itemize{
#'   \item{} A vector is valid for single stage wide data or long data.
#'   \item{} A named list is valid for multiple stages wide data. Each element
#'   must be a character vector with length K. Each vector can contain NA
#'   elements, if a covariate is not available for the given stage(s).
#' }
#' @param utility  Utility/Reward variable name(s). Character string or vector.
#' \itemize{
  #'   \item{} A string is valid for long data and wide data with a single final utility.
  #'   \item{} A vector is valid for wide data with incremental rewards. Must have length K+1; see Examples.
#' }
#' @param baseline Baseline covariate name(s). Character vector.
#' @param deterministic_rewards Deterministic reward variable name(s). Named list of character vectors of length K.
#' The name of each element must be on the form "U_Aa" where "a" corresponds to an action in the action set.
#' @param id ID variable name. Character string.
#' @param stage Stage number variable name.
#' @param event Event indicator name.
#' @param action_set Character string. Action set across all stages.
#' @param verbose Logical. If TRUE, formatting comments are printed to the console.
#' @param digits Minimum number of digits to be printed.
#' @param x Object to be printed.
#' @param ... Additional arguments passed to print.
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item{} \eqn{B} is a vector of baseline covariates.
#'  \item{} \eqn{U_k} is the reward at stage k (not influenced by the action \eqn{A_k}).
#'  \item{} \eqn{X_k} is a vector of state covariates summarizing the state at stage k.
#'  \item{} \eqn{A_k} is the categorical action at stage k.
#' }
#' The utility is given by the sum of the rewards, i.e.,
#' \eqn{U = \sum_{k = 1}^{K+1} U_k}.
#' @returns
#' \code{policy_data()} returns an object of class "policy_data".
#' The object is a list containing the following elements:
#' \item{\code{stage_data}}{[data.table] containing the id, stage number, event
#'                           indicator, action (\eqn{A_k}), state covariates
#'                           (\eqn{X_k}), reward (\eqn{U_k}), and the
#'                           deterministic rewards.}
#' \item{\code{baseline_data}}{[data.table] containing the id and baseline
#'                             covariates (\eqn{B}).}
#' \item{\code{colnames}}{List containing the state covariate names, baseline
#'                        covariate names, and the deterministic reward variable
#'                        names.}
#' \item{\code{action_set}}{Sorted character vector describing the action set, i.e.,
#'                          the possible actions at all stages.}
#' \item{\code{stage_action_sets}}{List of sorted character vectors describing
#'                                 the observed actions at each stage.}
#' \item{\code{dim}}{List containing the number of observations (n) and the
#'                   number of stages (K).}
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class \code{policy_data}:
#' \itemize{
#' \item{[partial()]}{ Trim the maximum number
#'                              of stages in a \code{policy_data} object.}
#' \item{[subset.policy_data()]}{ Subset a a \code{policy_data} object on ID.}
#' \item{[get_history()]}{ Summarize the history and action at
#'                                    a given stage.}
#' \item{[get_history_names()]}{ Get history variable names.}
#' \item{[get_actions()]}{ Get the action at every stage.}
#' \item{[get_utility()]}{Get the utility.}
#' }
#' @seealso
#' [policy_eval()], [policy_learn()], [copy_policy_data()]
#' @examples
#' library("polle")
#' ### Single stage: Wide data
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' head(d1, 5)
#' # constructing policy_data object:
#' pd1 <- policy_data(d1,
#'                    action="A",
#'                    covariates=c("Z", "B", "L"),
#'                    utility="U")
#' pd1
#' # associated S3 methods:
#' methods(class = "policy_data")
#' head(get_actions(pd1), 5)
#' head(get_utility(pd1), 5)
#' head(get_history(pd1)$H, 5)
#'
#' ### Two stage: Wide data
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d2 <- sim_two_stage(5e2, seed=1)
#' head(d2, 5)
#' # constructing policy_data object:
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#' head(get_history(pd2, stage = 2)$H, 5) # state/Markov type history and action, (H_k,A_k).
#' head(get_history(pd2, stage = 2, full_history = TRUE)$H, 5) # Full history and action, (H_k,A_k).
#'
#' ### Multiple stages: Long data
#' source(system.file("sim", "multi_stage.R", package="polle"))
#' d3 <- sim_multi_stage(5e2, seed = 1)
#' head(d3$stage_data, 10)
#' # constructing policy_data object:
#' pd3 <- policy_data(data = d3$stage_data,
#'                    baseline_data = d3$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#' pd3
#' head(get_history(pd3, stage = 3)$H, 5) # state/Markov type history and action, (H_k,A_k).
#' head(get_history(pd3, stage = 2, full_history = TRUE)$H, 5) # Full history and action, (H_k,A_k).
#' @export
policy_data <- function(data, baseline_data,
                        type="wide",
                        action, covariates, utility,
                        baseline = NULL,
                        deterministic_rewards = NULL,
                        id = NULL, stage = NULL, event = NULL,
                        action_set = NULL,
                        verbose = FALSE) {
  data <- check_data(data)
  data <- copy(data)
  if (!missing(baseline_data)){
    baseline_data <- check_data(baseline_data)
    baseline_data <- copy(baseline_data)
  }

  if(!(is.character(type) & (length(type == 1))))
    stop("'type' must be either \"wide\" or \"long\".")
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
      action_set = action_set,
      verbose = verbose
    )
  } else if (any(type %in% "long")) {
    if (!is.null(deterministic_rewards))
      warning("deterministic_rewards is not used when type = 'long'.")
    if (missing(covariates))
      covariates <- NULL
    if(!is.null(covariates))
      warning("covariates is not used when type = 'long'.")

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
      action_set = action_set,
      verbose = verbose)

  } else{
    stop("'type' must be either \"wide\" or \"long\".")
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

melt_wide_data <- function(wide_data,
                           id,
                           action,
                           covariates,
                           baseline,
                           utility,
                           deterministic_rewards){

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
  if(!all(action %in% colnames(wide_data))){
    mes <- paste(action[!(action %in% colnames(wide_data))], collapse = ",")
    mes <- paste("Action variables ", mes, " not found in data.", sep = "")
    stop(mes)
  }

  # getting the action set:
  action_set <- sort(unique(unlist(wide_data[ , action, with = FALSE])))

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
      if(length(du) != K){
        mes <- paste("Each element in 'deterministic_rewards' must have length ", K, ".", sep = "")
        stop(mes)
      }
      return(du)
    })
    deterministic_reward_names <- paste("U_A", action_set, sep = "")
    if ((!all(names(deterministic_rewards) %in% deterministic_reward_names)) | (!is.null(deterministic_rewards) & is.null(names(deterministic_rewards)))) {
      mes <- paste(deterministic_reward_names, collapse = "', '")
      mes <- paste("'deterministic_rewards' must be a named list with names in the set '", mes, "'.", sep = "")
      stop(mes)
    }
  }

  ### checking if 'data' contains all non-NA variable names:
  tmp <- unlist(c(id, action, covariates, baseline, utility, deterministic_rewards))
  tmp <- tmp[!is.na(tmp)]
  if (!all(tmp %in% names(wide_data))){
    mes <- tmp[!(tmp %in% names(wide_data))]
    mes <- paste(mes, collapse = "\", \"")
    mes <- paste("Variables not found in data: \"", mes, "\".", sep = "")
    stop(mes)
  }
  rm(tmp)

  ### checking for non-NA duplicates:
  tmp <- unlist(c(id, action, covariates, baseline, utility, deterministic_rewards))
  tmp <- tmp[!is.na(tmp)]
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
  # setting id if missing:
  if (is.null(id)){
    if (any("id" %in% colnames(wide_data))){
      stop("'data' has a variable id, but 'id' = NULL. Please set 'id' = \"id\" or change the name of the id variable.")
    }
    # setting id:
    wide_data[, id := 1:.N]
    id <- "id"
  }

  # setting the rewards to 0 in case the final utility is provided:
  if (length(utility)==1) {
    for (i in seq(K)) {
      u <- paste0("_", utility[i], "_", i)
      stopifnot(!any(u %in% names(wide_data)))
      wide_data[, (u) := 0]
      utility <- c(u, utility)
    }
  }

  # augmenting wide_data to handle NA covariates entries:
  for (l in seq_along(covariates)){
    na_idx <- is.na(covariates[[l]])
    if (any(na_idx)){
      if (all(na_idx)){
        mes <- names(covariates[l])
        mes <- paste("covariate", mes, 'is invalid.')
        stop(mes)
      }
      # augmenting wide_data with NA column (of the same class):
      NA_col_name <- paste("_NA_", l, sep = "")
      wide_data[, c(NA_col_name) := wide_data[[covariates[[l]][!na_idx][[1]]]]]
      wide_data[, c(NA_col_name) := NA]
      # updating the covariate names:
      covariates[[l]][na_idx] <- NA_col_name
    }
  }

  ### melting stage data:
  stage <- A <- U <- event <- NULL
  measure <- append(list("A" = action), covariates)
  measure <- append(measure, list("U" = utility))
  if (!is.null(deterministic_rewards)){
    measure <- append(measure, deterministic_rewards)
  }
  # selecting subset:
  sel <- unlist(c(id, action, covariates, utility, deterministic_rewards))
  sel <- sel[!is.na(sel)]
  stage_data <- subset(wide_data, select = sel)
  # converts to long data:
  stage_data <- melt(stage_data, id.vars = id, measure.vars = measure, variable.name = "stage")
  setnames(stage_data, id, "id")

  stage_data[ , stage := as.integer(as.character(stage))]
  stage_data[ , A := as.character(A)]
  # setting the event variable:
  stage_data[!is.na(A), event := 0]
  stage_data <- stage_data[!(is.na(A) & is.na(U)), ]
  stage_data[is.na(A), event := 1]
  # setting keys:
  setkeyv(stage_data, c("id", "stage"))
  rm(stage, A, U, event)

  ### getting baseline data:
  if (!is.null(baseline)){
    sel <- c(id, baseline)
    baseline_data <- subset(wide_data, select = sel)
    setnames(baseline_data, id, "id")
    setkeyv(baseline_data, "id")
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
    mes <- paste("The variables ", paste(drn, collapse = ", "), " in 'data' are considered deternistic rewards.", sep = "")
    message(mes)
  }

  if (!is.null(baseline_data)){
    set_default_name(baseline_data, variable_name = id, default_name = "id")
  }

  out <- list(
    stage_data = long_data,
    baseline_data = baseline_data
  )
  return(out)
}
