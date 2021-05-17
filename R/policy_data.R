#' @export
new_policy_data <- function(stage_data, baseline_data, id = "id", stage = "stage", event = "event", action = "A", utility = "U"){

  # check if data.table
  stopifnot(
    is.data.table(stage_data),
    is.data.table(baseline_data)
  )

  # copy data.table's
  stage_data <- copy(stage_data)
  baseline_data <- copy(baseline_data)

  # setting keys
  setkey(stage_data, id, stage)
  setcolorder(stage_data, c("id", "stage", "event", "A"))
  setkey(baseline_data, id)

  stopifnot(
    is.character(stage_data$A), # A must be a character
    all(sapply(stage_data, function(col) !is.factor(col))), # factors in stage_data are not allowed
    all(sapply(baseline_data, function(col) !is.factor(col))) # factors in baseline_data are not allowed
  )

  # maximal set of actions:
  action_set <- sort(unique(stage_data$A))

  # names of columns for the deterministic utility contributions for every action in the action set:
  action_utility_names <- paste("U", action_set, sep = "_")
  # required column names in stage_data:
  required_names <- c("id", "stage", "event", "A", "U", action_utility_names)
  # required column names in baseline_data:
  required_baseline_names <- c("id")
  stopifnot(
    all(required_names %in% names(stage_data)), # required column names in stage_data
    all(required_baseline_names %in% names(baseline_data)) # required column names in baseline_data
  )

  # names of columns for the stage specific state data (X_k)
  stage_data_names <- names(stage_data)[!(names(stage_data) %in% required_names)]
  # names of columns for the baseline data
  baseline_data_names <- names(baseline_data)[!(names(baseline_data) %in% required_baseline_names)]

  # checks
  stopifnot(
    anyDuplicated(stage_data, by = key(stage_data)) == 0, # no duplicated keys
    anyDuplicated(baseline_data, by = key(baseline_data)) == 0,
    all(unique(stage_data$id) == baseline_data$id), # id match in stage_data and baseline_data
    all(stage_data[ , .(check = all(stage == 1:.N)), by = id]$check), # stages must be on the form 1, 2, ..., k
    all(stage_data[, .(check = all(event == c(rep(0, times = (.N-1)), 1) | event == c(rep(0, times = (.N-1)), 2))), id]$check), # events must be on the form 0,0,...,0,j (j in {1,2})
    all(is.numeric(stage_data$U) & !is.na(stage_data$U)), # the utility must be numeric
    all(sapply(stage_data[, ..action_utility_names], function(col) is.numeric(col)))
  )

  # getting dimensions (and setting event as an index)
  n <- length(unique(stage_data$id))
  K <- stage_data[event == 0, .(max(stage))][[1]]

  object <- list(
    stage_data = stage_data,
    baseline_data = baseline_data,
    colnames = list(
      stage_data_names = stage_data_names,
      action_utility_names = action_utility_names,
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
