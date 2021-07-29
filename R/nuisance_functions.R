#' @export
evaluate <- function(object, ...)
  UseMethod("evaluate")

#' @export
evaluate.nuisance_functions <- function(object, policy_data){
  K <- get_K(policy_data)
  action_set <- get_action_set(policy_data)
  full_history <- attr(object, "full_history")

  if (length(object) == K){
    history <- lapply(1:K, function(s) get_stage_history(policy_data, stage = s, full_history = full_history))
    values <- mapply(history, object, FUN = function(h, f) evaluate(f, new_history = h), SIMPLIFY = FALSE)
    values <- rbindlist(values)
    setkey(values, id, stage)
  } else if (length(object) == 1){
    history <- state_history(policy_data)
    values <- evaluate(object[[1]], new_history = history)
  } else{
    stop("Provide either 1 or K nuisance functions for evaluation.")
  }

  return(values)
}

#' @export
cf_fit_nuisance_functions <- function(folds, fit_functions, policy_data, models, full_history = FALSE, ...){
  id <- get_id(policy_data)
  K <- policy_data$dim$K

  cf_fit <- lapply(
    folds,
    function(f){
      train_id <- id[-f]
      train_policy_data <- subset(policy_data, train_id)
      if (train_policy_data$dim$K != K) stop("The number of stages K varies across the training policy data folds.")
      train_functions <- fit_functions(policy_data = train_policy_data, models = models, full_history = full_history, ...)

      validation_id <- id[f]
      validation_policy_data <- subset(policy_data, validation_id)
      validation_values <- evaluate(train_functions, validation_policy_data)

      list(
        train_functions = train_functions,
        validation_values = validation_values
      )
    }
  )
  cf_fit <- simplify2array(cf_fit)

  functions <- cf_fit["train_functions", ]
  values <- cf_fit["validation_values", ]

  values <- rbindlist(values)
  setkeyv(values, c("id", "stage"))

  out <- list(
    functions = functions,
    values = values
  )
  return(out)
}
