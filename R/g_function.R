#' @export
fit_g_function <- function(history, g_model){
  action_set <- history$action_set

  # getting the action (A) and the model matrix (H):
  A <- get_A(history)
  H <- get_H(history)

  # checking that all actions in the actions set occur:
  if (!all(action_set == sort(unique(A)))) stop("An action in the action set does not occur.")

  # fitting the model:
  g_model <- g_model(A = A, H = H)

  g_function <- list(
    g_model = g_model,
    H_names = colnames(H),
    action_set = action_set
  )
  class(g_function) <- "g_function"

  return(g_function)
}

#' @export
print.g_function <- function(x){
  y <- x$g_model
  y$action_set <- NULL
  attr(y,"class") <- NULL

  print(y)
}

#' @export
evaluate.g_function <- function(object, new_history){
  g_model <- object$g_model
  H_names <- object$H_names
  action_set <- object$action_set

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  if (!all(names(new_H) %in% H_names)) stop("new_history does not have the same variable names as the original history.")

  g_values <- predict(g_model, new_H = new_H)
  colnames(g_values) <- paste("g", action_set, sep = "_")

  if (!all(complete.cases(g_values))){
    stage <- unique(new_history$H$stage)
    mes <- paste("Evaluation of the g-function at stage ", stage, " have missing values.", sep = "")
    stop(mes)
  }

  # including the id's
  g_values <- data.table(id_stage, g_values)
  setkey(g_values, id, stage)

  return(g_values)
}

#' @export
fit_g_functions <- function(policy_data, g_models, full_history){
  K <- get_K(policy_data)

  # checking the g_models:
  if (class(g_models)[[1]] == "list"){
    if (length(g_models) != K) stop("g_models must either be a list of length K or a single g-model.")
  } else{
    if (full_history == TRUE) stop("full_history must be FALSE when a single g-model is provided.")
  }

  if (class(g_models)[[1]] == "list"){
    history <- lapply(1:K, function(s) get_stage_history(policy_data, stage = s, full_history = full_history))
    g_functions <- mapply(history, g_models, FUN = function(h, gm) fit_g_function(history = h, g_model = gm), SIMPLIFY = FALSE)
    names(g_functions) <- paste("stage_", 1:K, sep = "")
  } else{
    history <- state_history(policy_data)
    g_functions <- list(across_stages = fit_g_function(history, g_models))
  }

  class(g_functions) <- "nuisance_functions"
  attr(g_functions, "full_history") <- full_history

  return(g_functions)
}

#' @export
fit_g_functions_cf <- function(folds, policy_data, g_models, full_history, ...){
  id <- get_id(policy_data)
  K <- policy_data$dim$K

  fit_cf <- lapply(
    folds,
    function(f){
      train_id <- id[-f]
      train_policy_data <- subset(policy_data, train_id)
      if (train_policy_data$dim$K != K) stop("The number of stages K varies across the training policy data folds.")
      train_g_functions <- fit_g_functions(policy_data = train_policy_data, g_models = g_models, full_history = full_history, ...)

      validation_id <- id[f]
      validation_policy_data <- subset(policy_data, validation_id)
      validation_g_values <- evaluate(train_g_functions, validation_policy_data)

      list(
        train_g_functions = train_g_functions,
        validation_g_values = validation_g_values
      )
    }
  )
  fit_cf <- simplify2array(fit_cf)

  g_functions_cf <- fit_cf["train_g_functions", ]
  g_values <- fit_cf["validation_g_values", ]

  g_values <- rbindlist(g_values)
  setkeyv(g_values, c("id", "stage"))

  out <- list(
    g_functions_cf = g_functions_cf,
    g_values = g_values
  )
  return(out)
}




