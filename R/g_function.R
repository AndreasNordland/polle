#' @export
fit_g_function <- function(history, g_model){
  action_set <- history$action_set

  # getting the action (A) and the model matrix (X):
  A <- get_A(history)
  X <- get_X(history)

  # checking that all actions in the actions set occur:
  if (!all(action_set == sort(unique(A)))) stop("An action in the action set does not occur.")

  # fitting the model:
  g_model <- g_model(A = A, X = X)

  g_function <- list(
    g_model = g_model,
    X_names = colnames(X),
    action_set = action_set
  )
  class(g_function) <- "g_function"

  return(g_function)
}

#' @export
evaluate.g_function <- function(object, new_history){
  g_model <- object$g_model
  X_names <- object$X_names
  action_set <- object$action_set

  id_stage <- get_id_stage(new_history)
  new_X <- get_X(new_history)

  if (!all(names(new_X) %in% X_names)) stop("new_history does not have the same variable names as the original history.")

  g_values <- predict(g_model, new_X = new_X)

  colnames(g_values) <- paste("g", action_set, sep = "_")

  # including the id's
  g_values <- data.table(id_stage, g_values)
  setkey(g_values, id, stage)

  return(g_values)
}

#' @export
fit_g_functions <- function(policy_data, g_models, full_history = FALSE){
  K <- policy_data$dim$K

  # checking the g_models:
  if (class(g_models)[[1]] == "list"){
    if (length(g_models) != K) stop("g_models must either be a list of length K or a single g-model.")
  } else{
    if (full_history == TRUE) stop("full_history must be FALSE when a single g-model is provided.")
  }

  if (class(g_models)[[1]] == "list"){
    history <- lapply(1:K, function(s) get_stage_history(policy_data, stage = s, full_history = full_history))
    g_functions <- mapply(history, g_models, FUN = function(h, gm) fit_g_function(history = h, g_model = gm), SIMPLIFY = FALSE)
  } else{
    history <- state_history(policy_data)
    g_functions <- list(fit_g_function(history, g_models))
  }

  class(g_functions) <- "nuisance_functions"
  attr(g_functions, "full_history") <- full_history

  return(g_functions)
}

