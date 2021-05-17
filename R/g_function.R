#' @export
g_function <- function(object, g_model)
  UseMethod("g_function")

#' @export
g_function.history <- function(object, g_model){
  action_set <- object$action_set

  # getting the action (A) and the model matrix (X):
  A <- get_A(object)
  X <- get_X(object)

  # fitting the model:
  gm <- g_model(A = A, X = X)

  g_function <- list(
    gm = gm,
    X_names = colnames(X),
    action_set = action_set
  )
  class(g_function) <- "g_function"

  return(g_function)
}

#' @export
predict.g_function <- function(object, new_history){
  X_names <- object$X_names
  g_model <- object$gm
  action_set <- object$action_set

  id_stage <- get_id_stage(new_history)
  new_X <- get_X(new_history)

  # checking that the model matrix has the correct form (could be an issue if factor levels are missing)
  stopifnot(
    names(new_X) == X_names
  )

  g_predictions <- predict(g_model, new_X = new_X, type = "probs", action_set = action_set)
  colnames(g_predictions) <- paste("g", action_set, sep = "_")

  # including the id's
  g_predictions <- data.table(id_stage, g_predictions)
  setkey(g_predictions, id, stage)

  return(g_predictions)
}

fit_g_model <- function(policy_data, g_model, g_full_history = TRUE){
  K <- policy_data$dim$K

  # checking g_model: must either be a list of length K or a single g-model function (with a markov history).
  stopifnot(
    if(class(g_model)[[1]] == "list")
      length(g_model) == K
    else !g_full_history
  )

  fg <- function(stage){
    his <- get_stage_history(policy_data, stage = stage, full_history = g_full_history)
    return(g_function(his, g_model[[stage]]))
  }

  if (class(g_model)[[1]] == "list"){
    out <- lapply(1:K, fg)
  } else{
    his <- markov_history(policy_data)
    out <- g_function(his, g_model)
  }

  return(out)
}
