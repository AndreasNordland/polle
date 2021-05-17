am <- Vectorize(
  function(A, action_set){
    am <- action_set %in% A
    return(am)
  },
  vectorize.args = "A"
)
action_matrix <- function(A, action_set){
  t(am(A = A, action_set = action_set))
}

get_action_predictions <- function(A, action_set, predictions){

  stopifnot(
    is.data.table(predictions),
    all(key(predictions) == c("id", "stage")),
    length(action_set) == ncol(predictions[ ,-c("id", "stage"), with = FALSE])
  )

  P <- action_matrix(A = A, action_set = action_set) * predictions[ ,-c("id", "stage"), with = FALSE]
  P <- apply(P, 1, sum)
  out <- data.table(predictions[ , c("id", "stage"), with = FALSE], P = P)

  return(out)
}

get_function_predictions <- function(policy_data, fun, full_history){
  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  p <- function(stage){
    his <- get_stage_history(policy_data, stage = stage, full_history = full_history)
    pred <- predict(fun[[stage]], his)
    return(pred)
  }

  if (class(fun)[[1]] == "list"){
    pred <- lapply(1:K, p)
    pred <- rbindlist(pred)
    setkey(pred, id, stage)
  } else{
    his <- markov_history(policy_data)
    pred <- predict(fun, his)
  }

  return(pred)
}
