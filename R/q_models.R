
#' @export
q_linear <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ A * ., data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "q_lm"
  return(m)

  return(m)
}

#' @export
q_interept <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ 1, data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "q_lm"
  return(m)

  return(m)
}

#' @export
predict.q_lm <- function(object, new_A, new_X){
  lm_model <- object$lm_model

  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }

  data <- cbind(A = new_A, new_X)
  pred <- predict(lm_model, newdata = data, type = "response")

  return(pred)
}
