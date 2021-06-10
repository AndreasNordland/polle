# TODO: Consider to introduce general model objects such as modelObj

#' @export
g_binomial_linear <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ ., data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "g_binomial"
  return(bm)
}

#' @export
g_binomial_intercept <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ 1, data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "g_binomial"
  return(bm)
}

#' @export
predict.g_binomial <- function(object, new_X){
  glm_model <- object$glm_model

  # model matrix as data.frame
  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")

  probs <- cbind((1-fit), fit)

  return(probs)
}
