#' @export
new_g_glm <- function(
  formula = ~.,
  family = binomial(),
  model = FALSE,
  ...
){
  dotdotdot <- list(...)

  g_glm <- function(A, X){
    action_set <- sort(unique(A))
    if (length(action_set) != 2)
      stop("g_glm requires exactly two levels.")
    AA <- A
    AA[A == action_set[1]] <- 0
    AA[A == action_set[2]] <- 1
    AA <- as.numeric(AA)

    if (is.matrix(X)) {
      X = as.data.frame(X)
    }

    tt <- terms(formula, data = X)

  if (length(attr(tt, "term.labels")) == 0)
    formula <- AA ~ 1
  else
    formula <- reformulate(attr(tt, "term.labels"), response = "AA")

    args_glm <- list(
      formula = formula,
      data = X,
      family = family,
      model = model
    )
    args_glm <- append(args_glm, dotdotdot)

    glm_model <- do.call(what = "glm", args = args_glm)
    glm_model$call <- NULL

    m <- list(
      glm_model = glm_model,
      action_set = action_set
    )

    class(m) <- "g_glm"
    return(m)

  }
}
#' @export
predict.g_glm <- function(object, new_X){
  glm_model <- getElement(object, "glm_model")

  # model matrix as data.frame
  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")

  probs <- cbind((1-fit), fit)

  return(probs)
}
