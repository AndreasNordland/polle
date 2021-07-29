
#' @export
q_glmnet <- function(formula = ~ A * .,
                     family = "gaussian",
                     alpha = 1,
                     s = "lambda.min",
                     ...) {
  dotdotdot <- list(...)

  q_glmnet <- function(V_res, AX){

    A <- as.factor(A)
    A_levels <- levels(A)

    if (is.matrix(X)) {
      X = as.data.frame(X)
    }
    data <- cbind(A = A, X)

    tt <- terms(formula, data = data)
    if (length(attr(tt, "term.labels")) == 0)
      formula <- ~ 1
    else
      formula <- reformulate(attr(tt, "term.labels"), response = NULL, intercept = FALSE)

    x <- model.matrix(formula, data)
    y <- V_res

    args_glmnet <- list(
      y = y,
      x = x,
      family = family,
      alpha = alpha
    )
    args_glmnet <- append(args_glmnet, dotdotdot)

    glmnet_model <- do.call(what = "cv.glmnet", args = args_glmnet)
    glmnet_model$call <- NULL

    m <- list(
      glmnet_model = glmnet_model,
      s = s,
      formula = formula,
      A_levels = A_levels
    )

    class(m) <- "glmnet_model"
    return(m)
  }

  return(q_glmnet)
}

#' @export
predict.glmnet_model <- function(object, new_A, new_X){
  glmnet_model <- getElement(object, "glmnet_model")
  formula <- getElement(object, "formula")
  A_levels <- getElement(object, "A_levels")
  s <- getElement(object, "s")

  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  new_A <- factor(new_A, levels = A_levels)
  newdata <- cbind(A = new_A, new_X)

  newx <- model.matrix(formula, newdata)

  pred <- predict(
    glmnet_model,
    newx = newx,
    type = "response",
    s = s
  )

  return(pred)
}

#' @export
q_glm <- function(formula = ~ A * .,
                  family = gaussian(),
                  model = FALSE,
                  ...) {
  dotdotdot <- list(...)

  q_glm <- function(V_res, AX){
    data <- AX

    tt <- terms(formula, data = data)
    if (length(attr(tt, "term.labels")) == 0)
      formula <- V_res ~ 1
    else
      formula <- reformulate(attr(tt, "term.labels"), response = "V_res")

    args_glm <- list(
      formula = formula,
      data = data,
      family = family,
      model = model
    )
    args_glm <- append(args_glm, dotdotdot)

    glm_model <- do.call(what = "glm", args = args_glm)
    glm_model$call <- NULL

    m <- list(
      glm_model = glm_model
    )

    class(m) <- "q_glm"
    return(m)
  }

  return(q_glm)
}

#' @export
predict.q_glm <- function(object, new_AX){
  glm_model <- getElement(object, "glm_model")

  newdata <- new_AX

  pred <- predict(glm_model, newdata = newdata, type = "response")

  return(pred)
}
