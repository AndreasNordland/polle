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
  return(g_glm)
}
#' @export
predict.g_glm <- function(object, new_X){
  glm_model <- getElement(object, "glm_model")

  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")

  probs <- cbind((1-fit), fit)

  return(probs)
}

#' @export
new_g_glmnet <- function(
  formula = ~.,
  family = "binomial",
  alpha = 1,
  s = "lambda.min",
  ...
){
  dotdotdot <- list(...)

  g_glmnet <- function(A, X){
    action_set <- sort(unique(A))
    if (length(action_set) != 2)
      stop("g_glm requires exactly two levels.")
    AA <- A
    AA[A == action_set[1]] <- 0
    AA[A == action_set[2]] <- 1
    y <- as.numeric(AA)

    tt <- terms(formula, data = X)
    attr(tt, "intercept") <- 0
    f <- delete.response(tt)
    mf <- model.frame(f, data = X)
    x_levels <- .getXlevels(tt, mf)
    x <- model.matrix(mf, data=X)
    attr(tt, ".Environment") <- NULL

    if (ncol(x)<2) stop("g_glmnet requires a model matrix with two or more columns.")

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
      xlevels = x_levels,
      tt = tt,
      action_set = action_set
    )
    class(m) <- "g_glmnet"
    return(m)

  }
  return(g_glmnet)
}

#' @export
predict.g_glmnet <- function(object, new_X){
  glmnet_model <- object$glmnet_model
  s <- object$s
  xlevels <- object$xlevels
  tt <- object$tt

  mf <- model.frame(tt, data=new_X, xlev = xlevels, drop.unused.levels=FALSE)
  newx <- model.matrix(mf, data=new_X, xlev = xlevels)

  fit <- predict(
    glmnet_model,
    newx = newx,
    type = "response",
    s = s
  )

  probs <- cbind((1-fit), fit)

  return(probs)
}

# xlevels <- getElement(object, "xlevels")
# formula <- getElement(object, "formula")
# mf <-model.frame(formula, data=new_X, xlev=xlevels, drop.unused.levels=FALSE)
