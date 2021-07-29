update_g_formula <- function(formula, A, X) {
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
  environment(formula)$AA <- AA
  attr(formula, "action_set") <- action_set
  attr(formula, "response") <- "AA"
  return(formula)
}

get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}

get_design <- function(formula, data) {
  tt <- terms(formula, data=data)
  attr(tt, "intercept") <- 0
  tt <- delete.response(tt)
  mf <- model.frame(tt, data=data)
  x_levels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  attr(tt, ".Environment") <- NULL
  return(list(terms=tt, x_levels=x_levels, x=x))
}

#' @export
new_g_glm <- function(formula = ~., family = binomial(), model = FALSE, ...) {
  dotdotdot <- list(...)
  g_glm <- function(A, X){
    formula <- update_g_formula(formula, A, X)
    action_set <- attr(formula, "action_set")
    args_glm <- append(list(formula = formula, data = X,
                            family = family, model = model), dotdotdot)
    glm_model <- do.call(what = "glm", args = args_glm)
    glm_model$call <- NULL

    m <- list(glm_model = glm_model, action_set = action_set)
    class(m) <- c("g_glm", "g_model")
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
new_g_glmnet <- function(formula = ~., family = "binomial",
                         alpha = 1, s = "lambda.min", ...) {
  dotdotdot <- list(...)
  g_glmnet <- function(A, X) {
    formula <- update_g_formula(formula, A, X)
    action_set <- attr(formula, "action_set")
    y <- get_response(formula, data=X)
    des <- get_design(formula, data=X)
    if (ncol(des$x)<2)
      stop("g_glmnet requires a model matrix with two or more columns.")

    args_glmnet <- list(y = y,  x = des$x, family = family, alpha = alpha)
    args_glmnet <- append(args_glmnet, dotdotdot)
    glmnet_model <- do.call(what = "cv.glmnet", args = args_glmnet)
    glmnet_model$call <- NULL

    m <- with(des, list(glmnet_model = glmnet_model,
                        s = s,
                        xlevels = x_levels,
                        terms = terms,
                        action_set = action_set))
    class(m) <- c("g_glmnet", "g_model")
    return(m)
  }
  return(g_glmnet)
}

#' @export
predict.g_glmnet <- function(object, new_X){
  glmnet_model <- object$glmnet_model
  mf <- with(object, model.frame(terms, data=new_X, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_X, xlev = object$xlevels)
  fit <- predict(glmnet_model,  newx = newx, type = "response", s = object$s)
  probs <- cbind((1-fit), fit)
  return(probs)
}
