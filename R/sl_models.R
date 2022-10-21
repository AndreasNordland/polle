SL.glm.formula <- function(Y, X, newX, obsWeights, family, id, formula=Y~., ...) {
  formula <- update(formula, Y~.)
  environment(formula) <- environment()
  fit <- glm(formula=formula, data=X, family=family, weights=obsWeights)
  pred <- predict(fit, newdata=newX, type="response")
  obj <- list(fit=fit)
  class(obj) <- c("SL.glm.formula")
  out <- list(pred = pred, fit = obj)
  return(out)
}

predict.SL.glm.formula <- function(object, newdata, family, X = NULL, Y = NULL,...) {
	pred <- predict(object=object$fit, newdata=newdata, type="response")
	return(pred)
}

SL.nls <- function(Y, X, newX, obsWeights, family, id, formula, start,  ...) {
  formula <- as.formula(paste0("Y~", tail(as.character(formula), 1)))
  environment(formula) <- environment()
  fit <- nls(formula=formula, data=X, weights=obsWeights, start=start, ...)
  pred <- predict(fit, newdata=newX)
  obj <- list(fit=fit)
  class(obj) <- c("SL.nls")
  out <- list(pred = pred, fit = obj)
  return(out)
}

predict.SL.nls <- function(object, newdata, family,
                           X = NULL, Y = NULL, ...) {
	pred <- predict(object=object$fit, newdata=newdata)
	return(pred)
}
