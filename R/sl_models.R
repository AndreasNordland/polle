##' @export
SL.glm.formula <- function(Y, X, newX, obsWeights, family, id, formula=Y~., ...) {
  formula <- update(formula, Y~.)
  environment(formula) <- environment()
  fit <- glm(formula=formula, data=X, family=family, weights=obsWeights)
  pred <- predict(fit, newdata=newX, type="response")
  class(fit) <- c("SL.glm1")
  out <- list(pred = pred, fit = list(object=fit))
  return(out)
}

##' @export
predict.SL.glm.formula <- function(object, newdata, family, X = NULL, Y = NULL,...) {
	pred <- predict(object=object$object, newdata=newdata, type="response")
	return(pred)
}

##' @export
SL.nls <- function(Y, X, newX, obsWeights, family, id, formula, start,  ...) {
  formula <- as.formula(paste0("Y~", tail(as.character(formula), 1)))
  environment(formula) <- environment()
  fit <- nls(formula=formula, data=X, weights=obsWeights, start=start, ...)
  pred <- predict(fit, newdata=newX)
  class(fit) <- c("SL.nls")
  out <- list(pred = pred, fit = list(object=fit))
  return(out)
}

##' @export
predict.SL.nls <- function(object, newdata, family, X = NULL, Y = NULL,...) {
	pred <- predict(object=object$object, newdata=newdata)
	return(pred)
}
