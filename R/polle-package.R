#' @keywords internal
"_PACKAGE"

## usethis namespace: start
##' @importFrom lava IC estimate
##' @importFrom SuperLearner All
##' @importFrom stats predict gaussian binomial glm as.formula nls model.frame
##'   model.matrix reformulate predict.glm terms update .getXlevels addmargins
##'   coef delete.response complete.cases model.response na.pass aggregate
##' @importFrom methods formalArgs
##' @importFrom utils tail capture.output
##' @importFrom survival survfit
##' @importFrom progressr progressor
##' @import data.table SuperLearner
## usethis namespace: end
NULL

##' @export
lava::IC

##' @export
lava::estimate

##' @export
SuperLearner::All

##' For internal use
##'
##' @title For internal use
##' @name SL.nls
##' @rdname internal
##' @keywords utilities
##' @export
##' @aliases
##' SL.nls SL.glm.formula
