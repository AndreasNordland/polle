
##' Policy Learning
##'
##' @name polle-package
##' @importFrom lava IC estimate
##' @importFrom stats predict gaussian binomial glm as.formula nls model.frame
##'   model.matrix reformulate predict.glm terms update .getXlevels addmargins
##'   coef delete.response complete.cases
##' @importFrom methods formalArgs
##' @importFrom utils tail capture.output
##' @importFrom survival survfit
##' @import data.table
##' @aliases polle-package polle
##' @docType package
##' @keywords package
NULL

##' For internal use
##'
##' @title For internal use
##' @name SL.nls
##' @rdname internal
##' @keywords utilities
##' @export
##' @aliases
##' SL.nls SL.glm.formula
