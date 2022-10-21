
##' Policy Learning
##'
##' @name polle-package
##' @importFrom lava IC estimate
##' @importFrom stats predict gaussian binomial glm as.formula nls model.frame
##'   model.matrix reformulate predict.glm terms update .getXlevels addmargins
##'   coef delete.response complete.cases model.response
##' @importFrom methods formalArgs
##' @importFrom utils tail capture.output
##' @importFrom survival survfit
##' @import data.table SuperLearner
##' @aliases polle-package polle
##' @docType package
##' @keywords package
##' @author Andreas Nordland (Maintainer) <andreas.nordland@@gmail.com>, Klaus Holst.
##' @description Framework for evaluating and learning realistic policies based on
##' doubly robust loss functions.
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
