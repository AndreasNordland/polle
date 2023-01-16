
##' Policy Learning
##'
##' @name polle-package
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
##' @export IC estimate
##' @export All
##' @aliases polle-package polle
##' @docType package
##' @keywords package
##' @author Andreas Nordland (Maintainer) <andreas.nordland@@gmail.com>, Klaus Holst.
##' @description Framework for evaluating user-specified finite stage policies
##' and learning realistic policies via doubly robust loss functions. Policy
##' learning methods include doubly robust restricted Q-learning,
##' sequential policy tree learning and outcome weighted learning.
##' See Nordland and Holst (20222) <https://arxiv.org/abs/2212.02335> for documentation and references.
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
