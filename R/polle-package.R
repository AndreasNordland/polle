#' @keywords internal
"_PACKAGE"

## usethis namespace: start
##' @importFrom lava IC estimate
##' @importFrom stats predict gaussian binomial glm as.formula nls model.frame model.matrix reformulate predict.glm terms update .getXlevels addmargins coef delete.response complete.cases model.response na.pass aggregate vcov var
##' @importFrom methods formalArgs
##' @importFrom utils tail capture.output
##' @importFrom survival survfit Surv
##' @importFrom progressr progressor
##' @import data.table
## usethis namespace: end
NULL

##' @export
lava::IC

##' @export
lava::estimate

## The 'SuperLearner' package is scheduled for archival on CRAN and has been
## removed as a dependency of 'polle'. The re-export of SuperLearner::All and
## the SL.nls / SL.glm.formula helpers (see R/sl_models.R) are retained as
## commented code and can be revived if a future polle release reintroduces
## a SuperLearner-based interface.
##
## ##' @export
## SuperLearner::All
