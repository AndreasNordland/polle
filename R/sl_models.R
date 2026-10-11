## Internal helpers for the SuperLearner-based q_sl() / g_sl() wrappers.
##
## 'SuperLearner' is listed in Suggests, so q_sl() and g_sl() must guard
## their use of the package at run time.

#' Guard SuperLearner usage
#'
#' Errors with an install hint when the 'SuperLearner' package is not
#' available. Called at the top of \code{q_sl()} / \code{g_sl()}.
#'
#' @noRd
check_superlearner <- function(fn) {
  if (!requireNamespace("SuperLearner", quietly = TRUE)) {
    stop(
      fn, "() requires the 'SuperLearner' package. ",
      "Install it with install.packages(\"SuperLearner\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Resolution environment for SuperLearner library/screen functions
#'
#' \code{SuperLearner::SuperLearner()} resolves screening and prediction
#' algorithm names via \code{get(nm, envir = env)}, walking \code{env}'s
#' parent chain. When 'SuperLearner' is attached (\code{Depends}) that
#' chain reaches the SuperLearner namespace naturally, but under
#' \code{Suggests} (and in particular inside \code{future}'s
#' \code{multisession} workers) it does not, which produces
#' \code{"object 'All' not found"}.
#'
#' We fix this by returning a fresh environment whose parent is the
#' SuperLearner namespace, pre-populated with any names from \code{env}
#' that the user may have overridden. The result is captured at
#' constructor-call time in \code{q_sl()} / \code{g_sl()} and so travels
#' with the fitted model to parallel workers.
#'
#' @param env Caller-supplied environment, typically
#'   \code{parent.frame()} of \code{q_sl()}/\code{g_sl()}.
#' @param SL.library Character vector or list, as accepted by
#'   \code{SuperLearner::SuperLearner()}.
#' @return An environment whose parent is \code{asNamespace("SuperLearner")}.
#' @noRd
sl_library_env <- function(env, SL.library) {
  nms <- unique(c("All", unlist(SL.library)))
  out <- new.env(parent = asNamespace("SuperLearner"))
  for (nm in nms) {
    if (exists(nm, envir = env, inherits = TRUE)) {
      assign(nm, get(nm, envir = env, inherits = TRUE), envir = out)
    }
  }
  out
}

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
#' @export
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
#' @export
predict.SL.nls <- function(object, newdata, family,
                           X = NULL, Y = NULL, ...) {
  pred <- predict(object=object$fit, newdata=newdata)
  return(pred)
}
