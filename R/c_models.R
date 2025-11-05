

new_c_model <- function(c_model){
  class(c_model) <- c("c_model", "function")
  return(c_model)
}

update_c_formula <- function(formula, time, time2, event, H) {
  tt <- terms(formula, data = H)
  if (length(attr(tt, "term.labels")) == 0){
    formula <- Surv(time = time, time2 = time2, event == 2) ~ 1
  } else {
    response <- "Surv(time = time, time2 = time2, event == 2)"
    formula <- reformulate(attr(tt, "term.labels"), response = response)
  }
  environment(formula)$time <- time
  environment(formula)$time2 <- time2
  environment(formula)$event <- event
  return(formula)
}

#' @title c_model class object
#'
#' @description
#' Provides constructors for right-censoring models.
#' Main constructors include:
#' * `c_cox()`: Cox proportional hazards model
#' * `c_no_censoring()`: Model for scenarios without censoring
#' The constructors are used as input for [policy_eval()] and [policy_learn()].
#'
#' @param formula An object of class [formula] specifying the design matrix for
#' the right-censoring model. Use [get_history_names()] to view the available
#' variable names.
#' @param offset offsets for Cox model, see [mets::phreg()]
#' @param weights weights for Cox score equations, see [mets::phreg()]
#' @param ... Additional arguments passed to the model.
#' @details
#' \code{c_cox()} is a wrapper of [mets::phreg()] (Cox proportional hazard model).\cr
#' @returns
#' A c-model object (function) with arguments:
#' * event: censoring events
#' * time: start time
#' * time2: end time
#' * H: history matrix
#' @seealso [get_history_names()].
#' @docType class
#' @name c_model
NULL

## Cox model interface

#' @rdname c_model
#' @export
c_cox <- function(formula = ~.,
                  offset = NULL,
                  weights = NULL,
                  ...) {
  formula <- as.formula(formula)
  dotdotdot <- list(...)

  if (!requireNamespace("mets")) stop("Package 'mets' required")

  c_cox <- function(event, time, time2, H) {
    ## setting the reponse of the formula to Surv(time = time, time2 = time2, event == 2):
    ## note that time, time2 and event are added to the formula environment
    formula <- update_c_formula(formula = formula,
                                time = time,
                                time2 = time2,
                                event = event,
                                H = H)
    args_cox <- append(list(formula = formula,
                            data = H,
                            offset = offset,
                            weights = weights),
                       dotdotdot)

    model <- tryCatch(do.call(mets::phreg, args = args_cox),
                      error = function(e) e
                      )
    if (inherits(model, "error")) {
      model$message <-
        paste0(model$message, " when calling 'c_cox' with formula:\n",
               format(formula))
      stop(model)
    }

    model$call <- NULL

    m <- list(model = model)
    class(m) <- c("c_cox")
    return(m)
  }

  ## setting class:
  c_cox <- new_c_model(c_cox)

  return(c_cox)
}

#' @export
predict.c_cox <- function(object, new_H, new_time, ...){
  model <- get_element(object, "model")

  ch <- targeted::cumhaz(object = model, newdata = new_H, times = new_time, individual.time = TRUE, ...)
  surv <- as.vector(get_element(ch, "surv"))
  return(surv)
}

#' @rdname c_model
#' @export
c_no_censoring <- function(){

  c_no_censoring <- function(event, time, time2, H){
    model <- list()
    class(model) <- c("c_no_censoring")
    return(model)
  }

  ## setting class:
  c_no_censoring <- new_c_model(c_no_censoring)

  return(c_no_censoring)
}

#' @export
predict.c_no_censoring <- function(object, new_H, new_time, ...){
  n <- length(new_time)
  surv <- rep(1, times = n)
  return(surv)
}
