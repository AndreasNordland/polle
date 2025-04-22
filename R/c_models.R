

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

## Cox model interface

#' @export
c_cox <- function(formula = ~.,
                  offset = NULL,
                  weights = NULL,
                  ...) {
  formula <- as.formula(formula)
  dotdotdot <- list(...)

  c_cox <- function(event, time, time2, H) {
    ## setting the reponse of the formula to Surv(time = time, time2 = time2, event == 2):
    ## note that time, time2 and event are added to the formula environment
    formula <- update_c_formula(formula = formula, time = time, time2 = time2, event = event, H = H)
    args_cox <- append(list(formula = formula,
                            data = H,
                            offset = offset,
                            weights = weights),
                       dotdotdot)

    model <- tryCatch(do.call(what = "phreg", args = args_cox),
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

                                        # setting class:
  c_cox <- new_c_model(c_cox)

  return(c_cox)
}

#' @export
predict.c_cox <- function(object, new_H, new_time, ...){
  model <- get_element(object, "model")
  ch <- targeted:::cumhaz(object = model, newdata = new_H, times = new_time, individual.time = TRUE, ...)
  surv <- as.vector(get_element(ch, "surv"))
  return(surv)
}
