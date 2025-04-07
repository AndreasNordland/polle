

new_c_model <- function(c_model){
  class(c_model) <- c("c_model", "function")
  return(c_model)
}

## Cox model interface

#' @export
c_cox <- function(formula = ~.,
                  time,
                  time_2 = NULL,
                  offset = NULL,
                  weights = NULL,
                  ...) {
  formula <- as.formula(formula)
  dotdotdot <- list(...)

  c_cox <- function(event, H){
    browser()
    formula <- update_g_formula(formula, A, H)

    args_cox <- append(list(formula = formula,
                            data = H,
                            family = family,
                            model = model,
                            na.action = na.action),
                       dotdotdot)

    model <- tryCatch(do.call(what = "glm", args = args_glm),
                      error = function(e) e
    )
    if (inherits(model, "error")) {
      model$message <-
        paste0(model$message, " when calling 'g_glm' with formula:\n",
               format(formula))
      stop(model)
    }

    model$call <- NULL

    m <- list(model = model)
    class(m) <- c("c_cox")
    return(m)
  }

  # setting class:
  g_glm <- new_g_model(g_glm)

  return(g_glm)
}
#' @export
predict.c_cox <- function(object, new_H, ...){
  browser()
  model <- getElement(object, "model")
  fit <- predict.glm(object = model, newdata = new_H, type = "response")
  probs <- cbind((1-fit), fit)
  return(probs)
}
