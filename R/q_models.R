
#' @export
q_glmnet <- function(formula = ~ A * .,
                     family = "gaussian",
                     alpha = 1,
                     s = "lambda.min",
                     ...) {
  dotdotdot <- list(...)

  q_glmnet <- function(V_res, AH){
    des <- get_design(formula, data=AH)
    y <- V_res
    args_glmnet <- c(list(y = y, x = des$x,
                        family = family, alpha = alpha), dotdotdot)
    fit <- do.call(what = "cv.glmnet", args = args_glmnet)
    fit$call <- NULL
    m <- with(des, list(
                     fit = fit,
                     s = s,
                     formula = formula,
                     terms = terms,
                     xlevels = x_levels))
    class(m) <- c("q_glmnet", "g_model")
    return(m)
  }
  return(q_glmnet)
}

#' @export
predict.q_glmnet <- function(object, new_AH, ...) {
  mf <- with(object, model.frame(terms, data=new_AH, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_AH, xlev = object$xlevels)
  pred <- predict(getElement(object, "fit"),
                  newx = newx,
                  type = "response",
                  s = getElement(object, "s"))
  return(pred)
}



qr_sl <- (formula = ~ . ...){
  if (!requireNamespace("SuperLearner"))
    stop("Package 'SuperLearner' required.")

  dotdotdot <- list(...)
}


perf_ranger <- function(fit, data,  ...) {
  y <- as.numeric(data[,1])
  x <- data[,-1,drop=FALSE]
  mean((predict(fit, data=x, num.threads=1)-y)^2)^.5
}

#' @export
q_rf <- function(formula = ~ .,
                 num.trees=c(250,500,750), mtry=NULL,
                 cv_args=list(K=3, rep=1), ...) {
  if (!requireNamespace("ranger")) stop("Package 'ranger' required.")
  dotdotdot <- list(...)
  hyper_par <- expand.list(num.trees=num.trees, mtry=mtry)
  rf_args <- function(p) {
    list(num.threads=1, num.trees=p$num.trees, mtry=p$mtry)
  }
  ml <- lapply(hyper_par, function(p)
    function(data) {
      rf_args <- append(rf_args(p), list(y=data[,1], x=as.matrix(data[,-1,drop=FALSE])))
      rf_args <- append(rf_args, dotdotdot)
      do.call("ranger", args=rf_args)
    })

  q_rf <- function(V_res, AH){
    des <- get_design(formula, data=AH)
    data <- data.frame(V_res, des$x)
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(lava::cv(ml, data=data, perf=perf_ranger,
                               K=cv_args$K, rep=cv_args$rep),
                      error=function(...) NULL)
      best <- if (is.null(res)) 1 else which.min(summary(res))
    }
    if (!is.null(res)) {
      fit <- res$fit[[best]]
    } else {
      fit <- ml[[best]](data)
    }
    res <- with(des, list(fit = fit,
                          rf_args = rf_args(hyper_par[[best]]),
                          num.trees=num.trees[best],
                          xlevels = x_levels,
                          terms = terms))
    class(res) <- c("q_rf", "g_model")
    return(res)
  }
  return(q_rf)
}

#' @export
predict.q_rf <- function(object, new_AH, ...){
  mf <- with(object, model.frame(terms, data=new_AH, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_AH, xlev = object$xlevels)
  pr <- predict(object$fit, data=newx, num.threads=1)$predictions
  return(pr)

}


#' @export
q_glm <- function(formula = ~ A * .,
                  family = gaussian(),
                  model = FALSE,
                  ...) {
  dotdotdot <- list(...)

  q_glm <- function(V_res, AH){
    data <- AH

    tt <- terms(formula, data = data)
    if (length(attr(tt, "term.labels")) == 0)
      formula <- V_res ~ 1
    else
      formula <- reformulate(attr(tt, "term.labels"), response = "V_res")

    args_glm <- list(
      formula = formula,
      data = data,
      family = family,
      model = model
    )
    args_glm <- append(args_glm, dotdotdot)

    glm_model <- do.call(what = "glm", args = args_glm)
    glm_model$call <- NULL

    m <- list(
      glm_model = glm_model
    )

    class(m) <- c("q_glm", "g_model")
    return(m)
  }

  return(q_glm)
}

#' @export
predict.q_glm <- function(object, new_AH){
  glm_model <- getElement(object, "glm_model")

  newdata <- new_AH

  pred <- predict(glm_model, newdata = newdata, type = "response")

  return(pred)
}
