update_g_formula <- function(formula, A, H) {
  action_set <- sort(unique(A))
  if (length(action_set) != 2)
    stop("g_glm requires exactly two levels.")
  AA <- A
  AA[A == action_set[1]] <- 0
  AA[A == action_set[2]] <- 1
  AA <- as.numeric(AA)
  tt <- terms(formula, data = H)
  if (length(attr(tt, "term.labels")) == 0)
    formula <- AA ~ 1
  else
    formula <- reformulate(attr(tt, "term.labels"), response = "AA")
  environment(formula)$AA <- AA
  attr(formula, "action_set") <- action_set
  attr(formula, "response") <- "AA"
  return(formula)
}

get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}

get_design <- function(formula, data, intercept=FALSE) {
  tt <- terms(formula, data=data)
  if (!intercept)
    attr(tt, "intercept") <- 0
  tt <- delete.response(tt)
  mf <- model.frame(tt, data=data)
  x_levels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  attr(tt, ".Environment") <- NULL
  return(list(terms=tt, x_levels=x_levels, x=x))
}

################################################################################
## Generalized Linear Model interface
################################################################################

#' @export
g_glm <- function(formula = ~., family = binomial(), model = FALSE, ...) {
  dotdotdot <- list(...)
  force(formula)
  g_glm <- function(A, H, action_set){
    formula <- update_g_formula(formula, A, H)
    args_glm <- append(list(formula = formula, data = H,
                            family = family, model = model), dotdotdot)
    glm_model <- do.call(what = "glm", args = args_glm)
    glm_model$call <- NULL

    m <- list(glm_model = glm_model, action_set = action_set)
    class(m) <- c("g_glm", "g_model")
    return(m)

  }
  return(g_glm)
}
#' @export
predict.g_glm <- function(object, new_H){
  glm_model <- getElement(object, "glm_model")
  fit <- predict.glm(object = glm_model, newdata = new_H, type = "response")
  probs <- cbind((1-fit), fit)
  return(probs)
}

################################################################################
## glmnet (Elastic Net) interface
################################################################################

#' @export
g_glmnet <- function(formula = ~., family = "binomial",
                     alpha = 1, s = "lambda.min", ...) {
  if (!requireNamespace("glmnet")) stop("Package 'ranger' required")
  force(formula)
  dotdotdot <- list(...)
  g_glmnet <- function(A, H, action_set) {
    formula <- update_g_formula(formula, A, H)
    ##action_set <- attr(formula, "action_set")
    y <- get_response(formula, data=H)
    des <- get_design(formula, data=H)
    if (ncol(des$x)<2)
      stop("g_glmnet requires a model matrix with two or more columns.")

    args_glmnet <- list(y = y,  x = des$x, family = family, alpha = alpha)
    args_glmnet <- append(args_glmnet, dotdotdot)
    glmnet_model <- do.call(glmnet::cv.glmnet, args = args_glmnet)
    glmnet_model$call <- NULL

    m <- with(des, list(glmnet_model = glmnet_model,
                        s = s,
                        xlevels = x_levels,
                        terms = terms,
                        action_set = action_set))
    class(m) <- c("g_glmnet", "g_model")
    return(m)
  }
  return(g_glmnet)
}

#' @export
predict.g_glmnet <- function(object, new_H) {
  glmnet_model <- object$glmnet_model
  mf <- with(object, model.frame(terms, data=new_H, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_H, xlev = object$xlevels)
  fit <- predict(glmnet_model,  newx = newx, type = "response", s = object$s)
  probs <- cbind((1-fit), fit)
  return(probs)
}

perf_ranger_prob <- function(fit, data,  ...) {
  score_class(predict(fit, data=as.matrix(data[,-1,drop=FALSE]),
                      num.threads=1)$predictions,
              data[,1], ...)
}

################################################################################
## ranger (Random Forest) interface
################################################################################

#' @export
g_rf <- function(formula = ~.,
                 num.trees=c(500), mtry=NULL,
                 cv_args=list(K=5, rep=1), ...) {
  if (!requireNamespace("ranger")) stop("Package 'ranger' required")
  force(formula)
  dotdotdot <- list(...)
  hyper_par <- expand.list(num.trees=num.trees, mtry=mtry)
    rf_args <- function(p) {
        list(probability=TRUE, num.threads=1,
             num.trees=p$num.trees, mtry=p$mtry)
    }
    ml <- lapply(hyper_par, function(p)
      function(data) {
        rf_args <- append(rf_args(p), list(y=data[,1], x=as.matrix(data[,-1,drop=FALSE])))
        rf_args <- append(rf_args, dotdotdot)
        do.call(ranger::ranger, args=rf_args)
      })

  g_rf <- function(A, H, action_set) {
    ##action_set <- sort(unique(A))
    A <- factor(A, levels=action_set)
    des <- get_design(formula, data=H)
    data <- data.frame(A, des$x)
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(lava::cv(ml, data=data, perf=perf_ranger_prob,
                               K=cv_args$K, rep=cv_args$rep),
                      error=function(...) NULL)
      best <- if (is.null(res)) 1 else which.min(summary(res))
    }
    if (!is.null(res)) {
      fit <- res$fit[[best]]
    } else {
      fit <- ml[[best]](data)
    }
    m <- with(des, list(fit = fit,
                        rf_args = rf_args(hyper_par[[best]]),
                        num.trees=num.trees[best],
                        xlevels = x_levels,
                        terms = terms,
                        action_set = action_set))
    class(m) <- c("g_rf", "g_model")
    return(m)
  }
  return(g_rf)
}

#' @export
predict.g_rf <- function(object, new_H, ...) {
  mf <- with(object, model.frame(terms, data=new_H, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_H, xlev = object$xlevels)
  pr <- predict(object$fit, data=newx, num.threads=1)$predictions
  return(pr)
}

################################################################################
## SuperLearner interface
################################################################################

##' @export
g_sl <- function(formula = ~ ., SL.library=c("SL.mean", "SL.glm"),
                 family=binomial(), ...) {
  if (!requireNamespace("SuperLearner"))
    stop("Package 'SuperLearner' required.")
  suppressPackageStartupMessages(require(SuperLearner))
  force(formula)
  force(SL.library)
  dotdotdot <- list(...)
  g_sl <- function(A, H, action_set) {
    A <- as.numeric(factor(A, levels=action_set))-1
    des <- get_design(formula, data=H)
    X <- data.frame(des$x)
    colnames(X) <- gsub("[^[:alnum:]]", "_", colnames(X))
    sl_args <- append(list(Y=A, X=X, family=family, SL.library=SL.library),
                      dotdotdot)
    fit <- do.call(SuperLearner::SuperLearner, sl_args)
    m <- with(des, list(fit = fit,
                        xlevels = x_levels,
                        terms = terms,
                        action_set = action_set))
    class(m) <- c("g_sl", "g_model")
    return(m)
  }
  return(g_sl)
}

#' @export
predict.g_sl <- function(object, new_H, ...) {
  mf <- with(object, model.frame(terms, data=new_H, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- as.data.frame(model.matrix(mf, data=new_H, xlev = object$xlevels))
  colnames(newx) <- gsub("[^[:alnum:]]", "_", colnames(newx))
  pr <- predict(object$fit, newdata=newx)$pred
  pr <- cbind((1-pr), pr)
  return(pr)
}

################################################################################
## sl3 (SuperLearner) interface
################################################################################

##' @export
g_sl3 <- function(formula = ~ ., learner, folds=5, ...) {
  if (!requireNamespace("sl3"))
    stop("Package 'sl3' required.")
  suppressPackageStartupMessages(require(sl3))
  force(formula)
  dotdotdot <- list(...)
  if (missing(learner)) {
    lrn_enet <- sl3::make_learner(sl3::Lrnr_glmnet, alpha=0.5, outcome_type="binomial")
    lrn_rf <- sl3::make_learner(sl3::Lrnr_ranger, num.trees = 500, outcome_type="binomial")
    lrn_mean <- sl3::make_learner(sl3::Lrnr_mean, outcome_type="binomial")
    lrn_stack <- sl3::Stack$new(lrn_enet, lrn_rf, lrn_mean)
    learner <- sl3::make_learner(sl3::Lrnr_sl, learners = lrn_stack, outcome_type="binomial")
  }
  g_sl3 <- function(A, H, action_set) {
    A <- factor(A, levels=action_set)
    des <- get_design(formula, data=H)
    X <- data.frame(des$x)
    colnames(X) <- gsub("[^[:alnum:]]", "_", colnames(X))
    tsk <- sl3::make_sl3_Task(data=cbind(A, X),
                              outcome_type="categorical",
                              outcome="A",
                              outcome_levels=action_set,
                              covariates=names(X),
                              folds=folds)
    fit <- learner$train(tsk)
    m <- with(des, list(fit = fit,
                        xlevels = x_levels,
                        terms = terms,
                        action_set = action_set))
    class(m) <- c("g_sl3", "g_model")
    return(m)
  }
  return(g_sl3)
}

#' @export
predict.g_sl3 <- function(object, new_H, ...) {
  mf <- with(object, model.frame(terms, data=new_H, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- as.data.frame(model.matrix(mf, data=new_H, xlev = object$xlevels))
  tsk <- sl3::make_sl3_Task(data=newx, covariates=names(newx))
  pr <- object$fit$predict(tsk)
  return(pr)
}
