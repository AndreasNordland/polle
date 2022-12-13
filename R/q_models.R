check_q_formula <- function(formula, data){
  tt <- terms(formula, data = data)
  formula <- reformulate(attr(tt, "term.labels"), response = NULL)
  tt <- terms(formula, data = data)
  v <- all.vars(tt)
  if(!all(v %in% colnames(data))){
    mes <- deparse(formula)
    mes <- paste("The Q-model formula", mes, "is invalid.")
    stop(mes)
  }
}

# Q-model documentation ---------------------------------------------------

#' @title q_model class object
#'
#' @description  Use \code{q_glm()}, \code{q_glmnet()}, \code{q_rf()}, and \code{q_sl()} to construct
#' an outcome regression model/Q-model object.
#' The constructors are used as input for [policy_eval()] and [policy_learn()].
#'
#' @param formula An object of class [formula] specifying the design matrix for
#' the outcome regression model/Q-model at the given stage. The action at the
#' given stage is always denoted 'A', see examples. Use
#' [get_history_names()] to see the additional
#' available variable names.
#' @param family A description of the error distribution and link function to
#' be used in the model.
#' @param model (Only used by \code{q_glm}) If \code{FALSE} model frame will
#' not be saved.
#' @param alpha (Only used by \code{q_glmnet}) The elasticnet mixing parameter
#' between 0 and 1. alpha equal to 1 is the lasso penalty, and alpha equal
#' to 0 the ridge penalty.
#' @param s (Only used by \code{q_glmnet}) Value(s) of the penalty parameter
#' lambda at which predictions are required, see [glmnet::predict.glmnet()].
#' @param num.trees (Only used by \code{q_rf}) Number of trees.
#' @param mtry (Only used by \code{q_rf}) Number of variables to possibly split
#'  at in each node.
#' @param cv_args (Only used by \code{q_rf}) Cross-validation parameters.
#' Only used if multiple hyper-parameters are given. \code{K} is the number
#' of folds and
#' \code{rep} is the number of replications.
#' @param SL.library (Only used by \code{q_sl}) Either a character vector of
#' prediction algorithms or a list containing character vectors,
#' see [SuperLearner::SuperLearner].
#' @param env (Only used by \code{q_sl}) Environment containing the learner
#' functions. Defaults to the calling environment.
#' @param onlySL (Only used by \code{q_sl}) Logical. If TRUE, only saves and computes predictions
#' for algorithms with non-zero coefficients in the super learner object.
#' @param ... Additional arguments passed to [glm()], [glmnet::glmnet],
#' [ranger::ranger] or [SuperLearner::SuperLearner].
#' @details
#' \code{q_glm()} is a wrapper of [glm()] (generalized linear model).\cr
#' \code{q_glmnet()} is a wrapper of [glmnet::glmnet()] (generalized linear model via
#' penalized maximum likelihood).\cr
#' \code{q_rf()} is a wrapper of [ranger::ranger()] (random forest).
#' When multiple hyper-parameters are given, the
#' model with the lowest cross-validation error is selected.\cr
#' \code{q_sl()} is a wrapper of [SuperLearner::SuperLearner] (ensemble model).
#' @returns q_model object: function with arguments 'AH'
#' (combined action and history matrix) and 'V_res' (residual value/expected
#' utility).
#' @docType class
#' @name q_model
#' @seealso [get_history_names()], [get_q_functions()].
#' @examples
#' library("polle")
#' ### Single stage case
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d <- sim_single_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action="A",
#'                   covariates=list("Z", "B", "L"),
#'                   utility="U")
#' pd
#'
#' # available history variable names for the outcome regression:
#' get_history_names(pd)
#'
#' # evaluating the static policy a=1 using inverse
#' # propensity weighting based on the given Q-model:
#' pe <- policy_eval(type = "or",
#'             policy_data = pd,
#'             policy = policy_def(1, name = "A=1"),
#'             q_model = q_glm(formula = ~A*.))
#' pe
#'
#' # getting the fitted Q-function values
#' head(predict(get_q_functions(pe), pd))
#'
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' par0 <- c(gamma = 0.5, beta = 1)
#' d2 <- sim_two_stage(5e2, seed=1, par=par0); rm(par0)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#'
#' # available full history variable names at each stage:
#' get_history_names(pd2, stage = 1)
#' get_history_names(pd2, stage = 2)
#'
#' # evaluating the static policy a=1 using outcome
#' # regression based on a glm model for each stage:
#' pe2 <- policy_eval(type = "or",
#'             policy_data = pd2,
#'             policy = policy_def(1, reuse = TRUE, name = "A=1"),
#'             q_model = list(q_glm(~ A * L_1),
#'                            q_glm(~ A * (L_1 + L_2))),
#'             q_full_history = TRUE)
#' pe2
#'
#' # getting the fitted Q-function values
#' head(predict(get_q_functions(pe2), pd2))
NULL

# glm interface --------------------------------------

#' @rdname q_model
#' @export
q_glm <- function(formula = ~ A*.,
                  family = gaussian(),
                  model = FALSE,
                  ...) {
  force(formula)
  dotdotdot <- list(...)

  q_glm <- function(AH, V_res) {
    data <- AH
    check_q_formula(formula = formula, data = data)
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

    m <- list(glm_model = glm_model)

    class(m) <- c("q_glm")
    return(m)
  }
  class(q_glm) <- c("q_model")
  return(q_glm)
}

predict.q_glm <- function(object, new_AH){
  glm_model <- getElement(object, "glm_model")
  pred <- predict(object$glm_model, newdata = new_AH, type = "response")
  return(pred)
}

# glmnet (Elastic Net) interface ------------------------------------------

#' @rdname q_model
#' @export
q_glmnet <- function(formula = ~ A*.,
                     family = "gaussian",
                     alpha = 1,
                     s = "lambda.min",
                     ...) {
  if (!requireNamespace("glmnet"))
    stop("Package 'glmnet' required.")
  force(formula)
  dotdotdot <- list(...)

  q_glmnet <- function(AH, V_res) {
    check_q_formula(formula = formula, data = AH)
    des <- get_design(formula, data=AH, intercept = TRUE)
    y <- V_res
    args_glmnet <- c(list(y = y, x = des$x,
                        family = family, alpha = alpha), dotdotdot)
    fit <- do.call(glmnet::cv.glmnet, args = args_glmnet)
    fit$call <- NULL
    m <- with(des, list(
                     fit = fit,
                     s = s,
                     formula = formula,
                     terms = terms,
                     xlevels = xlevels))
    class(m) <- c("q_glmnet")
    return(m)
  }
  class(q_glmnet) <- c("q_model")
  return(q_glmnet)
}

predict.q_glmnet <- function(object, new_AH, ...) {
  mf <- with(object, model.frame(terms, data=new_AH, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_AH, xlev = object$xlevels)
  idx_inter <- which(colnames(newx) == "(Intercept)")
  if (length(idx_inter)>0)
    newx <- newx[,-idx_inter, drop = FALSE]
  pred <- predict(getElement(object, "fit"),
                  newx = newx,
                  type = "response",
                  s = getElement(object, "s"))
  return(pred)
}

# ranger (Random Forest) interface ----------------------------------------

perf_ranger <- function(fit, data,  ...) {
  y <- as.numeric(data[, 1])
  x <- data[, -1, drop=FALSE]
  mean((predict(fit, data=x, num.threads=1) - y)^2)^.5
}

#' @rdname q_model
#' @export
q_rf <- function(formula = ~.,
                 num.trees=c(250, 500, 750), mtry=NULL,
                 cv_args=list(K=3, rep=1), ...) {
  if (!requireNamespace("ranger")) stop("Package 'ranger' required.")
  force(formula)
  dotdotdot <- list(...)
  hyper_par <- expand.list(num.trees=num.trees, mtry=mtry)
  rf_args <- function(p) {
    list(num.threads=1, num.trees=p$num.trees, mtry=p$mtry)
  }
  ml <- lapply(hyper_par, function(p)
    function(data) {
      rf_args <- append(rf_args(p), list(y=data[, 1],
                                         x=as.matrix(data[, -1, drop=FALSE])))
      rf_args <- append(rf_args, dotdotdot)
      do.call(ranger::ranger, args=rf_args)
    })

  q_rf <- function(AH, V_res) {
    check_q_formula(formula = formula, data = AH)
    des <- get_design(formula, data=AH, intercept = TRUE)
    data <- data.frame(V_res, des$x)
    colnames(data) <- gsub("[^[:alnum:]]", "_", colnames(data))
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(targeted::cv(ml, data=data, perf=perf_ranger,
                               K=cv_args$K, rep=cv_args$rep),
                      error=function(...) NULL)
      best <- if (is.null(res)) 1 else which.min(summary(res))
    }
    if (!is.null(res)) {
      fit <- res$fit[[best]]
    } else {
      fit <- ml[[best]](data)
    }
    fit$call <- NULL
    res <- with(des, list(fit = fit,
                          rf_args = rf_args(hyper_par[[best]]),
                          num.trees=num.trees[best],
                          xlevels = xlevels,
                          terms = terms))
    class(res) <- c("q_rf")
    return(res)
  }
  class(q_rf) <- c("q_model")
  return(q_rf)
}

predict.q_rf <- function(object, new_AH, ...) {
  mf <- with(object, model.frame(terms, data=new_AH, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=new_AH, xlev = object$xlevels)
  idx_inter <- which(colnames(newx) == "(Intercept)")
  if (length(idx_inter)>0)
    newx <- newx[,-idx_inter, drop = FALSE]
  colnames(newx) <- gsub("[^[:alnum:]]", "_", colnames(newx))
  pr <- predict(object$fit, data=newx, num.threads=1)$predictions
  return(pr)

}

# SuperLearner interface --------------------------------------------------

#' @rdname q_model
#' @export
q_sl <- function(formula = ~ .,
                 SL.library=c("SL.mean", "SL.glm"),
                 env = parent.frame(),
                 onlySL = TRUE,
                 ...){
  if (!requireNamespace("SuperLearner"))
    stop("Package 'SuperLearner' required.")
  dotdotdot <- list(...)
  force(SL.library)
  q_sl <- function(AH, V_res) {
    check_q_formula(formula = formula, data = AH)
    des <- get_design(formula, data=AH, intercept = TRUE)
    if (missing(V_res) || is.null(V_res))
      V_res <- get_response(formula, data=AH)
    X <- as.data.frame(des$x)
    colnames(X) <- gsub("[^[:alnum:]]", "_", colnames(X))
    args_SL <- list(Y = as.numeric(V_res),
                    X = X,
                    SL.library = SL.library,
                    env = env)
    args_SL <- append(args_SL, dotdotdot)
    fit <- do.call(SuperLearner::SuperLearner, args = args_SL)
    fit$call <- NULL
    if(onlySL == TRUE){
      fit$fitLibrary[fit$coef == 0] <- NA
    }

    m <- with(des, list(fit = fit,
                        onlySL = onlySL,
                        xlevels = xlevels,
                        terms = terms))
    class(m) <- c("q_sl")
    return(m)
  }
  class(q_sl) <- c("q_model")
  return(q_sl)
}

predict.q_sl <- function(object, new_AH, ...) {
  onlySL <- getElement(object, "onlySL")
  mf <- with(object, model.frame(terms, data=new_AH, xlev = xlevels,
                                 drop.unused.levels=FALSE))
  newx <- model.matrix(mf, data=as.data.frame(new_AH), xlev = object$xlevels)
  idx_inter <- which(colnames(newx) == "(Intercept)")
  if (length(idx_inter)>0)
    newx <- newx[,-idx_inter, drop = FALSE]
  newx <- as.data.frame(newx)
  colnames(newx) <- gsub("[^[:alnum:]]", "_", colnames(newx))
  pred <- predict(getElement(object, "fit"),
                  newdata = newx,
                  onlySL = onlySL)$pred[, 1]
  return(pred)
}
