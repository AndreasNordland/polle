check_q_formula <- function(formula, data){
  tt <- terms(formula, data = data)
  if (length(attr(tt, "term.labels"))>0){
    formula <- reformulate(attr(tt, "term.labels"), response = NULL)
    tt <- terms(formula, data = data)
    v <- all.vars(tt)
    if(!all(v %in% colnames(data))){
      mes <- deparse(formula)
      mes <- paste("The Q-model formula", mes, "is invalid.")
      stop(mes)
    }
  }
}

update_q_formula <- function(formula, data, V_res){
  tt <- terms(formula, data = data)
  if (length(attr(tt, "term.labels")) == 0)
    formula <- V_res ~ 1
  else
    formula <- reformulate(attr(tt, "term.labels"), response = "V_res")

  return(formula)
}

new_q_model <- function(q_model){
  class(q_model) <- c("q_model", "function")
  return(q_model)
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
#' @param na.action (Only used by \code{q_glm}) A function which indicates what
#' should happen when the data contain NAs, see [na.pass].
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
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1,
#'                    action="A",
#'                    covariates=list("Z", "B", "L"),
#'                    utility="U")
#' pd1
#'
#' # available history variable names for the outcome regression:
#' get_history_names(pd1)
#'
#' # evaluating the static policy a=1 using inverse
#' # propensity weighting based on the given Q-model:
#' pe1 <- policy_eval(type = "or",
#'                    policy_data = pd1,
#'                    policy = policy_def(1, name = "A=1"),
#'                    q_model = q_glm(formula = ~A*.))
#' pe1
#'
#' # getting the fitted Q-function values
#' head(predict(get_q_functions(pe1), pd1))
#'
#' ### Two stages:
#' d2 <- sim_two_stage(5e2, seed=1)
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
                  na.action = na.pass,
                  ...){
  formula <- as.formula(formula)
  environment(formula) <- NULL
  dotdotdot <- list(...)

  q_glm <- function(AH, V_res) {
    data <- AH
    check_q_formula(formula = formula, data = data)
    formula <- update_q_formula(formula = formula, data = data, V_res = V_res)

    args_glm <- list(
      formula = formula,
      data = data,
      family = family,
      model = model,
      na.action = na.action
    )
    args_glm <- append(args_glm, dotdotdot)
    model <- do.call(what = "glm", args = args_glm)
    model$call <- NULL

    m <- list(model = model)

    class(m) <- c("q_glm")
    return(m)
  }
  q_glm <- new_q_model(q_glm)
  return(q_glm)
}

predict.q_glm <- function(object, new_AH){
  model <- getElement(object, "model")
  pred <- predict(model, newdata = new_AH, type = "response")
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
  formula <- as.formula(formula)
  environment(formula) <- NULL
  dotdotdot <- list(...)

  q_glmnet <- function(AH, V_res) {
    check_q_formula(formula = formula, data = AH)
    check_missing_regressor(formula = formula, data = AH)
    des <- get_design(formula, data=AH)
    y <- V_res
    args_glmnet <- c(list(y = y, x = des$x,
                        family = family, alpha = alpha), dotdotdot)
    model <- do.call(glmnet::cv.glmnet, args = args_glmnet)
    model$call <- NULL

    des$x <- NULL
    m  <- list(model = model,
               s = s,
               design = des)

    class(m) <- c("q_glmnet")
    return(m)
  }
  q_glmnet <- new_q_model(q_glmnet)
  return(q_glmnet)
}

predict.q_glmnet <- function(object, new_AH, ...) {
  design <- getElement(object, "design")
  model <- getElement(object, "model")
  s <- getElement(object, "s")

  newx <- apply_design(design, data = new_AH)
  pred <- predict(model,
                  newx = newx,
                  type = "response",
                  s = s)
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
  formula <- as.formula(formula)
  environment(formula) <- NULL
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
    des <- get_design(formula, data=AH)
    data <- data.frame(V_res, des$x)
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(targeted::cv(ml, data=data, perf=perf_ranger,
                               K=cv_args$K, rep=cv_args$rep),
                      error=function(...) NULL)
      best <- if (is.null(res)) 1 else which.min(summary(res))
    }
    if (!is.null(res)) {
      model <- res$fit[[best]]
    } else {
      model <- ml[[best]](data)
    }
    model$call <- NULL

    des$x <- NULL
    m <- list(model = model,
              rf_args = rf_args(hyper_par[[best]]),
              num.trees=num.trees[best],
              design = des)
    class(m) <- c("q_rf")
    return(m)
  }
  q_rf <- new_q_model(q_rf)
  return(q_rf)
}

predict.q_rf <- function(object, new_AH, ...) {
  model <- getElement(object, "model")
  design <- getElement(object, "design")

  new_data <- apply_design(design = design, data = new_AH)
  pr <- predict(model, data=new_data, num.threads=1)$predictions
  return(pr)
}

# SuperLearner interface --------------------------------------------------

#' @rdname q_model
#' @export
q_sl <- function(formula = ~ .,
                 SL.library=c("SL.mean", "SL.glm"),
                 env = as.environment("package:SuperLearner"),
                 onlySL = TRUE,
                 ...){
  if (!requireNamespace("SuperLearner"))
    stop("Package 'SuperLearner' required.")
  formula <- as.formula(formula)
  environment(formula) <- NULL
  force(SL.library)
  force(env)
  dotdotdot <- list(...)
  q_sl <- function(AH, V_res) {
    check_q_formula(formula = formula, data = AH)
    des <- get_design(formula, data=AH)
    if (missing(V_res) || is.null(V_res))
      V_res <- get_response(formula, data=AH)
    args_SL <- list(Y = as.numeric(V_res),
                    X = as.data.frame(des$x),
                    SL.library = SL.library,
                    env = env)
    args_SL <- append(args_SL, dotdotdot)
    model <- do.call(SuperLearner::SuperLearner, args = args_SL)
    model$call <- NULL
    if(all(model$coef == 0))
      stop("In q_sl(): All metalearner coefficients are zero.")

    if(onlySL == TRUE){
      model$fitLibrary[model$coef == 0] <- NA
    }

    des$x <- NULL
    m <- list(model = model,
              design = des,
              onlySL = onlySL)
    class(m) <- c("q_sl")
    return(m)
  }
  q_sl <- new_q_model(q_sl)
  return(q_sl)
}

predict.q_sl <- function(object, new_AH, ...) {
  model <- getElement(object, "model")
  design <- getElement(object, "design")
  onlySL <- getElement(object, "onlySL")
  newdata <- apply_design(design = design, data = new_AH)
  newdata <- as.data.frame(newdata)
  pred <- predict(model,
                     newdata = newdata,
                     onlySL = onlySL)$pred[, 1]
  return(pred)
}
