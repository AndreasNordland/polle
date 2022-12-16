check_g_formula <- function(formula, data){
  tt <- terms(formula, data = data)
  formula <- reformulate(attr(tt, "term.labels"), response = NULL)
  tt <- terms(formula, data = data)
  v <- all.vars(tt)
  if(!all(v %in% colnames(data))){
    mes <- deparse(formula)
    mes <- paste("The g-model formula", mes, "is invalid.")
    stop(mes)
  }
}

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

check_missing_regressor <- function(formula, data){
  data <- as.data.table(data)
  tt <- terms(formula, data = data)
  tt <- terms(reformulate(attr(tt, "term.labels"), response = NULL),
              data = data)
  v <- all.vars(tt)
  cols_na <- names(which(colSums(is.na(data[,v, with = FALSE])) > 0))
  if (length(cols_na) > 0){
    mes <- paste(cols_na, collapse = ", ")
    mes <- paste("The regression variables ", mes, " have missing NA values.", sep = "")
    warning(mes)
  }
}

get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}

get_design <- function(formula, data) {
  tt <- terms(formula, data=data)
  attr(tt, "intercept") <- 1
  tt <- delete.response(tt)
  op <- options(na.action = "na.pass")
  mf <- model.frame(tt, data=data, drop.unused.levels = TRUE)
  xlevels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  options(op)
  idx_inter <- which(colnames(x) == "(Intercept)")
  if (length(idx_inter)>0)
    x <- x[,-idx_inter, drop = FALSE]
  attr(tt, ".Environment") <- NULL
  colnames(x) <- gsub("[^[:alnum:]]", "_", colnames(x))
  return(list(terms=tt, xlevels=xlevels, x=x))
}

apply_design <- function(design, data){
  terms <- getElement(design, "terms")
  xlevels <- getElement(design, "xlevels")

  op <- options(na.action = "na.pass")
  mf <- model.frame(terms,
                    data=data,
                    xlev = xlevels,
                    drop.unused.levels=FALSE)
  newx <- model.matrix(mf, data=data, xlev = xlevels)
  options(op)

  idx_inter <- which(colnames(newx) == "(Intercept)")
  if (length(idx_inter)>0)
    newx <- newx[,-idx_inter, drop = FALSE]

  colnames(newx) <- gsub("[^[:alnum:]]", "_", colnames(newx))
  return(newx)
}

# g_model documentation -----------------------------------------------------------

#' @title g_model class object
#'
#' @description  Use \code{g_glm()}, \code{g_glmnet()}, \code{g_rf()}, and \code{g_sl()} to construct
#' an action probability model/g-model object.
#' The constructors are used as input for [policy_eval()] and [policy_learn()].
#'
#' @param formula An object of class [formula] specifying the design matrix for
#' the propensity model/g-model. Use [get_history_names()] to view the available
#' variable names.
#' @param family A description of the error distribution and link function to
#' be used in the model.
#' @param model (Only used by \code{g_glm}) If \code{FALSE} model frame will
#' not be saved.
#' @param na.action (Only used by \code{g_glm}) A function which indicates what
#' should happen when the data contain NAs, see [na.pass].
#' @param alpha (Only used by \code{g_glmnet}) The elastic net mixing parameter
#' between 0 and 1. alpha equal to 1 is the lasso penalty, and alpha equal
#' to 0 the ridge penalty.
#' @param s (Only used by \code{g_glmnet}) Value(s) of the penalty parameter
#' lambda at which predictions are required, see [glmnet::predict.glmnet()].
#' @param num.trees (Only used by \code{g_rf}) Number of trees.
#' @param mtry (Only used by \code{g_rf}) Number of variables to possibly split
#'  at in each node.
#' @param cv_args (Only used by \code{g_rf}) Cross-validation parameters.
#' Only used if multiple hyper-parameters are given. \code{K} is the number
#' of folds and
#' \code{rep} is the number of replications.
#' @param SL.library (Only used by \code{g_sl}) Either a character vector of prediction algorithms or
#' a list containing character vectors, see [SuperLearner::SuperLearner].
#' @param env (Only used by \code{g_sl}) Environment containing the learner functions. Defaults to the calling environment.
#' @param onlySL (Only used by \code{g_sl}) Logical. If TRUE, only saves and computes predictions
#' for algorithms with non-zero coefficients in the super learner object.
#' @param ... Additional arguments passed to [glm()], [glmnet::glmnet],
#' [ranger::ranger] or [SuperLearner::SuperLearner].
#' @details
#' \code{g_glm()} is a wrapper of [glm()] (generalized linear model).\cr
#' \code{g_glmnet()} is a wrapper of [glmnet::glmnet()] (generalized linear model via
#' penalized maximum likelihood).\cr
#' \code{g_rf()} is a wrapper of [ranger::ranger()] (random forest).
#' When multiple hyper-parameters are given, the
#' model with the lowest cross-validation error is selected.\cr
#' \code{g_sl()} is a wrapper of [SuperLearner::SuperLearner] (ensemble model).
#' @returns g-model object: function with arguments 'A'
#' (action vector), 'H' (history matrix) and 'action_set'.
#' @seealso [get_history_names()], [get_g_functions()].
#' @docType class
#' @name g_model
#' @examples
#' library("polle")
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d2 <- sim_two_stage(2e2, seed=1)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#'
#' # available state history variable names:
#' get_history_names(pd2)
#' # defining a g-model:
#' g_model <- g_glm(formula = ~B+C)
#'
#' # evaluating the static policy (A=1) using inverse propensity weighting
#' # based on a state glm model across all stages:
#' pe2 <- policy_eval(type = "ipw",
#'                    policy_data = pd2,
#'                    policy = policy_def(1, reuse = TRUE),
#'                    g_models = g_model)
#' # inspecting the fitted g-model:
#' get_g_functions(pe2)
#'
#' # available full history variable names at each stage:
#' get_history_names(pd2, stage = 1)
#' get_history_names(pd2, stage = 2)
#'
#' # evaluating the same policy based on a full history
#' # glm model for each stage:
#' pe2 <- policy_eval(type = "ipw",
#'                    policy_data = pd2,
#'                    policy = policy_def(1, reuse = TRUE),
#'                    g_models = list(g_glm(~ L_1 + B),
#'                                    g_glm(~ A_1 + L_2 + B)),
#'                    g_full_history = TRUE)
#' # inspecting the fitted g-models:
#' get_g_functions(pe2)
NULL

# glm interface --------------------------------------

#' @rdname g_model
#' @export
g_glm <- function(formula = ~.,
                  family = "binomial",
                  model = FALSE,
                  na.action = na.pass,
                  ...) {
  formula <- as.formula(formula)
  dotdotdot <- list(...)

  g_glm <- function(A, H, action_set){
    check_g_formula(formula = formula, data = H)
    formula <- update_g_formula(formula, A, H)

    args_glm <- append(list(formula = formula,
                            data = H,
                            family = family,
                            model = model,
                            na.action = na.action),
                       dotdotdot)

    model <- do.call(what = "glm", args = args_glm)
    model$call <- NULL

    m <- list(model = model)
    class(m) <- c("g_glm")
    return(m)
  }
  class(g_glm) <- c("g_model", "function")
  return(g_glm)
}
predict.g_glm <- function(object, new_H){
  model <- getElement(object, "model")
  fit <- predict.glm(object = model, newdata = new_H, type = "response")
  probs <- cbind((1-fit), fit)
  return(probs)
}


# glmnet (Elastic Net) interface ------------------------------------------

#' @rdname g_model
#' @export
g_glmnet <- function(formula = ~.,
                     family = "binomial",
                     alpha = 1,
                     s = "lambda.min", ...) {
  if (!requireNamespace("glmnet")) stop("Package 'glmnet' required")
  force(formula)
  dotdotdot <- list(...)
  g_glmnet <- function(A, H, action_set){
    check_g_formula(formula = formula, data = H)
    check_missing_regressor(formula = formula, data = H)
    formula <- update_g_formula(formula, A, H)
    y <- get_response(formula, data=H)
    des <- get_design(formula, data=H)
    if (ncol(des$x)<2)
      stop("g_glmnet requires a model matrix with two or more columns.")

    args_glmnet <- list(y = y,  x = des$x, family = family, alpha = alpha)
    args_glmnet <- append(args_glmnet, dotdotdot)
    model <- do.call(glmnet::cv.glmnet, args = args_glmnet)
    model$call <- NULL

    m  <- list(model = model,
               s = s,
               design = des,
               action_set = action_set)
    class(m) <- c("g_glmnet")
    return(m)
  }
  class(g_glmnet) <- c("g_model", "function")
  return(g_glmnet)
}

predict.g_glmnet <- function(object, new_H) {
  design <- getElement(object, "design")
  model <- getElement(object, "model")
  s <- getElement(object, "s")

  newx <- apply_design(design, data = new_H)
  fit <- predict(model,  newx = newx, type = "response", s = s)
  probs <- cbind((1-fit), fit)
  return(probs)
}

# ranger (Random Forest) interface ----------------------------------------

perf_ranger_prob <- function(fit, data,  ...) {
  score_class(predict(fit, data=as.matrix(data[,-1,drop=FALSE]),
                      num.threads=1)$predictions,
              data[,1], ...)
}

#' @rdname g_model
#' @export
g_rf <- function(formula = ~.,
                 num.trees=c(500),
                 mtry=NULL,
                 cv_args=list(K=5, rep=1),
                 ...) {
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
    A <- factor(A, levels = action_set)
    check_g_formula(formula = formula, data = H)
    des <- get_design(formula, data=H)
    data <- data.frame(A, des$x)
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(targeted::cv(ml, data=data, perf=perf_ranger_prob,
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

    m <- list(model = model,
              rf_args = rf_args(hyper_par[[best]]),
              num.trees=num.trees[best],
              design = des,
              action_set = action_set)
    class(m) <- c("g_rf")
    return(m)
  }
  class(g_rf) <- c("g_model", "function")
  return(g_rf)
}

predict.g_rf <- function(object, new_H, ...){
  model <- getElement(object, "model")
  design <- getElement(object, "design")

  new_data <- apply_design(design = design, data = new_H)
  pr <- predict(model, data=new_data, num.threads=1)$predictions
  return(pr)
}

# SuperLearner interface --------------------------------------------------

#' @rdname g_model
#' @export
g_sl <- function(formula = ~ .,
                 SL.library=c("SL.mean", "SL.glm"),
                 family=binomial(),
                 env = parent.frame(),
                 onlySL = TRUE,
                 ...) {
  if (!requireNamespace("SuperLearner"))
    stop("Package 'SuperLearner' required.")
  force(formula)
  force(SL.library)
  dotdotdot <- list(...)
  g_sl <- function(A, H, action_set) {
    A <- as.numeric(factor(A, levels=action_set))-1
    check_g_formula(formula = formula, data = H)
    des <- get_design(formula, data=H)
    sl_args <- append(list(Y=A,
                           X=as.data.frame(des$x),
                           family=family,
                           SL.library=SL.library,
                           env = env),
                      dotdotdot)
    model <- do.call(SuperLearner::SuperLearner, sl_args)
    model$call <- NULL
    if(onlySL == TRUE){
      model$fitLibrary[model$coef == 0] <- NA
    }
    m <- list(model = model,
              design = des,
              onlySL = onlySL,
              action_set = action_set)

    class(m) <- c("g_sl")
    return(m)
  }
  class(g_sl) <- c("g_model", "function")
  return(g_sl)
}

#' @export
predict.g_sl <- function(object, new_H, ...) {
  model <- getElement(object, "model")
  design <- getElement(object, "design")
  onlySL <- getElement(object, "onlySL")

  newdata <- apply_design(design = design, data = new_H)
  newdata <- as.data.frame(newdata)
  pr <- predict(model,
                newdata=newdata,
                onlySL = onlySL)$pred
  pr <- cbind((1-pr), pr)
  return(pr)
}

## sl3 (SuperLearner) interface ----

# g_sl3 <- function(formula = ~ ., learner, folds=5, ...) {
#   if (!requireNamespace("sl3"))
#     stop("Package 'sl3' required.")
#   force(formula)
#   dotdotdot <- list(...)
#   if (missing(learner)) {
#     lrn_enet <- sl3::make_learner(sl3::Lrnr_glmnet, alpha=0.5, outcome_type="binomial")
#     lrn_rf <- sl3::make_learner(sl3::Lrnr_ranger, num.trees = 500, outcome_type="binomial")
#     lrn_mean <- sl3::make_learner(sl3::Lrnr_mean, outcome_type="binomial")
#     lrn_stack <- sl3::Stack$new(lrn_enet, lrn_rf, lrn_mean)
#     learner <- sl3::make_learner(sl3::Lrnr_sl, learners = lrn_stack, outcome_type="binomial")
#   }
#   g_sl3 <- function(A, H, action_set) {
#     A <- factor(A, levels=action_set)
#     check_g_formula(formula = formula, data = H)
#     des <- get_design(formula, data=H)
#     X <- data.frame(des$x)
#     colnames(X) <- gsub("[^[:alnum:]]", "_", colnames(X))
#     tsk <- sl3::make_sl3_Task(data=cbind(A, X),
#                               outcome_type="categorical",
#                               outcome="A",
#                               outcome_levels=action_set,
#                               covariates=names(X),
#                               folds=folds)
#     fit <- learner$train(tsk)
#     m <- with(des, list(fit = fit,
#                         xlevels = xlevels,
#                         terms = terms,
#                         action_set = action_set))
#     class(m) <- c("g_sl3")
#     return(m)
#   }
#   class(g_sl3) <- c("g_model", "function")
#   return(g_sl3)
# }
#
# predict.g_sl3 <- function(object, new_H, ...) {
#   mf <- with(object, model.frame(terms, data=new_H, xlev = xlevels,
#                                  drop.unused.levels=FALSE))
#   newx <- as.data.frame(model.matrix(mf, data=new_H, xlev = object$xlevels))
#   tsk <- sl3::make_sl3_Task(data=newx, covariates=names(newx))
#   pr <- object$fit$predict(tsk)
#   return(pr)
# }
