check_g_formula <- function(formula, data){
  tt <- terms(formula, data = data)
  if (length(attr(tt, "term.labels")) == 0)
    return(NULL)
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
    mes <- paste("The regression variables ", mes, " have missing (NA) values.", sep = "")
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

  mf <- tryCatch(model.frame(tt, data=data, drop.unused.levels = TRUE),
                    error = function(e) e
  )
  if (inherits(mf, "error")) {
    mf$message <-
      paste0(mf$message, " when calling model.frame with formula:\n",
             format(formula))
    stop(mf)
  }

  xlevels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  options(op)
  idx_inter <- which(colnames(x) == "(Intercept)")
  if (length(idx_inter)>0)
    x <- x[,-idx_inter, drop = FALSE]
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

supp_warnings <- function(expr, mess, fun) {
  if(!is.character(mess))
    stop()
  if(!is.character(fun))
    stop()
  if(length(mess) != length(fun))
    stop()

  eval.parent(
    substitute(
      withCallingHandlers(expr, warning = function (w) {
        mess_ <- mess
        fun_ <- fun
        cm   <- conditionMessage(w)
        cc <- conditionCall(w)
        cond_cc <- FALSE
        if (is.call(cc) & length(as.character(cc))>0){
          cc <- as.character(cc)[[1]]
          cond_cc <- (cc == fun_)
        }
        cond_cm <- (cm == mess_)
        if (any(cond_cm & cond_cc))
          tryInvokeRestart("muffleWarning")
      })
    )
  )
}

new_g_model <- function(g_model){
  class(g_model) <- c("g_model", "function")
  return(g_model)
}

# g_model documentation -----------------------------------------------------------

#' @title g_model class object
#'
#' @description  Use \code{g_glm()}, \code{g_empir()},
#' \code{g_glmnet()}, \code{g_rf()}, \code{g_sl()}, \code{g_xgboost} to construct
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
#' @param cv_args (Only used by \code{g_rf} and \code{g_xgboost}) Cross-validation parameters.
#' Only used if multiple hyper-parameters are given. \code{K} is the number
#' of folds and
#' \code{rep} is the number of replications.
#' @param SL.library (Only used by \code{g_sl}) Either a character vector of prediction algorithms or
#' a list containing character vectors, see [SuperLearner::SuperLearner].
#' @param env (Only used by \code{g_sl}) Environment containing the learner functions. Defaults to the calling environment.
#' @param onlySL (Only used by \code{g_sl}) Logical. If TRUE, only saves and computes predictions
#' for algorithms with non-zero coefficients in the super learner object.
#' @param objective (Only used by \code{g_xgboost}) specify the learning
#' task and the corresponding learning objective, see [xgboost::xgboost].
#' @param nrounds (Only used by \code{g_xgboost}) max number of boosting iterations.
#' @param max_depth (Only used by \code{g_xgboost}) maximum depth of a tree.
#' @param eta (Only used by \code{g_xgboost}) learning rate.
#' @param nthread (Only used by \code{g_xgboost}) number of threads.
#' @param params (Only used by \code{g_xgboost}) list of parameters.
#' @param ... Additional arguments passed to [glm()], [glmnet::glmnet],
#' [ranger::ranger] or [SuperLearner::SuperLearner].
#' @details
#' \code{g_glm()} is a wrapper of [glm()] (generalized linear model).\cr
#' \code{g_empir()} calculates the empirical probabilities within the groups
#' defined by the formula.\cr
#' \code{g_glmnet()} is a wrapper of [glmnet::glmnet()] (generalized linear model via
#' penalized maximum likelihood).\cr
#' \code{g_rf()} is a wrapper of [ranger::ranger()] (random forest).
#' When multiple hyper-parameters are given, the
#' model with the lowest cross-validation error is selected.\cr
#' \code{g_sl()} is a wrapper of [SuperLearner::SuperLearner] (ensemble model).\cr
#' \code{g_xgboost()} is a wrapper of [xgboost::xgboost].
#' @returns g-model object: function with arguments 'A'
#' (action vector), 'H' (history matrix) and 'action_set'.
#' @seealso [get_history_names()], [get_g_functions()].
#' @docType class
#' @name g_model
#' @examples
#' library("polle")
#' ### Two stages:
#' d <- sim_two_stage(2e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # available state history variable names:
#' get_history_names(pd)
#' # defining a g-model:
#' g_model <- g_glm(formula = ~B+C)
#'
#' # evaluating the static policy (A=1) using inverse propensity weighting
#' # based on a state glm model across all stages:
#' pe <- policy_eval(type = "ipw",
#'                   policy_data = pd,
#'                   policy = policy_def(1, reuse = TRUE),
#'                  g_models = g_model)
#' # inspecting the fitted g-model:
#' get_g_functions(pe)
#'
#' # available full history variable names at each stage:
#' get_history_names(pd, stage = 1)
#' get_history_names(pd, stage = 2)
#'
#' # evaluating the same policy based on a full history
#' # glm model for each stage:
#' pe <- policy_eval(type = "ipw",
#'                    policy_data = pd,
#'                    policy = policy_def(1, reuse = TRUE),
#'                    g_models = list(g_glm(~ L_1 + B),
#'                                    g_glm(~ A_1 + L_2 + B)),
#'                    g_full_history = TRUE)
#' # inspecting the fitted g-models:
#' get_g_functions(pe)
NULL


# empirical (group) probability -------------------------------------------

calculate_prop_table <- function(data, formula){
  tt <- terms(formula, data = data)
  # grouping variables:
  v <- all.vars(tt)
  N_ <- N_group_ <- empir_prob <- A <-  NULL
  tab <- data[ , list(N_ = .N), by = c("A", v)]
  tab[, N_group_ := sum(N_), by = v]
  tab <- tab[, empir_prob := N_ / N_group_][order(A), ]
  tab[, c("N_","N_group_") := NULL]

  return(list(tab=tab, v=v))
}

#' @rdname g_model
#' @export
g_empir <- function(formula = ~1, ...) {
  formula <- as.formula(formula)
  environment(formula) <- NULL

  g_empir <- function(A, H, action_set){
    check_g_formula(formula = formula, data = H)
    data <- cbind(A, H)
    tmp <- calculate_prop_table(data, formula = formula)
    tab <- tmp[["tab"]]
    v <- tmp[["v"]]
    out <- list(tab=tab, action_set=action_set, v=v)
    class(out) <- c("g_empir")
    return(out)
  }
  # setting class:
  g_empir <- new_g_model(g_empir)
  return(g_empir)
}
#' @export
predict.g_empir <- function(object, new_H, ...){
  tab <- object[["tab"]]
  action_set <- object[["action_set"]]
  v <- object[["v"]]

  if (length(v) == 0){
    A <- NULL
    probs <- tab[order(A),]$empir_prob
    probs <- matrix(probs,
                    nrow = nrow(new_H),
                    ncol = length(action_set),
                    byrow = TRUE)
  } else{
    probs <- matrix(0,
                    nrow = nrow(new_H),
                    ncol = length(action_set),
                    byrow = TRUE)
    new_H <- cbind(id = 1:nrow(new_H), new_H)
    for (j in seq_along(action_set)){
      A_ <- action_set[j]
      id <- A <- NULL
      tmp <- merge(new_H, tab[A == A_,], all.x = TRUE)[order(id)]
      tmp[is.na(tmp)] <- 0
      probs[,j] <- tmp[["empir_prob"]]
    }
  }

  return(probs)
}

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
    formula <- update_g_formula(formula, A, H)

    args_glm <- append(list(formula = formula,
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
    class(m) <- c("g_glm")
    return(m)
  }

  # setting class:
  g_glm <- new_g_model(g_glm)

  return(g_glm)
}
#' @export
predict.g_glm <- function(object, new_H, ...){
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
  formula <- as.formula(formula)
  dotdotdot <- list(...)
  g_glmnet <- function(A, H, action_set){
    formula <- update_g_formula(formula, A, H)
    y <- get_response(formula, data=H)
    des <- get_design(formula, data=H)
    if (ncol(des$x)<2)
      stop("g_glmnet requires a model matrix with two or more columns.")

    args_glmnet <- list(y = y,  x = des$x, family = family, alpha = alpha)
    args_glmnet <- append(args_glmnet, dotdotdot)

    model <- tryCatch(do.call(glmnet::cv.glmnet, args = args_glmnet),
                      error = function(e) e
    )
    if (inherits(model, "error")) {
      model$message <-
        paste0(model$message, " when calling 'g_glmnet' with formula:\n",
               format(formula))
      stop(model)
    }

    model$call <- NULL
    des$x <- NULL

    m  <- list(model = model,
               s = s,
               design = des,
               action_set = action_set)
    class(m) <- c("g_glmnet")
    return(m)
  }
  # setting class:
  g_glmnet <- new_g_model(g_glmnet)

  return(g_glmnet)
}

#' @export
predict.g_glmnet <- function(object, new_H, ...) {
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
                 cv_args=list(nfolds=5, rep=1),
                 ...) {
  if (!requireNamespace("ranger")) stop("Package 'ranger' required")
  formula <- as.formula(formula)
  environment(formula) <- NULL
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
    des <- get_design(formula, data=H)
    data <- data.frame(A, des$x)
    res <- NULL; best <- 1
    if (length(ml)>1) {
      res <- tryCatch(targeted::cv(ml, data=data, perf=perf_ranger_prob,
                               nfolds=cv_args$nfolds, rep=cv_args$rep),
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
              design = des,
              action_set = action_set)
    class(m) <- c("g_rf")
    return(m)
  }
  # setting class:
  g_rf <- new_g_model(g_rf)

  return(g_rf)
}

#' @export
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
  formula <- as.formula(formula)
  force(SL.library)
  force(env)
  dotdotdot <- list(...)
  g_sl <- function(A, H, action_set) {
    A <- as.numeric(factor(A, levels=action_set))-1
    des <- get_design(formula, data=H)
    sl_args <- append(list(Y=A,
                           X=as.data.frame(des$x),
                           family=family,
                           SL.library=SL.library,
                           env = env),
                      dotdotdot)
    model <- do.call(SuperLearner::SuperLearner, sl_args)

    model$call <- NULL
    if(all(model$coef == 0))
      stop("In g_sl(): All metalearner coefficients are zero.")
    if(onlySL == TRUE){
      model$fitLibrary[model$coef == 0] <- NA
    }
    des$x <- NULL

    m <- list(model = model,
              design = des,
              onlySL = onlySL,
              action_set = action_set)

    class(m) <- c("g_sl")
    return(m)
  }
  # setting class:
  g_sl <- new_g_model(g_sl)

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

# sl3 (SuperLearner) interface ----

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


# xgboost interface -----------------------------------------------------------------

#' @rdname g_model
#' @export
g_xgboost <- function(formula = ~.,
                      objective = "binary:logistic",
                      params = list(),
                      nrounds,
                      max_depth = 6,
                      eta = 0.3,
                      nthread = 1,
                      cv_args=list(nfolds=3, rep=1)) {
  if (!requireNamespace("xgboost")) stop("Package 'xgboost' required")
  formula <- as.formula(formula)
  environment(formula) <- NULL

  ml <- function(formula = A~., objective,
                 params, nrounds, max_depth,
                 eta, nthread){
    targeted::ml_model$new(formula,
                           info = "xgBoost",
                           estimate = function(x, y) {
                             xgboost::xgboost(
                               data = x, label = y,
                               objective = objective,
                               params = params,
                               nrounds = nrounds,
                               max_depth = max_depth,
                               eta = eta,
                               nthread = nthread,
                               verbose = 0, print_every = 0)
                           })
  }

  ml_args <- expand.list(
    nrounds = nrounds,
    max_depth = max_depth,
    eta = eta
  )
  cv_par <- ml_args
  ml_args <- lapply(ml_args, function(p){
    p <- append(p,
                list(
                  objective = objective,
                  params = params,
                  nthread = nthread
                ))
    return(p)
  })
  ml_models <- lapply(ml_args, function(par) do.call(ml, par))

  g <- function(A, H, action_set) {
    # checks:
    if (length(action_set) != 2)
      stop("g_xgboost in only implemented for a dichotomous action set.")

    # formatting data:
    A <- factor(A, levels = action_set)
    A <- as.numeric(A) - 1
    des <- get_design(formula, data=H)
    data <- data.frame(A, des$x)

    # cross-validating models
    cv_res <- NULL
    if (length(ml_models)>1){
      cv_res <- tryCatch(targeted::cv(ml_models, data, nfolds = cv_args$nfolds, rep = cv_args$rep),
        error = function(e) e
        )
      if (inherits(cv_res, "error")) {
        cv_res$message <-
          paste0(cv_res$message, " when calling 'g_xgboost' with formula:\n",
                 format(formula))
        stop(cv_res)
      }
      cv_res$names <- unlist(lapply(cv_par, function(x) paste(paste(names(x), x, sep = ":"), collapse = ",")))
      ml_args_best <- ml_args[[which.min(coef(cv_res)[, 1])]]
      model <- do.call(ml, ml_args_best)
      model <- model$estimate(data)
    } else {
      model <- do.call(ml, ml_args[[1]])
      model <- model$estimate(data)
    }

    # setting model output
    des$x <- NULL
    m <- list(model = model,
              design = des,
              action_set = action_set,
              cv_res = cv_res)
    class(m) <- c("g_xgboost")
    return(m)
  }
  # setting class:
  g <- new_g_model(g)

  return(g)
}

#' @export
predict.g_xgboost <- function(object, new_H, ...){
  model <- getElement(object, "model")
  design <- getElement(object, "design")
  new_data <- apply_design(design = design, data = new_H)

  fit <- predict(model, newdata = new_data)
  probs <- cbind((1-fit), fit)

  return(probs)
}
