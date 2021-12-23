##' @export
coef.policy_eval <- function(object, ...) {
  return(object$value_estimate)
}

##' @export
iid.policy_eval <- function(x, ...) {
  res <- cbind(x$iid)
  return(res/NROW(res))
}

##' @export
vcov.policy_eval <- function(object, ...) {
  return(crossprod(iid(object)))
}

##' @export
print.policy_eval <- function(x, ...) {
  print(summary(x, ...))
}

##' @export
summary.policy_eval <- function(object, ...) {
  lava::estimate(object, ...)
}

##' @export
estimate.policy_eval <- function(x, ..., labels=x$name) {
  p <- length(coef(x))
  if (is.null(labels)) {
    if (p==1) {
      "value"
    } else {
      labels <- paste0("value", seq(p))
    }
  }
  return(lava::estimate(NULL, coef=coef(x), iid=iid(x), labels=labels, ...))
}

##' @export
"merge.policy_eval" <- function(x, y, ..., paired=TRUE) {
  dots <- list(...)
  idx <- names(dots)%in%formalArgs(lava::estimate.default)[-1]
  est_args <- list()
  if (length(idx)>0) {
    est_args <- dots[which(idx)]
    dots <- dots[-which(idx)]
  }
  m <- lapply(c(list(x, y),dots), function(p)
    do.call(estimate, c(list(p),est_args)))
  do.call("merge", c(m, list(paired=paired)))
}

##' @export
"+.policy_eval" <- function(x,...) {
  merge(x, ...)
}

##' Policy Evaluation
##'
##' Policy evaluation function
##' @export
##' @param policy_data Policy data object
##' @param policy Policy object
##' @param g_models Propensity model object
##' @param q_models Outcome regression/Q-model
##' @param g_functions
##' @param q_functions
##' @param g_full_history Full history or Markov
##' @param q_full_history Full history or Markov
##' @param M Number of folds
##' @param type Type of model (dr, cv, ipw, or, ...)
##' @param ... Additional arguments parsed to lower level functions
policy_eval <- function(policy_data,
                        policy = NULL, policy_learner = NULL,
                        g_functions=NULL, g_models=g_glm(), g_full_history = FALSE,
                        q_functions=NULL, q_models=q_glm(), q_full_history = FALSE,
                        M=5, seed = NULL, type="dr", verbose = FALSE,
                        future.seed = NULL, ...) {
  type <- tolower(type)
  fm <- formals()[-(1:3)]
  fm[["..."]] <- NULL
  cl <- match.call(expand.dots=TRUE)
  for (i in setdiff(names(fm), names(cl)))
    cl[i] <- list(fm[[i]])

  if (type%in%c("cv", "crossfit", "cf", "cv_dr")) {
    cl[[1]] <- policy_eval_cv_dr
  }
  if (type%in%c("dr")) {
    cl[[1]] <- policy_eval_dr
  }
  if (type%in%c("or","q")) {
    cl[[1]] <- policy_eval_or
  }
  if (type%in%c("ipw")) {
    cl[[1]] <- policy_eval_ipw
  }
  val <- eval(cl)
  val$name <- attr(policy, "name")
  return(val)
}


policy_eval_dr_fold <- function(fold,
                                policy_data,
                                policy, policy_learner,
                                g_models, g_functions, g_full_history,
                                q_models, q_functions, q_full_history,
                                verbose,
                                dotdotdot){

  K <- get_K(policy_data)
  id <- get_id(policy_data)
  train_id <- id[-fold]
  validation_id <- id[fold]

  train_policy_data <- subset(policy_data, train_id)
  if (train_policy_data$dim$K != K) stop("The number of stages varies accross the training folds.")
  validation_policy_data <- subset(policy_data, validation_id)
  if (validation_policy_data$dim$K != K) stop("The number of stages varies accross the validation folds.")

  train_args <- list(policy_data = train_policy_data,
                     policy = policy, policy_learner = policy_learner,
                     g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                     q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
                     verbose = verbose)
  train_args <- append(train_args, dotdotdot)
  train_pe_dr <- do.call(what = "policy_eval_dr", args = train_args)

  # getting the policy:
  if (is.null(policy)){
    policy <- get_policy(train_pe_dr$policy_object)
  }

  validation_args <- list(policy_data = validation_policy_data,
                          policy = policy,
                          g_functions = train_pe_dr$g_functions,
                          q_functions = train_pe_dr$q_functions)
  validation_pe_dr <- do.call(what = "policy_eval_dr", args = validation_args)

  if (!is.null(train_pe_dr$policy_object)){
    validation_pe_dr$policy_object <- train_pe_dr$policy_object
  }

  return(validation_pe_dr)
}

policy_eval_cv_dr <- function(policy_data,
                              policy = NULL, policy_learner = NULL,
                              g_models = NULL, g_functions = NULL, g_full_history,
                              q_models = NULL, q_functions = NULL, q_full_history,
                              M, seed = NULL,
                              verbose = FALSE,
                              future.seed = NULL, ...){

  n <- get_n(policy_data)
  id <- get_id(policy_data)

  # setting up the folds
  if (!is.null(seed))
    set.seed(seed)
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))

  dotdotdot <- list(...)
  pe_dr_cv <- future.apply::future_lapply(
    folds,
    FUN = policy_eval_dr_fold,
    policy_data = policy_data,
    policy = policy, policy_learner = policy_learner,
    g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
    q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
    verbose = verbose,
    future.seed = future.seed,
    dotdotdot = dotdotdot
  )

  id <- unlist(lapply(pe_dr_cv, function(x) x$id))
  iid <- unlist(lapply(pe_dr_cv, function(x) x$iid))

  n <- unlist(lapply(pe_dr_cv, function(x) length(x$id)))
  value_estimate <- unlist(lapply(pe_dr_cv, function(x) x$value_estimate))
  value_estimate <- sum((n / sum(n)) * value_estimate)

  value_estimate_ipw <- unlist(lapply(pe_dr_cv, function(x) x$value_estimate_ipw))
  value_estimate_ipw <- sum((n / sum(n)) * value_estimate_ipw)

  iid <- iid[order(id)]
  id <- id[order(id)]

  out <- list(value_estimate = value_estimate,
              iid = iid,
              pe_dr_cv = pe_dr_cv,
              id = id,
              value_estimate_ipw = value_estimate_ipw,
              folds = folds
  )
  class(out) <- c("policy_eval_cv_dr", "policy_eval")
  return(out)
}

policy_eval_dr <- function(policy_data,
                           policy = NULL, policy_learner = NULL,
                           g_models = NULL, g_functions = NULL, g_full_history,
                           q_models = NULL, q_functions = NULL, q_full_history,
                           verbose = FALSE,
                           ...){

  # fitting the g-functions, Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                          policy = policy, policy_learner = policy_learner,
                          g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                          q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
                          verbose = verbose)

  # calculating the doubly robust score and value estimate:
  if (is.null(policy))
    policy <- get_policy(function_fits$policy_object)
  value_object <- dr_value(policy_data = policy_data,
                    policy = policy,
                    g_functions = function_fits$g_functions,
                    q_functions = function_fits$q_functions)

  out <- list(
    value_estimate = value_object$value_estimate,
    iid=value_object$iid,
    value_estimate_ipw = value_object$value_estimate_ipw,
    value_estimate_or = value_object$value_estimate_or,
    g_functions = function_fits$g_functions,
    q_functions = function_fits$q_functions,
    id = get_id(policy_data),
    policy_object = function_fits$policy_object
  )

  class(out) <- c("policy_eval_dr", "policy_eval")
  return(out)
}

policy_eval_or <- function(policy_data,
                           policy = NULL, policy_learner = NULL,
                           q_models = NULL, q_functions = NULL, q_full_history,
                           verbose = FALSE,
                           ...){

  # fitting the Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learner = policy_learner,
                                 q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
                                 verbose = verbose)

  # calculating the doubly robust score and value estimate:
  if (is.null(policy))
    policy <- get_policy(function_fits$policy_object)
  value_object <- or_value(policy_data = policy_data,
                           policy = policy,
                           q_functions = function_fits$q_functions)

  out <- list(
    value_estimate = value_object$value_estimate,
    iid=value_object$iid,
    q_functions = function_fits$q_functions,
    id = get_id(policy_data),
    policy_object = function_fits$policy_object
  )
  class(out) <- c("policy_eval_or", "policy_eval")
  return(out)
}

policy_eval_ipw <- function(policy_data,
                            policy = NULL, policy_learner = NULL,
                            g_models = NULL, g_functions = NULL, g_full_history,
                            verbose = FALSE,
                            ...){

  # fitting the g-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learner = policy_learner,
                                 g_models = g_models, g_functions = g_functions, g_full_history = g_full_history)

  # calculating the doubly robust score and value estimate:
  if (is.null(policy))
    policy <- get_policy(function_fits$policy_object)
  value_object <- ipw_value(policy_data = policy_data,
                           policy = policy,
                           g_functions = function_fits$g_functions)

  out <- list(
    value_estimate = value_object$value_estimate,
    iid=value_object$iid,
    g_functions = function_fits$g_functions,
    id = get_id(policy_data),
    policy_object = function_fits$policy_object
  )
  class(out) <- c("policy_eval_ipw", "policy_eval")
  return(out)
}
