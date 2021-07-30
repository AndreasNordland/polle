##' @export
coef.policy_eval <- function(object, ...) {
  return(object$value_estimate)
}

##' @export
iid.policy_eval <- function(x, ...) {
  res <- cbind(x$iid)
  return(res/nrow(res))
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
  estimate(object, ...)
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
  return(estimate(NULL, coef=coef(x), iid=iid(x), labels=labels, ...))
}

##' @export
"merge.policy_eval" <- function(x, y, ..., paired=TRUE) {
  dots <- list(...)
  idx <- names(dots)%in%formalArgs(estimate.default)[-1]
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

##' @export
static_policy <- function(action, name=paste0("a=",action)) {
  f <- function(history) {
    pol <- history$H
    pol[, d := action]
    return(pol[, c("id", "stage", "d"), with = FALSE])
  }
  return(structure(f, name=name))
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
                        g_functions=NULL, g_models=NULL, g_full_history = FALSE,
                        q_functions=NULL, q_models=NULL, q_full_history = FALSE,
                        M=5, type="dr", ...) {
  type <- tolower(type)

  cl_args <- c(as.list(environment()), list(...))

  if (type%in%c("cv", "crossfit", "cf", "cv_dr")) {
    val <- do.call(what = "policy_eval_cv_dr", cl_args)
  }
  if (type%in%c("dr")) {
    val <- do.call(what = "policy_eval_dr", cl_args)
  }
  if (type%in%c("or","q")) {
    val <- do.call(what = "policy_eval_or", cl_args)
  }
  if (type%in%c("ipw")) {
    val <- do.call(what = "policy_eval_ipw", cl_args)
  }
  val$name <- attr(policy, "name")
  return(val)
}


policy_eval_dr_fold <- function(fold,
                                policy_data,
                                policy, policy_learner,
                                g_models, g_functions, g_full_history,
                                q_models, q_functions, q_full_history,
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
                     q_models = q_models, q_functions = q_functions, q_full_history = q_full_history)
  train_args <- append(train_args, dotdotdot)
  train_pe_dr <- do.call(what = "policy_eval_dr", args = train_args)

  # getting the policy:
  if (is.null(policy))
    policy <- get_policy(train_pe_dr$policy_object)

  validation_args <- list(policy_data = validation_policy_data,
                          policy = policy,
                          g_functions = train_pe_dr$g_functions,
                          q_functions = train_pe_dr$q_functions)
  validation_pe_dr <- do.call(what = "policy_eval_dr", args = validation_args)

  return(validation_pe_dr)
}

policy_eval_cv_dr <- function(policy_data,
                              policy = NULL, policy_learner = NULL,
                              g_models = NULL, g_functions = NULL, g_full_history,
                              q_models = NULL, q_functions = NULL, q_full_history,
                              M, ...){

  n <- get_n(policy_data)
  id <- get_id(policy_data)

  # setting up the folds
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))

  dotdotdot <- list(...)
  pe_dr_cv <- lapply(
    folds,
    FUN = policy_eval_dr_fold,
    policy_data = policy_data,
    policy = policy, policy_learner = policy_learner,
    g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
    q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
    dotdotdot = dotdotdot
  )

  # dr_list <- parallel::mclapply(
  #   folds,
  #   FUN = dr_fold,
  #   ...,
  #   id = id,
  #   policy_data = policy_data,
  #   policy = policy,
  #   g_models = g_models,
  #   q_models = q_models,
  #   g_full_history = g_full_history,
  #   q_full_history = q_full_history
  #   )
  #

  id <- unlist(lapply(pe_dr_cv, function(x) x$id))
  iid <- unlist(lapply(pe_dr_cv, function(x) x$iid))

  n <- unlist(lapply(pe_dr_cv, function(x) length(x$id)))
  value_estimate <- unlist(lapply(pe_dr_cv, function(x) x$value_estimate))
  value_estimate <- sum((n / sum(n)) * value_estimate)

  iid <- iid[order(id)]
  id <- id[order(id)]

  out <- list(value_estimate = value_estimate,
              iid = iid,
              pe_dr_cv = pe_dr_cv,
              id = id
  )
  class(out) <- c("policy_eval_cv_dr", "policy_eval")
  return(out)
}

policy_eval_dr <- function(policy_data,
                           policy = NULL, policy_learner = NULL,
                           g_models = NULL, g_functions = NULL, g_full_history,
                           q_models = NULL, q_functions = NULL, q_full_history, ...){

  # fitting the g-functions, Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                          policy = policy, policy_learner = policy_learner,
                          g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                          q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
                          ...)


  # calculating the doubly robust score and value estimate:
  if (is.null(policy))
    policy <- get_policy(function_fits$policy_object)
  value_object <- dr_value(policy_data = policy_data,
                    policy = policy,
                    g_functions = function_fits$g_functions,
                    q_function = function_fits$q_functions)

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
                           ...){

  # fitting the Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learner = policy_learner,
                                 q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
                                 ...)

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
                            ...){

  # fitting the g-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learner = policy_learner,
                                 g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                                 ...)

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
