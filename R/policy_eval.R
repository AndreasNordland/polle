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
##' @param policy_learn Policy learner object
##' @param g_models Propensity model/g-model
##' @param q_models Outcome regression/Q-model
##' @param g_functions Fitted g-model object
##' @param q_functions Fitted Q-model object
##' @param g_full_history g-model: Full history or single stage/Markov history
##' @param q_full_history Q-model: Full history or single stage/Markov history
##' @param cross_fit Should the evaluation be cross-fitted or not
##' @param M Number of folds
##' @param type Type of evaluation (dr/doubly robust, ipw/inverse propensity weighting, or/outcome regression)
policy_eval <- function(policy_data,
                        policy = NULL, policy_learn = NULL,
                        g_functions=NULL, g_models=g_glm(), g_full_history = FALSE,
                        q_functions=NULL, q_models=q_glm(), q_full_history = FALSE,
                        cross_fit = FALSE, M=5, future_args = NULL,
                        type = "dr"
                        ) {
  args <- list(
    policy_data = policy_data,
    policy = policy,
    policy_learn = policy_learn,
    g_functions = g_functions,
    g_models = g_models,
    g_full_history = g_full_history,
    q_functions = q_functions,
    q_models = q_models,
    q_full_history = q_full_history
  )

  call <- NULL
  if (type %in% c("dr")){
    call <- "policy_eval_dr"
  }
  if (type %in% c("ipw")){
    call <- "policy_eval_ipw"
  }
  if (type %in% c("or", "q")){
    call <- "policy_eval_or"
  }
  if (is.null(call)){
    mes <- "type must be either 'dr', 'ipw' or 'or'"
    stop(mes)
  }

  if (cross_fit == FALSE){
      val <- do.call(what = call, args = args)
  }
  if (cross_fit == TRUE){
    val <- policy_eval_cross_fitted(call = call,
                                    args = args,
                                    M = M,
                                    future_args = future_args)
  }

  val$name <- attr(policy, "name")
  return(val)
}

policy_eval_fold <- function(fold,
                             call,
                             policy_data,
                             policy, policy_learn,
                             g_models, g_functions, g_full_history,
                             q_models, q_functions, q_full_history
){
  K <- get_K(policy_data)
  id <- get_id(policy_data)
  train_id <- id[-fold]
  validation_id <- id[fold]

  # training data:
  train_policy_data <- subset(policy_data, train_id)
  if (train_policy_data$dim$K != K) stop("The number of stages varies accross the training folds.")
  # validation data:
  validation_policy_data <- subset(policy_data, validation_id)
  if (validation_policy_data$dim$K != K) stop("The number of stages varies accross the validation folds.")

  # training the policy evaluation models:
  train_args <- list(policy_data = train_policy_data,
                     policy = policy,
                     policy_learn = policy_learn,
                     g_models = g_models,
                     g_functions = g_functions,
                     g_full_history = g_full_history,
                     q_models = q_models,
                     q_functions = q_functions,
                     q_full_history = q_full_history)
  train_policy_eval <- do.call(what = call, args = train_args)

  # getting the policy:
  if (is.null(policy)){
    policy <- get_policy(train_policy_eval$policy_object)
  }

  # validating the policy evaluation models:
  validation_args <- list(policy_data = validation_policy_data,
                          policy = policy,
                          g_functions = train_policy_eval$g_functions,
                          q_functions = train_policy_eval$q_functions)
  validation_policy_eval <- do.call(what = call, args = validation_args)

  # saving the fitted policy objects (if available):
  if (!is.null(train_policy_eval$policy_object)){
    validation_policy_eval$policy_object <- train_policy_eval$policy_object
  }

  return(validation_policy_eval)
}

# policy_eval_dr_fold <- function(fold,
#                                 policy_data,
#                                 policy, policy_learn,
#                                 g_models, g_functions, g_full_history,
#                                 q_models, q_functions, q_full_history
#                                 ){
#
#   K <- get_K(policy_data)
#   id <- get_id(policy_data)
#   train_id <- id[-fold]
#   validation_id <- id[fold]
#
#   train_policy_data <- subset(policy_data, train_id)
#   if (train_policy_data$dim$K != K) stop("The number of stages varies accross the training folds.")
#   validation_policy_data <- subset(policy_data, validation_id)
#   if (validation_policy_data$dim$K != K) stop("The number of stages varies accross the validation folds.")
#
#   train_args <- list(policy_data = train_policy_data,
#                      policy = policy, policy_learn = policy_learn,
#                      g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
#                      q_models = q_models, q_functions = q_functions, q_full_history = q_full_history)
#   # train_args <- append(train_args, dotdotdot)
#   train_pe_dr <- do.call(what = "policy_eval_dr", args = train_args)
#
#   # getting the policy:
#   if (is.null(policy)){
#     policy <- get_policy(train_pe_dr$policy_object)
#   }
#
#   validation_args <- list(policy_data = validation_policy_data,
#                           policy = policy,
#                           g_functions = train_pe_dr$g_functions,
#                           q_functions = train_pe_dr$q_functions)
#   validation_pe_dr <- do.call(what = "policy_eval_dr", args = validation_args)
#
#   if (!is.null(train_pe_dr$policy_object)){
#     validation_pe_dr$policy_object <- train_pe_dr$policy_object
#   }
#
#   return(validation_pe_dr)
# }

policy_eval_cross_fitted <- function(call,
                                     args,
                                     M,
                                     future_args){
  policy_data <- args$policy_data

  n <- get_n(policy_data)
  id <- get_id(policy_data)

  # setting up the folds
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))
  names(folds) <- paste("fold_", 1:M, sep = "")

  future_args <- append(future_args,
                        list(X = folds,
                             FUN = policy_eval_fold))
  future_args <- append(future_args, list(call = call))
  future_args <- append(future_args, args)

  cross_fit <- do.call(what = future.apply::future_lapply, future_args)

  # collecting IDs:
  id <- unlist(lapply(cross_fit, function(x) x$id), use.names = FALSE)

  # collecting the value estimate:
  n <- unlist(lapply(cross_fit, function(x) length(x$id)))
  value_estimate <- unlist(lapply(cross_fit, function(x) x$value_estimate))
  value_estimate <- sum((n / sum(n)) * value_estimate)

  # collecting the IID decomposition:
  iid <- unlist(lapply(cross_fit, function(x) x$iid), use.names = FALSE)

  value_estimate_ipw <- unlist(lapply(cross_fit, function(x) x$value_estimate_ipw))
  if (!is.null(value_estimate_ipw)){
    value_estimate_ipw <- sum((n / sum(n)) * value_estimate_ipw)
  }

  value_estimate_or <- unlist(lapply(cross_fit, function(x) x$value_estimate_or))
  if (!is.null(value_estimate_or)){
    value_estimate_or <- sum((n / sum(n)) * value_estimate_or)
  }

  # sorting via the IDs:
  iid <- iid[order(id)]
  id <- id[order(id)]

  out <- list(value_estimate = value_estimate,
              iid = iid,
              value_estimate_ipw = value_estimate_ipw,
              value_estimate_or = value_estimate_or,
              id = id,
              cross_fit = cross_fit,
              folds = folds
  )
  out[sapply(out, is.null)] <- NULL

  class(out) <- c("policy_eval_cross_fitted", "policy_eval")
  return(out)
}

# policy_eval_cv_dr <- function(policy_data,
#                               policy = NULL, policy_learn = NULL,
#                               g_models = NULL, g_functions = NULL, g_full_history,
#                               q_models = NULL, q_functions = NULL, q_full_history,
#                               M,
#                               future_args = NULL,
#                               ...){
#
#   n <- get_n(policy_data)
#   id <- get_id(policy_data)
#
#   # setting up the folds
#   folds <- split(sample(1:n, n), rep(1:M, length.out = n))
#
#   # # setting up the folds
#   # if (!is.null(seed)){
#   #   withr::with_seed(seed, {
#   #     folds <- split(sample(1:n, n), rep(1:M, length.out = n))
#   #   })
#   # } else{
#   #   withr::with_preserve_seed({
#   #     folds <- split(sample(1:n, n), rep(1:M, length.out = n))
#   #   })
#   # }
#
#   # dotdotdot <- list(...)
#
#   future_args <- append(future_args, list(
#     X = folds,
#     FUN = policy_eval_dr_fold,
#     policy_data = policy_data,
#     policy = policy, policy_learn = policy_learn,
#     g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
#     q_models = q_models, q_functions = q_functions, q_full_history = q_full_history
#   ))
#   force(future_args)
#
#   pe_dr_cv <- do.call(what = future.apply::future_lapply, future_args)
#
#   id <- unlist(lapply(pe_dr_cv, function(x) x$id))
#   iid <- unlist(lapply(pe_dr_cv, function(x) x$iid))
#
#   n <- unlist(lapply(pe_dr_cv, function(x) length(x$id)))
#   value_estimate <- unlist(lapply(pe_dr_cv, function(x) x$value_estimate))
#   value_estimate <- sum((n / sum(n)) * value_estimate)
#
#   value_estimate_ipw <- unlist(lapply(pe_dr_cv, function(x) x$value_estimate_ipw))
#   value_estimate_ipw <- sum((n / sum(n)) * value_estimate_ipw)
#
#   iid <- iid[order(id)]
#   id <- id[order(id)]
#
#   out <- list(value_estimate = value_estimate,
#               iid = iid,
#               pe_dr_cv = pe_dr_cv,
#               id = id,
#               value_estimate_ipw = value_estimate_ipw,
#               folds = folds
#   )
#   class(out) <- c("policy_eval_cv_dr", "policy_eval")
#   return(out)
# }

policy_eval_dr <- function(policy_data,
                           policy = NULL, policy_learn = NULL,
                           g_models = NULL, g_functions = NULL, g_full_history,
                           q_models = NULL, q_functions = NULL, q_full_history,
                           ...){

  # fitting the g-functions, Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learn = policy_learn,
                                 g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                                 q_models = q_models, q_functions = q_functions, q_full_history = q_full_history)

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
                           policy = NULL, policy_learn = NULL,
                           q_models = NULL, q_functions = NULL, q_full_history,
                           ...){

  # fitting the Q-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learn = policy_learn,
                                 q_models = q_models, q_functions = q_functions, q_full_history = q_full_history)

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
                            policy = NULL, policy_learn = NULL,
                            g_models = NULL, g_functions = NULL, g_full_history,
                            ...){

  # fitting the g-functions and policy (functions):
  function_fits <- fit_functions(policy_data = policy_data,
                                 policy = policy, policy_learn = policy_learn,
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
