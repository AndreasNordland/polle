

#' Policy Evaluation
#'
#' \code{policy_eval} is used to estimate the value of a given fixed policy or a data adaptive policy (e.g. a policy learned from the data).
#' @param policy_data Policy data object created by [policy_data()].
#' @param policy Policy object created by [policy_def()].
#' @param policy_learn Policy learner object created by [policy_learn()].
#' @param g_models Propensity models/g-models created by [g_glm()], [g_rf()], [g_sl()] or similar functions. Only used for evaluation if \code{g_functions} is NULL.
#' @param q_models Outcome regression models/Q-models created by [q_glm()], [q_rf()], [q_sl()] or similar functions. Only used for evaluation if \code{q_functions} is NULL.
#' @param g_functions Fitted g-model objects, see [fit_g_functions()]. Preferably, use \code{g_models}.
#' @param q_functions Fitted Q-model objects, see [fit_Q_functions()]. Only valid if the Q-functions are fitted using the same policy. Preferably, use \code{q_models}.
#' @param g_full_history If TRUE, the full history is used to fit each g-model. If FALSE, the state/Markov type history is used to fit each g-model.
#' @param q_full_history Similar to g_full_history.
#' @param M Number of folds for cross-fitting.
#' @param type Type of evaluation (dr/doubly robust, ipw/inverse propensity weighting, or/outcome regression).
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @returns \code{policy_eval()} returns an object of class "policy_eval".
#' The object is a list containing the following elements:
#' \item{\code{value_estimate}}{Numeric. The estimated value of the policy.}
#' \item{\code{type}}{Character string. The type of evaluation ("dr", "ipw", "or").}
#' \item{\code{IC}}{Numeric vector. Estimated influence curve associated with the value estimate.}
#' \item{\code{value_estimate_ipw}}{(only if \code{type = "dr"}) Numeric.
#' The estimated value of the policy based on inverse probability weighting.}
#' \item{\code{value_estimate_or}}{(only if \code{type = "dr"}) Numeric.
#' The estimated value of the policy based on outcome regression.}
#' \item{\code{id}}{Character vector. The IDs of the observations.}
#' \item{\code{policy_actions}}{[data.table] with keys id and stage. Actions
#' associated with the policy for every observation and stage.}
#' \item{\code{policy_object}}{(only if \code{policy = NULL} and \code{M = 1})
#' The policy object returned by \code{policy_learn}, see [policy_learn].}
#' \item{\code{g_functions}}{(only if \code{M = 1}) The
#' fitted g-functions. Object of class "nuisance_functions".}
#' \item{\code{q_functions}}{(only if \code{M = 1}) The
#' fitted Q-functions. Object of class "nuisance_functions".}
#' \item{\code{cross_fits}}{(only if \code{M > 1}) List containing the
#' "policy_eval" object for every (validation) fold.}
#' \item{\code{folds}}{(only if \code{M > 1}) The (validation) folds used
#' for cross-fitting.}
#' @export
#' @examples
#' library("polle")
#' ### Single stage:
#' source(system.file("sim", "single_stage.R", package="polle"))
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#' # defining a static policy:
#' pl1 <- policy_def(static_policy(1))
#' # evaluating the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'             policy = pl1,
#'             g_models = g_glm(),
#'             q_models = q_glm())
#' # summarising the estimated value of the policy:
#' pe1
#'
#' ### Two stages:
#' source(system.file("sim", "two_stage.R", package="polle"))
#' d2 <- sim_two_stage(5e2, seed=1)
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#' # defining a policy learner based on cross-fitted doubly robust Q-learning:
#' pl2 <- policy_learn(type = "rqvl",
#'                    qv_models = list(q_glm(~C_1), q_glm(~C_1+C_2)),
#'                    full_history = TRUE,
#'                    L = 2) # number of folds for cross-fitting
#' pe2 <- policy_eval(type = "dr",
#'                    policy_data = pd2,
#'                    policy_learn = pl2,
#'                    q_models = q_glm(),
#'                    g_models = g_glm(),
#'                    M = 2) # number of folds for cross-evaluation
#' pe2
#' # getting the influence curve for the value:
#' head(IC(pe2))
policy_eval <- function(policy_data,
                        policy = NULL, policy_learn = NULL,
                        g_functions=NULL, g_models=g_glm(), g_full_history = FALSE,
                        q_functions=NULL, q_models=q_glm(), q_full_history = FALSE,
                        type = "dr",
                        M=1, future_args = list(future.seed = TRUE)
                        ) {
  args <- list(
    policy = policy,
    policy_learn = policy_learn,
    g_functions = g_functions,
    g_models = g_models,
    g_full_history = g_full_history,
    q_functions = q_functions,
    q_models = q_models,
    q_full_history = q_full_history,
    type = type
  )

  if (M > 1){
    val <- policy_eval_cross(args = args,
                                    policy_data = policy_data,
                                    M = M,
                                    future_args = future_args)
  } else {
    args[["train_policy_data"]] <- policy_data
    args[["valid_policy_data"]] <- policy_data
    val <- do.call(what = policy_eval_type, args = args)
  }

  val$name <- attr(policy, "name")
  return(val)
}

policy_eval_type <- function(type,
                             train_policy_data,
                             valid_policy_data,
                             policy, policy_learn,
                             g_models, g_functions, g_full_history,
                             q_models, q_functions, q_full_history){

  type <- tolower(type)
  if (length(type) != 1)
    stop("type must be a character string.")

  if (type %in% c("dr", "aipw")){
    type <- "dr"
  } else if (type %in% c("ipw")){
    type <- "ipw"
  } else if (type %in% c("or", "q")) {
    type <- "or"
  } else{
    stop("type must be either 'dr', 'ipw' or  'or'.")
  }

  # fitting the g-functions, the q-functions and the policy (functions):
  fits <- fit_functions(policy_data = train_policy_data,
                        type = type,
                        policy = policy, policy_learn = policy_learn,
                        g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
                        q_models = q_models, q_functions = q_functions, q_full_history = q_full_history)

  # getting the fitted policy and associated actions:
  if (is.null(policy)){
    policy <- get_policy(getElement(fits, "policy_object"))
  }
  policy_actions <- policy(valid_policy_data)

  # calculating the doubly robust score and value estimate:
  value_object <- value(type = type,
                        policy_data = valid_policy_data,
                        policy = policy,
                        g_functions = getElement(fits, "g_functions"),
                        q_functions = getElement(fits, "q_functions"))
  out <- list(
    value_estimate = getElement(value_object, "value_estimate"),
    type = type,
    IC = getElement(value_object, "IC"),
    value_estimate_ipw = getElement(value_object, "value_estimate_ipw"),
    value_estimate_or = getElement(value_object, "value_estimate_or"),
    id = get_id(valid_policy_data),
    policy_actions = policy_actions,
    policy_object = getElement(fits, "policy_object"),
    g_functions = getElement(fits, "g_functions"),
    q_functions = getElement(fits, "q_functions")
  )
  out <- Filter(Negate(is.null), out)

  class(out) <- c("policy_eval")
  return(out)
}

policy_eval_cross <- function(args,
                              policy_data,
                              M,
                              future_args){
  n <- get_n(policy_data)
  id <- get_id(policy_data)

  # setting up the folds
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))
  folds <- lapply(folds, sort)
  names(folds) <- paste("fold_", 1:M, sep = "")

  cross_args <- append(list(X = folds,
                             FUN = policy_eval_fold,
                             policy_data = policy_data,
                             args = args),
                        future_args)

  # cross fitting the policy evaluation using the folds:
  cross_fits <- do.call(what = future.apply::future_lapply, cross_args)

  # collecting ids:
  id <- unlist(lapply(cross_fits, function(x) getElement(x, "id")), use.names = FALSE)

  # collecting the value estimates:
  n <- unlist(lapply(cross_fits, function(x) length(getElement(x, "id"))))
  value_estimate <- unlist(lapply(cross_fits, function(x) getElement(x, "value_estimate")))
  value_estimate <- sum((n / sum(n)) * value_estimate)

  # collecting the IC decompositions:
  IC <- unlist(lapply(cross_fits, function(x) getElement(x, "IC")), use.names = FALSE)

  # collecting the IPW value estimates (only if type == "dr")
  value_estimate_ipw <- unlist(lapply(cross_fits, function(x) getElement(x, "value_estimate_ipw")))
  if (!is.null(value_estimate_ipw)){
    value_estimate_ipw <- sum((n / sum(n)) * value_estimate_ipw)
  }

  # collecting the OR value estimates (only if type = "dr")
  value_estimate_or <- unlist(lapply(cross_fits, function(x) getElement(x, "value_estimate_or")))
  if (!is.null(value_estimate_or)){
    value_estimate_or <- sum((n / sum(n)) * value_estimate_or)
  }

  # collecting the policy actions
  policy_actions <- lapply(cross_fits, function(x) getElement(x, "policy_actions"))
  policy_actions <- rbindlist(policy_actions)
  setkey(policy_actions, "id", "stage")

  # sorting via the IDs:
  IC <- IC[order(id)]
  id <- id[order(id)]

  out <- list(value_estimate = value_estimate,
              type = getElement(args, "type"),
              IC = IC,
              value_estimate_ipw = value_estimate_ipw,
              value_estimate_or = value_estimate_or,
              id = id,
              policy_actions = policy_actions,
              cross_fits = cross_fits,
              folds = folds
  )

  out <- Filter(Negate(is.null), out)

  class(out) <- c("policy_eval")
  return(out)
}

policy_eval_fold <- function(fold,
                             policy_data,
                             args
){

  K <- get_K(policy_data)
  id <- get_id(policy_data)

  train_id <- id[-fold]
  validation_id <- id[fold]

  # training data:
  train_policy_data <- subset(policy_data, train_id)
  if (get_K(train_policy_data) != K) stop("The number of stages varies accross the training folds.")

  # validation data:
  valid_policy_data <- subset(policy_data, validation_id)
  if (get_K(valid_policy_data) != K) stop("The number of stages varies accross the validation folds.")

  eval_args <- append(args, list(valid_policy_data = valid_policy_data,
                                 train_policy_data = train_policy_data))

  out <- do.call(what = "policy_eval_type", args = eval_args)

  return(out)
}

#' @rdname policy_eval
#' @export
coef.policy_eval <- function(object, ...) {
  return(object$value_estimate)
}


#' @rdname policy_eval
#' @export
IC.policy_eval <- function(x, ...) {
  res <- cbind(getElement(x, "IC"))
  return(res)
}

#' @rdname policy_eval
#' @export
vcov.policy_eval <- function(object, ...) {
  return(crossprod(IC(object)))
}

#' @export
print.policy_eval <- function(x, ...) {
  print(summary(x, ...))
}


#' @rdname policy_eval
#' @export
summary.policy_eval <- function(object, ...) {
  lava::estimate(object, ...)
}

#' @rdname policy_eval
#' @export
estimate.policy_eval <- function(x, ..., labels=x$name) {
  p <- length(coef(x))
  if (is.null(labels)) {
    if (p==1) {
      "value"
    } else {
      labels <- paste0("value", seq(p))
    }
  }
  return(lava::estimate(NULL, coef=coef(x), IC=IC(x), labels=labels, ...))
}

#' @export
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

#' @export
"+.policy_eval" <- function(x,...) {
  merge(x, ...)
}
