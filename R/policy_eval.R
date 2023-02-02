#' Policy Evaluation
#'
#' \code{policy_eval()} is used to estimate the value of a given fixed policy
#' or a data adaptive policy (e.g. a policy learned from the data).
#' @param policy_data Policy data object created by [policy_data()].
#' @param policy Policy object created by [policy_def()].
#' @param policy_learn Policy learner object created by [policy_learn()].
#' @param g_models List of action probability models/g-models for each stage
#' created by [g_empir()], [g_glm()], [g_rf()], [g_sl()] or similar functions.
#' Only used for evaluation if \code{g_functions} is \code{NULL}.
#' If a single model is provided and \code{g_full_history} is \code{FALSE},
#' a single g-model is fitted across all stages. If \code{g_full_history} is
#' \code{TRUE} the model is reused at every stage.
#' @param q_models Outcome regression models/Q-models created by
#' [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' Only used for evaluation if \code{q_functions} is \code{NULL}.
#' If a single model is provided, the model is reused at every stage.
#' @param g_functions Fitted g-model objects, see [nuisance_functions].
#' Preferably, use \code{g_models}.
#' @param q_functions Fitted Q-model objects, see [nuisance_functions].
#' Only valid if the Q-functions are fitted using the same policy.
#' Preferably, use \code{q_models}.
#' @param g_full_history If TRUE, the full history is used to fit each g-model.
#' If FALSE, the state/Markov type history is used to fit each g-model.
#' @param q_full_history Similar to g_full_history.
#' @param M Number of folds for cross-fitting.
#' @param type Type of evaluation (dr/doubly robust, ipw/inverse propensity
#' weighting, or/outcome regression).
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @param name Character string.
#' @param object,x,y Objects of class "policy_eval".
#' @param labels Name(s) of the estimate(s).
#' @param paired \code{TRUE} indicates that the estimates are based on
#' the same data sample.
#' @param ... Additional arguments.
#' @returns \code{policy_eval()} returns an object of class "policy_eval".
#' The object is a list containing the following elements:
#' \item{\code{value_estimate}}{Numeric. The estimated value of the policy.}
#' \item{\code{type}}{Character string. The type of evaluation ("dr", "ipw",
#' "or").}
#' \item{\code{IC}}{Numeric vector. Estimated influence curve associated with
#' the value estimate.}
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
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class \code{policy_eval}:
#' \itemize{
#' \item{[get_g_functions()]}{ Extract the fitted g-functions.}
#' \item{[get_q_functions()]}{ Extract the fitted Q-functions.}
#' \item{[get_policy()]}{ Extract the fitted policy object.}
#' \item{[get_policy_functions()]}{ Extract the fitted policy function for
#'                                 a given stage.}
#' \item{[get_policy_actions()]}{ Extract the (fitted) policy actions.}
#' \item{[plot.policy_eval()]}{Plot diagnostics.}
#' }
#' @seealso [lava::IC], [lava::estimate.default].
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item{} \eqn{B} is a vector of baseline covariates.
#'  \item{} \eqn{U_k} is the reward at stage k (not influenced by the action \eqn{A_k}).
#'  \item{} \eqn{X_k} is a vector of state covariates summarizing the state at stage k.
#'  \item{} \eqn{A_k} is the categorical action within the action set \eqn{\mathcal{A}} at stage k.
#' }
#' The utility is given by the sum of the rewards, i.e.,
#' \eqn{U = \sum_{k = 1}^{K+1} U_k}.
#'
#' A policy is a set of functions
#' \deqn{d = \{d_1, ..., d_K\},}
#' where \eqn{d_k} for \eqn{k\in \{1, ..., K\}} maps \eqn{\{B, X_1, A_1, ..., A_{k-1}, X_k\}} into the
#' action set.
#'
#' Recursively define the Q-models (\code{q_models}):
#' \deqn{Q^d_K(h_K, a_K) = E[U|H_K = h_K, A_K = a_K]}
#' \deqn{Q^d_k(h_k, a_k) = E[Q_{k+1}(H_{k+1}, d_{k+1}(B,X_1, A_1,...,X_{k+1}))|H_k = h_k, A_k = a_k].}
#' If \code{q_full_history = TRUE},
#' \eqn{H_k = \{B, X_1, A_1, ..., A_{k-1}, X_k\}}, and if
#' \code{q_full_history = FALSE}, \eqn{H_k = \{B, X_k\}}.
#'
#' The g-models (\code{g_models}) are defined as
#' \deqn{g_k(h_k, a_k) = P(A_k = a_k|H_k = h_k).}
#' If \code{g_full_history = TRUE},
#' \eqn{H_k = \{B, X_1, A_1, ..., A_{k-1}, X_k\}}, and if
#' \code{g_full_history = FALSE}, \eqn{H_k = \{B, X_k\}}.
#' Furthermore, if \code{g_full_history = FALSE} and \code{g_models} is a
#' single model, it is assumed that \eqn{g_1(h_1, a_1) = ... = g_K(h_K, a_K)}.
#'
#' If \code{type = "or"} \code{policy_eval} returns the empirical estimates of
#' the value (\code{value_estimate}) and score (\code{IC}):
#' \deqn{E[Q^d_1(H_1, d_1(...))]}
#' \deqn{Q^d_1(H_1, d_1(...)) - E[Q^d_1(H_1, d_1(...))]}
#' for an appropriate input \eqn{...} to the policy.
#'
#' If \code{type = "ipw"} \code{policy_eval} returns the empirical estimates of
#' the value (\code{value_estimate}) and score (\code{IC}):
#' \deqn{E[(\prod_{k=1}^K I\{A_k = d_k(...)\} g_k(H_k, A_k)^{-1}) U].}
#' \deqn{(\prod_{k=1}^K I\{A_k = d_k(...)\} g_k(H_k, A_k)^{-1}) U - E[(\prod_{k=1}^K I\{A_k = d_k(...)\} g_k(H_k, A_k)^{-1}) U].}
#'
#' If \code{type = "dr"} \code{policy_eval} returns the empirical estimates of
#' the value (\code{value_estimate}) and influence curve (\code{IC}):
#' \deqn{E[Z^d_1],}
#' \deqn{Z^d_1 - E[Z^d_1],}
#' where
#' \deqn{
#' Z^d_1 = Q^d_1(H_1 , d_1(...)) + \sum_{r = 1}^K \prod_{j = 1}^{r}
#' \frac{I\{A_j = d_j(...)\}}{g_{j}(H_j, A_j)}
#' \{Q_{r+1}^d(H_{r+1} , d_{r+1}(...)) - Q_{r}^d(H_r , d_r(...))\}.
#' }
#' @references
#' van der Laan, Mark J., and Alexander R. Luedtke. "Targeted learning of the
#' mean outcome under an optimal dynamic treatment rule." Journal of causal
#' inference 3.1 (2015): 61-95. \doi{10.1515/jci-2013-0022}\cr
#' \cr
#' Tsiatis, Anastasios A., et al. Dynamic treatment regimes: Statistical methods
#'for precision medicine. Chapman and Hall/CRC, 2019. \doi{10.1201/9780429192692}.
#' @export
#' @examples
#' library("polle")
#' ### Single stage:
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#' # defining a static policy (A=1):
#' pl1 <- policy_def(1)
#'
#' # evaluating the policy:
#' pe1 <- policy_eval(policy_data = pd1,
#'                    policy = pl1,
#'                    g_models = g_glm(),
#'                    q_models = q_glm(),
#'                    name = "A=1 (glm)")
#'
#' # summarizing the estimated value of the policy:
#' # (equivalent to summary(pe1)):
#' pe1
#' coef(pe1) # value coefficient
#' sqrt(vcov(pe1)) # value standard error
#'
#' # getting the g-function and Q-function values:
#' head(predict(get_g_functions(pe1), pd1))
#' head(predict(get_q_functions(pe1), pd1))
#'
#' # getting the fitted influence curve (IC) for the value:
#' head(IC(pe1))
#'
#' # evaluating the policy using random forest nuisance models:
#' set.seed(1)
#' pe1_rf <- policy_eval(policy_data = pd1,
#'                       policy = pl1,
#'                       g_models = g_rf(),
#'                       q_models = q_rf(),
#'                       name = "A=1 (rf)")
#'
#' # merging the two estimates (equivalent to pe1 + pe1_rf):
#' (est1 <- merge(pe1, pe1_rf))
#' coef(est1)
#' head(IC(est1))
#'
#' ### Two stages:
#' d2 <- sim_two_stage(5e2, seed=1)
#' pd2 <- policy_data(d2,
#'                    action = c("A_1", "A_2"),
#'                    covariates = list(L = c("L_1", "L_2"),
#'                                      C = c("C_1", "C_2")),
#'                    utility = c("U_1", "U_2", "U_3"))
#' pd2
#'
#' # defining a policy learner based on cross-fitted doubly robust Q-learning:
#' pl2 <- policy_learn(type = "drql",
#'                     control = control_drql(qv_models = list(q_glm(~C_1),
#'                                                             q_glm(~C_1+C_2))),
#'                     full_history = TRUE,
#'                     L = 2) # number of folds for cross-fitting
#'
#' # evaluating the policy learner using 2-fold cross fitting:
#' pe2 <- policy_eval(type = "dr",
#'                    policy_data = pd2,
#'                    policy_learn = pl2,
#'                    q_models = q_glm(),
#'                    g_models = g_glm(),
#'                    M = 2, # number of folds for cross-fitting
#'                    name = "drql")
#' # summarizing the estimated value of the policy:
#' pe2
#'
#' # getting the cross-fitted policy actions:
#' head(get_policy_actions(pe2))
policy_eval <- function(policy_data,
                        policy = NULL, policy_learn = NULL,
                        g_functions = NULL, g_models = g_glm(), g_full_history = FALSE,
                        q_functions = NULL, q_models = q_glm(), q_full_history = FALSE,
                        type = "dr",
                        M = 1, future_args = list(future.seed=TRUE),
                        name = NULL
                        ) {
  args <- as.list(environment())
  args[["policy_data"]] <- NULL
  args[["M"]] <- NULL
  args[["future_args"]] <- NULL
  args[["name"]] <- NULL

  # input checks:
  if (!inherits(policy_data, what = "policy_data"))
    stop("policy_data must be of inherited class 'policy_data'.")
  if (!is.null(policy)){
    if (!inherits(policy, what = "policy"))
      stop("policy must be of inherited class 'policy'.")
  }
  if ((is.null(policy) & is.null(policy_learn)) |
      (!is.null(policy_learn) & !is.null(policy)))
    stop("Provide either policy or policy_learn.")
  if (is.null(policy) & !is.null(policy_learn)){
    if (!inherits(policy_learn, what = "policy_learn"))
      stop("policy_learn must be of inherited class 'policy_learn'.")
  }
  if (!is.null(g_functions)){
    if(!(inherits(g_functions, "g_functions")))
      stop("g_functions must be of class 'g_functions'.")
  }
  if (!(is.logical(g_full_history) & (length(g_full_history) == 1)))
    stop("g_full_history must be TRUE or FALSE")
  if (!is.null(q_functions)){
    if(!(inherits(q_functions, "q_functions")))
      stop("q-functions must be of class 'q_functions'.")
  }
  if (!(is.logical(q_full_history) & (length(q_full_history) == 1)))
    stop("q_full_history must be TRUE or FALSE")
  if (!(is.numeric(M) & (length(M) == 1)))
    stop("M must be an integer greater than 0.")
  if (!(M %% 1 == 0))
    stop("M must be an integer greater than 0.")
  if (M<=0)
    stop("M must be an integer greater than 0.")
  if (!is.list(future_args))
    stop("future_args must be a list.")
  if (!is.null(name)){
    name <- as.character(name)
    if (length(name) != 1)
      stop("name must be a character string.")
  }


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
  if (is.null(name)){
    if (!is.null(policy))
      val$name <- attr(policy, "name")
    else
      val$name <- attr(policy_learn, "name")
  } else
    val$name <- name

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

  # checking that the (fitted) policy actions comply with the stage action sets:
  check_actions(actions = policy_actions,
                policy_data = train_policy_data)

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

  prog <- progressor(along = folds)
  cross_args <- append(list(X = folds,
                            FUN = policy_eval_fold,
                            policy_data = policy_data,
                            args = args,
                            prog = prog),
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
                             args,
                             prog
){

  K <- get_K(policy_data)
  id <- get_id(policy_data)

  train_id <- id[-fold]
  validation_id <- id[fold]

  # training data:
  train_policy_data <- subset_id(policy_data, train_id)
  if (get_K(train_policy_data) != K) stop("The number of stages varies accross the training folds.")

  # validation data:
  valid_policy_data <- subset_id(policy_data, validation_id)
  if (get_K(valid_policy_data) != K) stop("The number of stages varies accross the validation folds.")

  eval_args <- append(args, list(valid_policy_data = valid_policy_data,
                                 train_policy_data = train_policy_data))

  out <- do.call(what = "policy_eval_type", args = eval_args)

  # progress:
  prog()

  return(out)
}

check_actions <- function(actions, policy_data){
  # checking the format of the actions data.table
  if (!is.data.table(actions))
    stop("actions must be a data.table.")
  if (!any("d" %in% colnames(actions)))
    stop("actions must have an action varible named 'd'.")
  if (!all(key(actions) == c("id", "stage")))
    stop("actions must have keys 'id' and 'stage'.")

  # checking that the actions comply with the stage action sets
  K <- get_K(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)
  stage <- NULL
  for(k_ in 1:K){
    if (!all(unlist(actions[stage == k_, "d"]) %in% stage_action_sets[[k_]])){
      mes <- "The policy actions does not comply with the stage action sets of the policy data object."
      stop(mes)
    }
  }
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
  ic <- IC(object)
  if (is.null(ic))
    return(NULL)
  n <- nrow(ic)
  return(crossprod(ic)/(n*n))
}

#' @rdname policy_eval
#' @export
print.policy_eval <- function(x, ...) {
  print(summary(x, ...))
}


#' @rdname policy_eval
#' @export
summary.policy_eval <- function(object, ...){
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
  ic <- IC(x)
  if (is.null(ic)){
    est <- lava::estimate(NULL, coef=coef(x), vcov=NULL, ...) # labels=labels
  }
  else
    est <- lava::estimate(NULL, coef=coef(x), IC=ic, labels=labels, ...)
  return(est)
}

#' @rdname policy_eval
#' @export
"merge.policy_eval" <- function(x, y, ..., paired = TRUE) {
  dots <- list(...)
  idx <- names(dots) %in% formalArgs(lava::estimate.default)[-1]
  est_args <- list()
  if (length(idx)>0) {
    est_args <- dots[which(idx)]
    dots <- dots[-which(idx)]
  }
  m <- lapply(c(list(x, y), dots), function(p)
    do.call(estimate, c(list(p),est_args)))
  m <- do.call("merge", c(m, list(paired=paired)))
  return(m)
}

#' @rdname policy_eval
#' @export
"+.policy_eval" <- function(x,...) {
  merge(x, ...)
}

#' @title Conditional Policy Evaluation
#' @description \code{conditional()} is used to calculate the
#' policy value for each group defined by a given baseline variable.
#' @param object Policy evaluation object created by [policy_eval()].
#' @param policy_data Policy data object created by [policy_data()].
#' @param baseline Character string.
#' @returns object of inherited class 'estimate', see [lava::estimate.default].
#' The object is a list with elements 'coef' (policy value estimate for each
#' group) and 'IC' (influence curve estimate matrix).
#' @examples
#' library("polle")
#' d <- sim_single_stage(n=2e3)
#' pd <- policy_data(d,
#'                   action = "A",
#'                   baseline = c("B"),
#'                   covariates = c("Z","L"),
#'                   utility = "U")
#'
#' # static policy:
#' p <- policy_def(1)
#'
#' pe <- policy_eval(pd,
#'                   policy = p)
#' pe
#'
#' # conditional value for each group defined by B
#' conditional(pe, pd, "B")
#' @export
conditional <- function(object, policy_data, baseline)
  UseMethod("conditional")

#' @export
conditional.policy_eval <- function(object, policy_data, baseline){
  policy_eval <- object

  if (!inherits(policy_eval, "policy_eval"))
    stop("policy_eval must be of inherited class 'policy_eval'.")
  if (!inherits(policy_data, "policy_data"))
    stop("policy_data must be of inherited class 'policy_data'.")
  if (!is.character(baseline) | length(baseline)!= 1)
    stop("baseline must be a single character.")

  baseline_data <- policy_data[["baseline_data"]]

  # checking IDs
  check <- all.equal(
    policy_eval[["id"]],
    baseline_data[["id"]]
  )
  if (!check)
    stop("ID's does not match.")

  # getting the doubly robust score:
  z <- IC(policy_eval) + coef(policy_eval)

  by <- baseline_data[, baseline, with = FALSE]
  agg <- aggregate(z, by = by, mean)
  coef <- agg[["V1"]]

  groups <- agg[[baseline]]
  IC <- matrix(0, nrow = nrow(baseline_data), ncol = length(groups))
  for (j in seq_along(coef)){
    idx <- baseline_data[[baseline]] == groups[j]
    id <- baseline_data[["id"]][idx]
    ic <- z[idx,] - coef[j]
    IC[idx,j] <- ic
  }
  est <- estimate(NULL,
                  coef = coef,
                  IC = cbind(IC),
                  id = baseline_data[["id"]],
                  labels = paste(baseline, groups, sep = ":"))
  return(est)
}

#' @export
get_g_functions.policy_eval <- function(object){
  getElement(object, "g_functions")
}

#' @export
get_q_functions.policy_eval <- function(object){
  getElement(object, "q_functions")
}

#' @export
get_policy.policy_eval <- function(object){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy(po)
  return(pf)
}

#' @title Get Policy Actions
#'
#' @description \code{get_policy_actions()} extract the actions dictated by the
#' (learned and possibly cross-fitted) policy a every stage.
#' @param object Object of class [policy_eval].
#' @returns [data.table] with keys \code{id} and \code{stage} and action variable
#' \code{d}.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # defining a policy learner based on cross-fitted doubly robust Q-learning:
#' pl <- policy_learn(type = "drql",
#'                    control = control_drql(qv_models = list(q_glm(~C_1), q_glm(~C_1+C_2))),
#'                    full_history = TRUE,
#'                    L = 2) # number of folds for cross-fitting
#'
#' # evaluating the policy learner using 2-fold cross fitting:
#' pe <- policy_eval(type = "dr",
#'                    policy_data = pd,
#'                    policy_learn = pl,
#'                    q_models = q_glm(),
#'                    g_models = g_glm(),
#'                    M = 2) # number of folds for cross-fitting
#'
#' # Getting the cross-fitted actions dictated by the fitted policy:
#' head(get_policy_actions(pe))
#' @export
get_policy_actions <- function(object)
  UseMethod("get_policy_actions")

#' @export
get_policy_actions.policy_eval <- function(object){
  getElement(object, "policy_actions")
}

#' @export
get_policy_functions.policy_eval <- function(object, stage){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy_functions(po, stage = stage)
  return(pf)
}

#' @title Plot Diagnostics for a \code{policy_eval} Object
#'
#' @param x Object of class [policy_eval]
#' @param which A subset of the numbers 1:2
#' \itemize{
#'  \item{1} Histogram of the influence curve terms
#'  \item{2} Plot of the policy actions
#' }
#' @param policy_data Object of class [policy_data]
#' @param stage Stage number for plot 2
#' @param history_variables character vector of length 2 for plot 2
#' @param ... Additional arguments passed to [hist()] and [plot()]
#' @examples
#' d <- sim_two_stage(2e3, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = "BB",
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' pe <- policy_eval(pd,
#'                   policy_learn = policy_learn())
#'
#' plot(pe,
#'      which = c(1,2),
#'      policy_data = pd,
#'      stage = 1,
#'      history_variables = c("C", "L"))
#' @export
plot.policy_eval <- function(x,
                             which = c(1),
                             policy_data = NULL,
                             stage = 1,
                             history_variables = NULL,
                             ...){

  if (!is.numeric(which) || any(which < 1) || any(which > 2))
    stop("'which' must be in 1:2")
  show <- rep(FALSE, 2)
  show[which] <- TRUE

  if (!is.null(stage)){
    if (!(is.numeric(stage) & (length(stage) == 1)))
      stop("stage must be an integer greater than 0.")
    if (!(stage %% 1 == 0))
      stop("stage must be an integer greater than 0.")
    if (stage<=0)
      stop("stage must be an integer greater than 0.")
  }
  stage_ <- stage

  if (show[1L]) {
    ic <- IC(x)
    se <- sqrt(mean(ic^2))
    graphics::hist(ic,
                   xlab = "IC",
                   main = "Histogram of Influence Curve Terms",
                   prob = TRUE,
                   ...)
    graphics::curve(stats::dnorm(x, mean=0, sd=se),
          add = TRUE,
          lwd = 2,
          col = "red")
    grDevices::dev.flush()
  }
  if (show[2L]){
    if (is.null(policy_data) | !inherits(policy_data, "policy_data"))
      stop("For plot 2 please provide policy_data.")
    K <- get_K(policy_data)
    if (!(stage_ <= K))
      stop("stage must be lower than or equal to the maximal number of stages.")
    if (is.null(history_variables))
      stop("For plot 2 please provide history_variables (character vector of length 2).")
    if (!(is.character(history_variables) & length(history_variables)==2))
      stop("For plot 2 please provide history_variables (character vector of length 2).")


    if (!all.equal(x[["id"]], get_id(policy_data)))
      stop("policy_eval and policy_data IDs does not match.")

    plot_data <- data.table::merge.data.table(
      get_policy_actions(x)[stage ==stage_],
      get_history(policy_data)[["H"]][stage ==stage_]
    )

    if (!all(history_variables %in% colnames(plot_data)))
      stop("Invalid history_variables.")

    d <- as.factor(plot_data[["d"]])
    main <- paste("Plot of policy actions at stage ", stage_, sep = "")
    xx <- plot_data[[history_variables[1]]]
    if (is.character(xx))
      xx <- factor(xx, levels = sort(unique(xx)))
    yy <- plot_data[[history_variables[2]]]
    if (is.character(yy))
      yy <- factor(yy, levels = sort(unique(yy)))
    graphics::plot.default(
      x = xx,
      xlab = history_variables[1],
      xaxt = 'n',
      y = yy,
      yaxt = 'n',
      ylab = history_variables[2],
      main = main,
      col=d,
      ...)
    graphics::legend('topright', legend = levels(d), col = 1:8, cex = 0.8, pch = 1)
    if (is.factor(xx)){
      graphics::axis(1, at=(1:length(levels(xx))),labels=levels(xx))
    } else {
      graphics::axis(1)
    }
    if (is.factor(yy)){
      graphics::axis(2, at=(1:length(levels(yy))),labels=levels(yy))
    } else {
      graphics::axis(2)
    }


    grDevices::dev.flush()
  }

  invisible()
}


