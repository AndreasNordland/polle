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
#' @param save_g_functions If TRUE, the fitted g-functions are saved.
#' @param save_q_functions Similar to save_g_functions.
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
#' \item{\code{g_values}}{The fitted g-function values.}
#' \item{\code{q_functions}}{(only if \code{M = 1}) The
#' fitted Q-functions. Object of class "nuisance_functions".}
#' \item{\code{q_values}}{The fitted Q-function values.}
#' \item{\code{cross_fits}}{(only if \code{M > 1}) List containing the
#' "policy_eval" object for every (validation) fold.}
#' \item{\code{folds}}{(only if \code{M > 1}) The (validation) folds used
#' for cross-fitting.}
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class \code{policy_eval}:
#' \describe{
#' \item{[get_g_functions()]}{ Extract the fitted g-functions.}
#' \item{[get_q_functions()]}{ Extract the fitted Q-functions.}
#' \item{[get_policy()]}{ Extract the fitted policy object.}
#' \item{[get_policy_functions()]}{ Extract the fitted policy function for
#'                                 a given stage.}
#' \item{[get_policy_actions()]}{ Extract the (fitted) policy actions.}ps
#' \item{[plot.policy_eval()]}{Plot diagnostics.}
#' }
#' @seealso [lava::IC], [lava::estimate.default].
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item \eqn{B} is a vector of baseline covariates.
#'  \item \eqn{U_k} is the reward at stage k (not influenced by the action \eqn{A_k}).
#'  \item \eqn{X_k} is a vector of state covariates summarizing the state at stage k.
#'  \item \eqn{A_k} is the categorical action within the action set \eqn{\mathcal{A}} at stage k.
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
#' the value (\code{value_estimate}):
#' \deqn{E[Q^d_1(H_1, d_1(...))]}
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
                        g_functions = NULL, g_models = g_glm(), g_full_history = FALSE, save_g_functions = TRUE,
                        q_functions = NULL, q_models = q_glm(), q_full_history = FALSE, save_q_functions = TRUE,
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
                             g_models, g_functions, g_full_history, save_g_functions,
                             q_models, q_functions, q_full_history, save_q_functions){

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

  # calculating the doubly robust score and value estimate:
  g_functions <- getElement(fits, "g_functions")
  q_functions <- getElement(fits, "q_functions")
  value_object <- value(type = type,
                        policy_data = valid_policy_data,
                        policy = policy,
                        g_functions = g_functions,
                        q_functions = q_functions)
  g_values <- getElement(value_object, "g_values")
  q_values <- getElement(value_object, "q_values")

  # setting g-functions output:
  if (save_g_functions != TRUE){
    g_functions <- NULL
  }
  # setting Q-functions output:
  if(save_q_functions != TRUE){
    q_functions <- NULL
  }

  out <- list(
    value_estimate = getElement(value_object, "value_estimate"),
    type = type,
    IC = getElement(value_object, "IC"),
    value_estimate_ipw = getElement(value_object, "value_estimate_ipw"),
    value_estimate_or = getElement(value_object, "value_estimate_or"),
    id = get_id(valid_policy_data),
    policy_actions = getElement(value_object, "policy_actions"),
    policy_object = getElement(fits, "policy_object"),
    g_functions = g_functions,
    g_values = g_values,
    q_functions = q_functions,
    q_values = q_values
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

  # collecting the g- and Q-values:
  g_values <- lapply(cross_fits, function(x) getElement(x, "g_values"))
  null_g_values <- unlist(lapply(g_values, is.null))
  if (!all(null_g_values)){
    g_values <- data.table::rbindlist(g_values)
    setkey(g_values, "id", "stage")
  } else {
    g_values <- NULL
  }

  q_values <- lapply(cross_fits, function(x) getElement(x, "q_values"))
  null_q_values <- unlist(lapply(q_values, is.null))
  if (!all(null_q_values)){
    q_values <- data.table::rbindlist(q_values)
    setkey(q_values, "id", "stage")
  } else {
    q_values <- NULL
  }

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
              g_values = g_values,
              q_values = q_values,
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


