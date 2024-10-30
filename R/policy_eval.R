#' Policy Evaluation
#'
#' \code{policy_eval()} is used to estimate
#' the value of a given fixed policy
#' or a data adaptive policy (e.g. a policy
#' learned from the data). \code{policy_eval()}
#' is also used to estimate the average
#' treatment effect among the subjects who would
#' get the treatment under the policy.
#' @param policy_data Policy data object created by [policy_data()].
#' @param policy Policy object created by [policy_def()].
#' @param policy_learn Policy learner object created by [policy_learn()].
#' @param g_functions Fitted g-model objects, see [nuisance_functions].
#' Preferably, use \code{g_models}.
#' @param g_models List of action probability models/g-models for each stage
#' created by [g_empir()], [g_glm()], [g_rf()], [g_sl()] or similar functions.
#' Only used for evaluation if \code{g_functions} is \code{NULL}.
#' If a single model is provided and \code{g_full_history} is \code{FALSE},
#' a single g-model is fitted across all stages. If \code{g_full_history} is
#' \code{TRUE} the model is reused at every stage.
#' @param g_full_history If TRUE, the full history is used to fit each g-model.
#' If FALSE, the state/Markov type history is used to fit each g-model.
#' @param save_g_functions If TRUE, the fitted g-functions are saved.
#' @param q_functions Fitted Q-model objects, see [nuisance_functions].
#' Only valid if the Q-functions are fitted using the same policy.
#' Preferably, use \code{q_models}.
#' @param q_models Outcome regression models/Q-models created by
#' [q_glm()], [q_rf()], [q_sl()] or similar functions.
#' Only used for evaluation if \code{q_functions} is \code{NULL}.
#' If a single model is provided, the model is reused at every stage.
#' @param q_full_history Similar to g_full_history.
#' @param save_q_functions Similar to save_g_functions.
#' @param target Character string. Either "value" or "subgroup". If "value",
#' the target parameter is the policy value.
#' If "subgroup", the target parameter
#' is the average treatement effect among
#' the subgroup of subjects that would receive
#' treatment under the policy, see details.
#' "subgroup" is only implemented for \code{type = "dr"}
#' in the single-stage case with a dichotomous action set.
#' @param type Character string. Type of evaluation. Either \code{"dr"}
#' (doubly robust),
#' \code{"ipw"} (inverse propensity weighting),
#' or \code{"or"} (outcome regression).
#' @param cross_fit_type Character string.
#' Either "stacked", or "pooled", see details. (Only used if \code{M > 1} and target = "subgroup")
#' @param variance_type Character string. Either "pooled" (default),
#' "stacked" or "complete", see details. (Only used if \code{M > 1})
#' @param M Number of folds for cross-fitting.
#' @param rep Repetitions of cross-fitting (estimates averaged over repeated cross-fittings)
#' @param future_args Arguments passed to [future.apply::future_apply()].
#' @param name Character string.
#' @param object,x,y Objects of class "policy_eval".
#' @param labels Name(s) of the estimate(s).
#' @param paired \code{TRUE} indicates that the estimates are based on
#' the same data sample.
#' @param digits Integer. Number of printed digits.
#' @param width Integer. Width of printed parameter name.
#' @param std.error Logical. Should the std.error be printed.
#' @param level Numeric. Level of confidence limits.
#' @param p.value Logical. Should the p.value for associated confidence level be printed.
#' @param ... Additional arguments.
#' @return \code{policy_eval()} returns an object of class "policy_eval".
#' The object is a list containing the following elements:
#' \item{\code{coef}}{Numeric vector. The estimated target parameter:
#' policy value or subgroup average treatment effect.}
#' \item{\code{IC}}{Numeric matrix. Estimated influence curve associated with
#' \code{coef}.}
#' \item{\code{type}}{Character string. The type of evaluation ("dr", "ipw",
#' "or").}
#' \item{\code{target}}{Character string. The target parameter ("value" or "subgroup")}
#' \item{\code{id}}{Character vector. The IDs of the observations.}
#' \item{\code{name}}{Character vector. Names for the each element in \code{coef}.}
#' \item{\code{coef_ipw}}{(only if \code{type = "dr"}) Numeric vector.
#' Estimate of \code{coef} based solely on inverse probability weighting.}
#' \item{\code{coef_or}}{(only if \code{type = "dr"}) Numeric vector.
#' Estimate of \code{coef} based solely on outcome regression.}
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
#' \item{\code{Z}}{(only if \code{target = "subgroup"})
#' Matrix with the doubly robust stage 1 scores for each action.}
#' \item{\code{subgroup_indicator}}{(only if \code{target = "subgroup"})
#' Logical matrix identifying subjects in the subgroup.
#' Each column represents a different subgroup threshold.}
#' \item{\code{cross_fits}}{(only if \code{M > 1}) List containing the
#' "policy_eval" object for every (validation) fold.}
#' \item{\code{folds}}{(only if \code{M > 1}) The (validation) folds used
#' for cross-fitting.}
#' \item{\code{cross_fit_type}}{Character string.}
#' \item{\code{variance_type}}{Character string.}
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class \code{policy_eval}:
#' \describe{
#' \item{[get_g_functions()]}{ Extract the fitted g-functions.}
#' \item{[get_q_functions()]}{ Extract the fitted Q-functions.}
#' \item{[get_policy()]}{ Extract the fitted policy object.}
#' \item{[get_policy_functions()]}{Extract the fitted policy function for a given stage.}
#' \item{[get_policy_actions()]}{ Extract the (fitted) policy actions.}
#' \item{[plot.policy_eval()]}{Plot diagnostics.}
#' }
#' @seealso [lava::IC], [lava::estimate.default].
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item \eqn{B} is a vector of baseline covariates.
#'  \item \eqn{U_k} is the reward at stage k
#' (not influenced by the action \eqn{A_k}).
#'  \item \eqn{X_k} is a vector of state
#' covariates summarizing the state at stage k.
#'  \item \eqn{A_k} is the categorical action
#' within the action set \eqn{\mathcal{A}} at stage k.
#' }
#' The utility is given by the sum of the rewards, i.e.,
#' \eqn{U = \sum_{k = 1}^{K+1} U_k}.
#'
#' A policy is a set of functions
#' \deqn{d = \{d_1, ..., d_K\},}
#' where \eqn{d_k} for \eqn{k\in \{1, ..., K\}}
#' maps \eqn{\{B, X_1, A_1, ..., A_{k-1}, X_k\}} into the
#' action set.
#'
#' Recursively define the Q-models (\code{q_models}):
#' \deqn{Q^d_K(h_K, a_K) = E[U|H_K = h_K, A_K = a_K]}
#' \deqn{Q^d_k(h_k, a_k) = E[Q_{k+1}(H_{k+1},
#' d_{k+1}(B,X_1, A_1,...,X_{k+1}))|H_k = h_k, A_k = a_k].}
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
#' If \code{target = "value"} and \code{type = "or"}
#' \code{policy_eval()} returns the empirical estimate of
#' the value (\code{coef}):
#' \deqn{E\left[Q^d_1(H_1, d_1(\cdot))\right]}
#'
#' If \code{target = "value"} and \code{type = "ipw"} \code{policy_eval()}
#' returns the empirical estimates of
#' the value (\code{coef}) and influence curve (\code{IC}):
#' \deqn{E\left[\left(\prod_{k=1}^K I\{A_k = d_k(\cdot)\}
#' g_k(H_k, A_k)^{-1}\right) U\right].}
#' \deqn{\left(\prod_{k=1}^K I\{A_k =
#' d_k(\cdot)\} g_k(H_k, A_k)^{-1}\right) U -
#' E\left[\left(\prod_{k=1}^K
#' I\{A_k = d_k(\cdot)\} g_k(H_k, A_k)^{-1}\right) U\right].}
#'
#' If \code{target = "value"} and
#' \code{type = "dr"} \code{policy_eval} returns the empirical estimates of
#' the value (\code{coef}) and influence curve (\code{IC}):
#' \deqn{E[Z_1(d,g,Q^d)(O)],}
#' \deqn{Z_1(d, g, Q^d)(O) - E[Z_1(d,g, Q^d)(O)],}
#' where
#' \deqn{
#' Z_1(d, g, Q^d)(O) = Q^d_1(H_1 , d_1(\cdot)) +
#' \sum_{r = 1}^K \prod_{j = 1}^{r}
#' \frac{I\{A_j = d_j(\cdot)\}}{g_{j}(H_j, A_j)}
#' \{Q_{r+1}^d(H_{r+1} , d_{r+1}(\cdot)) - Q_{r}^d(H_r , d_r(\cdot))\}.
#' }
#'
#' If \code{target = "subgroup"}, \code{type = "dr"}, \code{K = 1},
#' and \eqn{\mathcal{A} = \{0,1\}}, \code{policy_eval()}
#' returns the empirical estimates of the subgroup average
#' treatment effect (\code{coef}) and influence curve (\code{IC}):
#' \deqn{E[Z_1(1,g,Q)(O) - Z_1(0,g,Q)(O) | d_1(\cdot) = 1],}
#' \deqn{\frac{1}{P(d_1(\cdot) = 1)} I\{d_1(\cdot) = 1\}
#' \Big\{Z_1(1,g,Q)(O) - Z_1(0,g,Q)(O) - E[Z_1(1,g,Q)(O)
#' - Z_1(0,g,Q)(O) | d_1(\cdot) = 1]\Big\}.}
#'
#' Applying \eqn{M}-fold cross-fitting using the \{M\} argument, let
#' \deqn{\mathcal{Z}_{1,m}(a) = \{Z_1(a, g_m, Q_m^d)(O): O\in \mathcal{O}_m \}.}
#'
#' If \code{target = "subgroup"}, \code{type = "dr"}, \code{K = 1},
#' \eqn{\mathcal{A} = \{0,1\}}, and \code{cross_fit_type = "pooled"},
#' \code{policy_eval()} returns the estimate \deqn{\frac{1}{{N^{-1} \sum_{i =
#' 1}^N I\{d(H_i) = 1\}}} N^{-1} \sum_{m=1}^M \sum_{(Z, H) \in \mathcal{Z}_{1,m}
#' \times \mathcal{H}_{1,m}} I\{d_1(H) = 1\} \left\{Z(1)-Z(0)\right\}} If
#' \code{cross_fit_type = "stacked"} the returned estimate is \deqn{M^{-1}
#' \sum_{m = 1}^M \frac{1}{{n^{-1} \sum_{h \in \mathcal{H}_{1,m}} I\{d(h) =
#' 1\}}} n^{-1} \sum_{(Z, H) \in \mathcal{Z}_{1,m} \times \mathcal{H}_{1,m}}
#' I\{d_1(H) = 1\} \left\{Z(1)-Z(0)\right\},} where for ease of notation we let
#' the integer \eqn{n} be the number of oberservations in each fold.
#' @references
#' van der Laan, Mark J., and Alexander R. Luedtke.
#' "Targeted learning of the mean outcome under an optimal dynamic treatment rule."
#' Journal of causal inference 3.1 (2015): 61-95.
#' \doi{10.1515/jci-2013-0022}
#' \cr \cr
#' Tsiatis, Anastasios A., et al. Dynamic
#' treatment regimes: Statistical methods for precision medicine. Chapman and
#' Hall/CRC, 2019. \doi{10.1201/9780429192692}.
#' \cr \cr
#' Victor Chernozhukov, Denis
#' Chetverikov, Mert Demirer, Esther Duflo, Christian Hansen, Whitney Newey,
#' James Robins, Double/debiased machine learning for treatment and structural
#' parameters, The Econometrics Journal, Volume 21, Issue 1, 1 February 2018,
#' Pages C1–C68, \doi{10.1111/ectj.12097}.
#' @export
#' @examples
#' library("polle")
#' ### Single stage:
#' d1 <- sim_single_stage(5e2, seed=1)
#' pd1 <- policy_data(d1,
#'                    action = "A",
#'                    covariates = list("Z", "B", "L"),
#'                    utility = "U")
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
#' pl2 <- policy_learn(
#'    type = "drql",
#'    control = control_drql(qv_models = list(q_glm(~C_1),
#'                                            q_glm(~C_1+C_2))),
#'    full_history = TRUE,
#'    L = 2) # number of folds for cross-fitting
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
                        g_functions = NULL, g_models = g_glm(),
                        g_full_history = FALSE, save_g_functions = TRUE,
                        q_functions = NULL, q_models = q_glm(),
                        q_full_history = FALSE, save_q_functions = TRUE,
                        target = "value",
                        type = "dr",
                        cross_fit_type = "pooled",
                        variance_type = "pooled",
                        M = 1,
                        rep = 1,
                        future_args = list(future.seed=TRUE),
                        name = NULL
                        ) {
  # input checks:
  if (!inherits(policy_data, what = "policy_data"))
    stop("policy_data must be of inherited class 'policy_data'.")
  if (!is.null(policy)) {
    if (!inherits(policy, what = "policy")) {
      stop("policy must be of inherited class 'policy'.")
    }
  }
  if ((is.null(policy) && is.null(policy_learn)) ||
    (!is.null(policy_learn) && !is.null(policy))) {
    stop("Provide either policy or policy_learn.")
  }
  if (is.null(policy) && !is.null(policy_learn)) {
    if (!inherits(policy_learn, what = "policy_learn")) {
      stop("policy_learn must be of inherited class 'policy_learn'.")
    }
  }
  if (!is.null(g_functions)) {
    if (!(inherits(g_functions, "g_functions"))) {
      stop("g_functions must be of class 'g_functions'.")
    }
  }
  if (!(is.logical(g_full_history) && (length(g_full_history) == 1))) {
    stop("g_full_history must be TRUE or FALSE")
  }
  if (!is.null(q_functions)) {
    if (!(inherits(q_functions, "q_functions"))) {
      stop("q-functions must be of class 'q_functions'.")
    }
  }
  if (!(is.logical(q_full_history) && (length(q_full_history) == 1))) {
    stop("q_full_history must be TRUE or FALSE")
  }
  if (!(is.numeric(M) && (length(M) == 1))) {
    stop("M must be an integer greater than 0.")
  }
  if (!(M %% 1 == 0)) {
    stop("M must be an integer greater than 0.")
  }
  if (M <= 0) {
    stop("M must be an integer greater than 0.")
  }
  if (!is.list(future_args)) {
    stop("future_args must be a list.")
  }
  if (!is.null(name)) {
    name <- as.character(name)
    if (length(name) != 1) {
      stop("name must be a character string.")
    }
  }
  target <- tolower(target)
  if (length(target) != 1) {
    stop("target must be a character string.")
  }
  if (target %in% c(
    "value",
    "policy_value"
  )) {
    target <- "value"
  } else if (target %in% c(
    "subgroup",
    "sub_effect",
    "subeffect",
    "subvalue",
    "sub_value"
  )) {
    target <- "subgroup"
  } else {
    stop("target must be either 'value' or 'subgroup'.")
  }
  type <- tolower(type)
  if (length(type) != 1) {
    stop("type must be a character string.")
  }
  if (type %in% c("dr", "aipw")) {
    type <- "dr"
  } else if (type %in% c("ipw")) {
    type <- "ipw"
  } else if (type %in% c("or", "q")) {
    type <- "or"
  } else {
    stop("type must be either 'dr', 'ipw' or  'or'.")
  }

  ## editing name:
  if (is.null(name)) {
    if (target == "value") {
      if (!is.null(policy)) {
        pol_name <- attr(policy, "name")
      } else {
        pol_name <- attr(policy_learn, "name")
      }
      name <- "E[Z(d)]"
      if (!is.null(pol_name)) {
        name <- paste0(name, ": d=", pol_name)
      }
      rm(pol_name)
    }

    if (target == "subgroup") {
      if (!is.null(policy)) {
        pol_name <- attr(policy, "name")
      } else {
        pol_name <- attr(policy_learn, "name")
      }
      as <- get_action_set(policy_data)
      name1 <- paste0("E[Z(", as[2], ")-Z(", as[1], ")|d=", as[2], "]")
      name2 <- paste0("E[Z(", as[2], ")-Z(", as[1], ")|d=", as[1], "]")
      if (!is.null(pol_name)) {
        name <- c(
          paste0(name1, ": d=", pol_name),
          paste0(name2, ": d=", pol_name)
        )
      } else {
        name <- c(name1, name2)
      }
      rm(as, pol_name, name1, name2)
    }
  }

  ## collecting the arguments to be passed on:
  args <- as.list(environment())
  args[["policy_data"]] <- NULL
  args[["M"]] <- NULL
  args[["future_args"]] <- NULL
  args[["variance_type"]] <- NULL
  args[["cross_fit_type"]] <- NULL
  args[["rep"]] <- NULL

  if (M > 1) {
    eval <- policy_eval_cross(
      args = args,
      policy_data = policy_data,
      M = M,
      cross_fit_type = cross_fit_type,
      variance_type = variance_type,
      future_args = future_args
    )
    eval$rep <- rep

    if (rep > 1) {
      ## Repeated cross-fitting to reduce dependence on seed
      eval0 <- function(...) {
        res <- policy_eval_cross(
          args = args,
          policy_data = policy_data,
          M = M,
          cross_fit_type = cross_fit_type,
          variance_type = variance_type,
          future_args = future_args
        )
        list(coef=res$coef, IC=res$IC)
      }
      val <- do.call(
        what = future.apply::future_lapply,
        c(list(X = seq_len(rep - 1), FUN = eval0), future_args)
      )
      for (x in val) {
        eval$coef <- eval$coef + x$coef
        eval$IC <- eval$IC + x$IC
      }
      eval$coef <- eval$coef / rep
      eval$IC <- eval$IC / rep
    }

  } else {
    args[["train_policy_data"]] <- policy_data
    args[["valid_policy_data"]] <- policy_data
    eval <- do.call(what = policy_eval_type, args = args)
  }

  return(eval)
}

policy_eval_object <- function(
    coef,
    IC,
    type,
    target,
    id,
    name,
    coef_ipw = NULL,
    coef_or = NULL,
    policy_actions = NULL,
    policy_object = NULL,
    g_functions = NULL,
    g_values = NULL,
    q_functions = NULL,
    q_values = NULL,
    Z = NULL,
    subgroup_indicator = NULL,
    cross_fits = NULL,
    folds = NULL,
    cross_fit_type = NULL,
    variance_type = NULL) {
  out <- as.list(environment())
  out <- remove_null_elements(out)
  class(out) <- c("policy_eval")

  return(out)
}

policy_eval_type <- function(target,
                             type,
                             train_policy_data,
                             valid_policy_data,
                             policy, policy_learn,
                             g_models, g_functions,
                             g_full_history, save_g_functions,
                             q_models, q_functions,
                             q_full_history, save_q_functions,
                             name) {
  ##
  ## training
  ##

  ## fitting the g-functions, the q-functions and the policy/policies:
  fits <- fit_functions(
    policy_data = train_policy_data,
    type = type,
    policy = policy, policy_learn = policy_learn,
    g_models = g_models, g_functions = g_functions,
    g_full_history = g_full_history,
    q_models = q_models, q_functions = q_functions,
    q_full_history = q_full_history
  )
  rm(train_policy_data)

  ## getting the fitted g-functions and Q-functions:
  g_functions <- get_element(fits, "g_functions")
  q_functions <- get_element(fits, "q_functions")

  ## getting the fitted policy/policies as a list:
  if (is.null(policy)) {
    policy <- get_policy(get_element(fits, "policy_object"))
  }
  if (inherits(policy, what = "policy")) {
    policy <- list(policy)
  }

  ##
  ## validating
  ##

  # getting the g-function values:
  g_values <- NULL
  if (!is.null(g_functions)) {
    g_values <- predict(g_functions, valid_policy_data)
  }

  # getting the Q-function values:
  q_values <- NULL
  if (!is.null(q_functions)) {
    q_values <- predict(q_functions, valid_policy_data)
  }

  # setting g-functions output:
  if (save_g_functions != TRUE) {
    g_functions <- NULL
  }
  # setting Q-functions output:
  if (save_q_functions != TRUE) {
    q_functions <- NULL
  }

  # getting the number of stages:
  K <- get_K(valid_policy_data)

  # getting the action set and stage action set:
  action_set <- get_action_set(valid_policy_data)

  # getting the observed actions:
  actions <- get_actions(valid_policy_data)

  # getting the utility:
  utility <- get_utility(valid_policy_data)


  ## calculating the target estimate for each policy:
  if (length(policy) == 1) {
    # getting the policy actions:
    policy_actions <- policy[[1]](valid_policy_data)

    # checking that the policy actions comply with the stage action sets:
    check_actions(
      actions = policy_actions,
      policy_data = valid_policy_data
    )

    estimate_objects <- estimate_target(
      target = target,
      type = type,
      K,
      action_set,
      actions,
      policy_actions,
      g_values,
      q_values,
      utility
    )
    estimate_objects <- list(estimate_objects)
  } else {
    policy_actions <- NULL

    estimate_objects <- lapply(
      policy,
      function(p) {
        # getting the policy actions:
        policy_actions <- p(valid_policy_data)

        # checking that the policy actions comply with the stage action sets:
        check_actions(
          actions = policy_actions,
          policy_data = valid_policy_data
        )

        out <- estimate_target(
          target = target,
          type = type,
          K,
          action_set,
          actions,
          policy_actions,
          g_values,
          q_values,
          utility
        )
        return(out)
      }
    )
  }

  subgroup_indicator <- NULL
  Z <- NULL
  if (target == "subgroup") {
    ## getting the doubly robust score for each treatment:
    Z <- get_element(estimate_objects[[1]], "Z")
    ## getting the subgroup indicator (if available):
    subgroup_indicator <- lapply(
      estimate_objects,
      function(eb) get_element(eb, "subgroup_indicator")
    )
    subgroup_indicator <- do.call(what = "cbind", subgroup_indicator)
  }

  ## collecting the target estimates:
  coef <- unlist(lapply(estimate_objects, function(eb) get_element(eb, "coef")))
  coef_ipw <- unlist(lapply(
    estimate_objects,
    function(eb) get_element(eb, "coef_ipw", check_name = FALSE)
  ))
  coef_or <- unlist(lapply(
    estimate_objects,
    function(eb) get_element(eb, "coef_or", check_name = FALSE)
  ))

  ## getting the influence curves:
  IC <- lapply(estimate_objects, function(eb) get_element(eb, "IC"))
  IC <- do.call(what = "cbind", IC)

  if (target == "subgroup" & length(coef) > 1) {
    ## Reorder coefficients so all thresholds for d=1 are presented first
    ## followied by d=0
    idx <- seq_len(length(coef) / 2) * 2 - 1
    coef <- coef[c(idx, idx + 1)]
    IC <- IC[, c(idx, idx + 1)]
  }

  out <- policy_eval_object(
    coef = coef,
    IC = IC,
    type = type,
    target = target,
    coef_ipw = coef_ipw,
    coef_or = coef_or,
    id = get_id(valid_policy_data),
    policy_actions = policy_actions,
    policy_object = get_element(fits, "policy_object", check_name = FALSE),
    g_functions = g_functions,
    g_values = g_values,
    q_functions = q_functions,
    q_values = q_values,
    Z = Z,
    subgroup_indicator = subgroup_indicator,
    name = name
  )

  return(out)
}

policy_eval_cross <- function(args,
                              policy_data,
                              M,
                              cross_fit_type,
                              variance_type,
                              future_args
                              ) {

  ## check cross_fit_type input:
  cross_fit_type <- tolower(cross_fit_type)
  if (length(cross_fit_type) != 1) {
    stop("cross_fit_type must be a character string.")
  }
  if (!(cross_fit_type %in% c("pooled", "stacked"))) {
    stop("cross_fit_type must be either 'pooled' or 'stacked'.")
  }

  ## check varince_type input:
  variance_type <- tolower(variance_type)
  if (length(variance_type) != 1) {
    stop("variance_type must be a character string.")
  }
  if (!(variance_type %in% c("pooled", "stacked", "complete"))) {
    stop("variance_type must be either 'pooled', 'stacked' or 'complete'.")
  }

  type <- get_element(args, "type")
  target <- get_element(args, "target")
  name <- get_element(args, "name")
  n <- get_n(policy_data)
  id <- get_id(policy_data)
  policy_learn <- get_element(args, "policy_learn")
  if (!is.null(policy_learn)) {
    n_coef <- length(get_element(attr(policy_learn, "pl_args"), "threshold"))
  } else {
    n_coef <- 1
  }

  ## setting up the folds:
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))
  folds <- lapply(folds, sort)
  names(folds) <- paste("fold_", 1:M, sep = "")

  ## cross-fitting the policy evaluation:
  prog <- progressor(along = folds)
  cross_args <- append(
    list(
      X = folds,
      FUN = policy_eval_fold,
      policy_data = policy_data,
      args = args,
      prog = prog
    ),
    future_args
  )

  cross_fits <- do.call(what = future.apply::future_lapply, cross_args)

  ## collecting the ids from each fold (unsorted):
  id <- unlist(lapply(
    cross_fits,
    function(x) get_element(x, "id")
  ), use.names = FALSE)

  ## getting the number of observations in each (training) fold (unsorted):
  n_folds <- unlist(lapply(
    cross_fits,
    function(x) length(get_element(x, "id"))
  ))

  ## collecting the cross-fitted policy actions (sorted):
  policy_actions <- NULL
  if (n_coef == 1) {
    policy_actions <- lapply(
      cross_fits, function(x) get_element(x, "policy_actions")
    )
    policy_actions <- rbindlist(policy_actions)
    setkey(policy_actions, "id", "stage")
  }

  ## collecting the subgroup indicator (sorted):
  subgroup_indicator <- NULL
  if (target == "subgroup") {
    subgroup_indicator <- lapply(
      cross_fits, function(x) get_element(x, "subgroup_indicator")
    )
    subgroup_indicator <- do.call(what = "rbind", subgroup_indicator)
    subgroup_indicator <- subgroup_indicator[order(id), , drop = FALSE]
    subgroup_indicator <- cbind(subgroup_indicator, !subgroup_indicator)
  }

  ##
  ## collecting the cross-fitted doubly robust scores (sorted):
  ##

  ## target parameter: policy value
  if (target == "value") {
    ## vector of length n with elements Z_1(d):
    Zd <- lapply(
      cross_fits,
      function(x) {
        IC <- get_element(x, "IC")
        coef <- get_element(x, "coef")
        sweep(IC, MARGIN = 2, coef, FUN = "+")
      }
    )
    Zd <- do.call(what = "rbind", Zd)
    Zd <- Zd[order(id), , drop = FALSE]
  }

  ## target parameter: subgroup average treatment effect
  if (target == "subgroup") {
    Z <- lapply(
      cross_fits,
      function(x) {
        get_element(x, "Z")
      }
    )
    Z <- do.call(what = "rbind", Z)
    Z <- Z[order(id), , drop = FALSE]
  }

  ##
  ## calculating the cross-fitted estimate:
  ##

  ## target parameter: policy value
  if (target == "value") {
    coef <- apply(Zd, MARGIN = 2, mean)
  }

  ## target parameter: subgroup average treatment effect
  if (target == "subgroup") {
    if (cross_fit_type == "stacked") {
      coef <- lapply(
        cross_fits, function(x) get_element(x, "coef")
      )
      coef <- do.call(what = "rbind", coef)
      coef <- apply(
        coef,
        MARGIN = 2,
        function(x) {
          sum((n_folds / sum(n_folds)) * x)
        }
      )
    }
    if (cross_fit_type == "pooled") {
      ## calculating the subgroup average treatment effect estimate:
      coef <- apply(subgroup_indicator,
        MARGIN = 2,
        function(x) mean((Z[, 2] - Z[, 1])[x])
      )
    }
    n_coef <- length(coef)
  }

  ##
  ## calculating the influence curve for the cross-fitted estimator (sorted):
  ##

  ## target parameter: policy value
  if (target == "value") {
    if (variance_type == "stacked") {
      IC <- lapply(
        cross_fits, function(x) IC(x)
      )
      IC <- do.call(what = "rbind", IC)
      IC <- IC[order(id), , drop = FALSE]
    }
    if (variance_type == "pooled") {
      IC <- sweep(Zd, MARGIN = 2, coef)
    }
    if (variance_type == "complete") {
      args[["train_policy_data"]] <- policy_data
      args[["valid_policy_data"]] <- policy_data
      pe <- do.call(what = policy_eval_type, args = args)
      IC <- IC(pe)
      rm(pe)
    }
  }

  ## target parameter: subgroup average treatment effect
  if (target == "subgroup") {
    if (variance_type == "stacked") {
      IC <- lapply(
        cross_fits, function(x) IC(x)
      )
      IC <- do.call(what = "rbind", IC)
      IC <- IC[order(id), , drop = FALSE]
    }
    if (variance_type == "pooled") {
      tmp <- apply(subgroup_indicator,
        MARGIN = 2,
        function(x) mean((Z[, 2] - Z[, 1])[x])
        )
      IC <- matrix(nrow = n, ncol = n_coef)
      for (j in 1:n_coef) {
        ic <- 1 / mean(subgroup_indicator[, j]) *
          subgroup_indicator[, j] * ((Z[, 2] - Z[, 1]) - tmp[j])
        IC[, j] <- ic
      }
      rm(ic, tmp)
    }
    if (variance_type == "complete") {
      args[["train_policy_data"]] <- policy_data
      args[["valid_policy_data"]] <- policy_data
      pe <- do.call(what = policy_eval_type, args = args)
      IC <- IC(pe)
      rm(pe)
    }
  }


  ## calculating the cross-fitted ipw value estimate
  ## (only if target = "value" and type = "dr"):
  coef_ipw <- NULL
  if (target == "value" && type == "dr") {
    coef_ipw <- lapply(
      cross_fits, function(x) get_element(x, "coef_ipw")
    )
    coef_ipw <- do.call(what = "rbind", coef_ipw)
    coef_ipw <- apply(
      coef_ipw,
      MARGIN = 2,
      function(x) {
        sum((n_folds / sum(n_folds)) * x)
      }
    )
  }


  ## calculating the cross-fitted outcome regression value estimate
  ## (only if target = "value" and type = "dr"):
  coef_or <- NULL
  if (target == "value" && type == "dr") {
    coef_or <- lapply(
      cross_fits, function(x) get_element(x, "coef_or")
    )
    coef_or <- do.call(what = "rbind", coef_or)
    coef_or <- apply(
      coef_or,
      MARGIN = 2,
      function(x) {
        sum((n_folds / sum(n_folds)) * x)
      }
    )
  }

  ## collecting the cross-fitted g- and Q-values (sorted):
  g_values <- lapply(
    cross_fits,
    function(x) get_element(x, "g_values", check_name = FALSE)
  )
  null_g_values <- unlist(lapply(g_values, is.null))
  if (!all(null_g_values)) {
    g_values <- data.table::rbindlist(g_values)
    setkey(g_values, "id", "stage")
  } else {
    g_values <- NULL
  }
  q_values <- lapply(
    cross_fits,
    function(x) get_element(x, "q_values", check_name = FALSE)
  )
  null_q_values <- unlist(lapply(q_values, is.null))
  if (!all(null_q_values)) {
    q_values <- data.table::rbindlist(q_values)
    setkey(q_values, "id", "stage")
  } else {
    q_values <- NULL
  }

  ## sorting id:
  id <- id[order(id)]

  out <- policy_eval_object(
    coef = coef,
    IC = IC,
    type = type,
    target = target,
    coef_ipw = coef_ipw,
    coef_or = coef_or,
    id = id,
    policy_actions = policy_actions,
    g_values = g_values,
    q_values = q_values,
    cross_fits = cross_fits,
    folds = folds,
    name = name,
    variance_type = variance_type,
    cross_fit_type = cross_fit_type
  )

  return(out)
}

policy_eval_fold <- function(fold,
                             policy_data,
                             args,
                             prog) {
  K <- get_K(policy_data)
  id <- get_id(policy_data)

  train_id <- id[-fold]
  validation_id <- id[fold]

  # training data:
  train_policy_data <- subset_id(policy_data, train_id)
  if (get_K(train_policy_data) != K) {
    stop("The number of stages varies accross the training folds.")
  }

  # validation data:
  valid_policy_data <- subset_id(policy_data, validation_id)
  if (get_K(valid_policy_data) != K) {
    stop("The number of stages varies accross the validation folds.")
  }

  eval_args <- append(args, list(
    valid_policy_data = valid_policy_data,
    train_policy_data = train_policy_data
  ))

  out <- do.call(what = "policy_eval_type", args = eval_args)

  # progress:
  prog()

  return(out)
}
