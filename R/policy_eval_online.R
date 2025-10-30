#' Online/Sequential Policy Evaluation
#'
#' \code{policy_eval_online()} is used to estimate
#' the value of a given fixed policy
#' or a data adaptive policy (e.g. a policy
#' learned from the data). \code{policy_eval_online()}
#' is also used to estimate the subgroup average
#' treatment effect as defined by the (learned) policy.
#' The evaluation is based on a online/sequential validation
#' estimation scheme making the estimation approach valid for a
#' non-converging policy under no heterogenuous treatment effect
#' (exceptional law), see details.
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
#' @param c_functions Fitted c-model/censoring probability model objects. Preferably, use \code{c_models}.
#' @param c_models List of right-censoring probability models, see [c_model].
#' @param c_full_history Similar to g_full_history.
#' @param save_c_functions Similar to save_g_functions.
#' @param m_function Fitted outcome model object for stage K+1. Preferably, use \code{m_model}.
#' @param m_model Outcome model for the utility at stage K+1. Only used if the final utility
#' contribution is missing/has been right-censored
#' @param m_full_history Similar to g_full_history.
#' @param save_m_function Similar to save_g_functions.
#' @param target Character string. Either "value" or "subgroup". If "value",
#' the target parameter is the policy value.
#' If "subgroup", the target parameter
#' is the subgroup average treatement effect given by the policy, see details.
#' "subgroup" is only implemented for \code{type = "dr"}
#' in the single-stage case with a dichotomous action set.
#' @param M Number of folds for online estimation/sequential validation excluding the initial training block, see details.
#' @param train_block_size Integer. Size of the initial training block only
#' used for training of the policy and nuisance models, see details.
#' @param name Character string.
#' @param min_subgroup_size Minimum number of observations in the evaluated subgroup (Only used if target = "subgroup").
#' @return \code{policy_eval_online()} returns an object of inherited class "policy_eval_online", "policy_eval".
#' The object is a list containing the following elements:
#' \item{\code{coef}}{Numeric vector. The estimated target parameters:
#' policy value or subgroup average treatment effect.}
#' \item{\code{vcov}}{Numeric vector. The estimated squared standard deviation associated with
#' \code{coef}.}
#' \item{\code{target}}{Character string. The target parameter ("value" or "subgroup")}
#' \item{\code{id}}{Character vector. The IDs of the observations.}
#' \item{\code{name}}{Character vector. Names for the each element in \code{coef}.}
#' \item{\code{train_sequential_index}}{list of indexes used for training at each step.}
#' \item{\code{valid_sequential_index}}{list of indexes used for validation at each step.}
#' @details
#' ## Setup
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
#' A (subgroup) policy is a set of functions
#' \deqn{d = \{d_1, ..., d_K\},}
#' where \eqn{d_k} for \eqn{k\in \{1, ..., K\}}
#' maps a subset or function \eqn{V_1} of \eqn{\{B, X_1, A_1, ..., A_{k-1}, X_k\}} into the
#' action set (or set of subgroups).
#'
#' Recursively define the Q-models (\code{q_models}):
#' \deqn{Q^d_K(h_K, a_K) = \mathbb{E}[U|H_K = h_K, A_K = a_K]}
#' \deqn{Q^d_k(h_k, a_k) = \mathbb{E}[Q_{k+1}(H_{k+1},
#' d_{k+1}(V_{k+1}))|H_k = h_k, A_k = a_k].}
#' If \code{q_full_history = TRUE},
#' \eqn{H_k = \{B, X_1, A_1, ..., A_{k-1}, X_k\}}, and if
#' \code{q_full_history = FALSE}, \eqn{H_k = \{B, X_k\}}.
#'
#' The g-models (\code{g_models}) are defined as
#' \deqn{g_k(h_k, a_k) = \mathbb{P}(A_k = a_k|H_k = h_k).}
#' If \code{g_full_history = TRUE},
#' \eqn{H_k = \{B, X_1, A_1, ..., A_{k-1}, X_k\}}, and if
#' \code{g_full_history = FALSE}, \eqn{H_k = \{B, X_k\}}.
#' Furthermore, if \code{g_full_history = FALSE} and \code{g_models} is a
#' single model, it is assumed that \eqn{g_1(h_1, a_1) = ... = g_K(h_K, a_K)}.
#'
#' ## Target parameters
#' If \code{target = "value"}, \code{policy_eval_online}
#' returns the estimates of
#' the value, i.e., the expected potential utility under the policy, (\code{coef}):
#' \deqn{\mathbb{E}[U^{(d)}]}
#' If \code{target = "subgroup"}, \code{K = 1}, \eqn{\mathcal{A} = \{0,1\}},
#' and \eqn{d_1(V_1) \in \{s_1, s_2\} },  \code{policy_eval()}
#' returns the estimates of the subgroup average
#' treatment effect (\code{coef}):
#' \deqn{\mathbb{E}[U^{(1)} - U^{(0)}| d_1(\cdot) = s]\quad s\in \{s_1,s_2\},}
#'
#' ## Online estimation/sequential validation
#' Estimation of the target parameter is based online estimation/sequential
#' validation using the doubly robust score. The following figure illustrate
#' online estimation using \code{M = 5} steps and an initial training block of
#' size \code{train_block_size = }\eqn{l}.
#'
#' ![](online-validation.jpg "Online estimaiton scheme")
#'
#' Step 1:
#'
#' The \eqn{n} observations are randomly ordered. In step 1,
#' the first \eqn{\{1,...,l\}} observations, highlighted in teal/blue, are used to fit the
#' Q-models, g-models, the policy (if using the \code{policy_learn} argument), and other required models.
#' We denote the collection of these fitted models as \eqn{P}.
#' The remaining observations are split into \code{M} blocks of size \eqn{m = (n-l)/M}, which
#' we for simplicity assume to be a whole number. In step 1, the target
#' parameter is estimated using the associated doubly robust score \eqn{Z(P)}
#' evaluated on the first validation fold
#' highlighted in pink \eqn{\{l+1,...,l+m\}}:
#'
#' \deqn{
#'  \frac{\sum_{i = l+1}^{l+m}  {\widehat \sigma_{i}^{-1}}
#'  Z(\widehat P_i)(O_i)}
#'  {\sum_{i = l+1}^{l+m} \widehat \sigma_{i}^{-1}},
#' }
#' where \eqn{\widehat P_i} for \eqn{i \in \{l+1,...,l+m\}}
#' refer to the fitted models trained on \eqn{\{1,...,l\}}, and \eqn{\widehat \sigma_i}
#' is the insample estimate for the standard deviation based on the training observations \eqn{\{1,...,l\}}.
#' We will later give an exact expression for \eqn{\widehat \sigma_i} for each target parameter.
#' Note that \eqn{\widehat \sigma_i} is constant for \eqn{i \in \{l+1,...,l+m\}}, but it will be
#' convenient to keep the same index for \eqn{\widehat \sigma}.
#'
#' Step 2 to M:
#'
#' In step 2, observations with index \eqn{\{1,...,l+m\}} are used to fit the model collection \eqn{P},
#' as well as the insample estimate for the standard deviation. For \eqn{i \in \{l+m+1,...,l+2m\}} these are
#' denoted as \eqn{\widehat P_i, \widehat \sigma_i}.
#' This sequential model fitting is repeated for all \code{M}
#' steps and the updated online estimator is given by
#'
#' \deqn{
#'  \frac{\sum_{i = l+1}^{n}  {\widehat \sigma_{i}^{-1}}
#'  Z(\widehat P_i)(O_i)}
#'  {\sum_{i = l+1}^n \widehat \sigma_{i}^{-1}},
#' }
#'
#' with an associated standard error estimate given by
#'
#' \deqn{\frac{\left(\frac{1}{n-l}\sum_{i = l+1}^n \widehat \sigma_{i}^{-1}\right)^{-1}}{\sqrt{n-l}}.}
#'
#' ## Doubly robust scores
#' \code{target = "value"}:
#'
#' For a policy value target the doubly robust score is given by
#' \deqn{
#' Z(d, g, Q^d)(O) = Q^d_1(H_1 , d_1(V_1)) +
#' \sum_{r = 1}^K \prod_{j = 1}^{r}
#' \frac{I\{A_j = d_j(\cdot)\}}{g_{j}(H_j, A_j)}
#' \{Q_{r+1}^d(H_{r+1} , d_{r+1}(V_1)) - Q_{r}^d(H_r , d_r(V_1))\}.
#' }
#' The influence function(/curve) for the associated onestep etimator is
#' \deqn{Z(d, g, Q^d)(O) - \mathbb{E}[Z(d,g, Q^d)(O)],}
#' which is used to estimate the insample stadard deviation. For example,
#' in step 2, i.e., for \eqn{i \in \{l+m+1,...,l+2m\}}
#' \deqn{
#'  \widehat \sigma_i^2 = \frac{1}{l+m}\sum_{j=1}^{l+m} \left(Z(\widehat d_i,\widehat Q_i,\widehat{g}_i)(O_j) - \frac{1}{l+m}\sum_{r=1}^{l+m} Z(\widehat d_i,\widehat Q_i,\widehat{g}_i)(O_r) \right)^2
#' }
#'
#' \code{target = "subgroup"}:
#'
#' For a subgroup average treatment effect target,
#' where \code{K = 1} (single-stage),
#' \eqn{\mathcal{A} = \{0,1\}} (binary treatment), and
#' \eqn{d_1(V_1) \in \{s_1, s_2\}} (dichotomous subgroup policy) the
#' doubly robust score is given by
#'
#' \deqn{
#' Z(d,g,Q, D) = \frac{I\{d_1(\cdot) = s\}}{D}
#' \Big\{Z_1(1,g,Q)(O) - Z_1(0,g,Q)(O) \Big\}.}
#' \deqn{
#' Z_1(a, g, Q)(O) = Q_1(H_1 , a) +
#' \frac{I\{A = a\}}{g_1(H_1, a)}
#' \{U - Q_{1}(H_1 , a)\},
#' }
#' where \eqn{D} is \eqn{\mathbb{P}(d_1(V_1) = s)}.
#'
#' The associated onestep/estimating equation estimator has influence function
#' \deqn{\frac{ I\{d_1(\cdot) = s\}}{D}
#' \Big\{Z_1(1,g,Q)(O) - Z_1(0,g,Q)(O) - E[Z_1(1,g,Q)(O)
#' - Z_1(0,g,Q)(O) | d_1(\cdot) = s]\Big\},}
#' which is used to estimate the standard deviation \eqn{\widehat \sigma}.
#'
#' @references
#' Luedtke, Alexander R, and Mark J van der Laan.
#' “STATISTICAL INFERENCE FOR THE MEAN OUTCOME
#' UNDER A POSSIBLY NON-UNIQUE OPTIMAL TREATMENT STRATEGY.”
#' Annals of statistics vol. 44,2 (2016): 713-742.
#' \doi{10.1214/15-AOS1384}
#' @export
policy_eval_online <- function(policy_data,
                               policy = NULL, policy_learn = NULL,
                               g_functions = NULL, g_models = g_glm(),
                               g_full_history = FALSE, save_g_functions = TRUE,
                               q_functions = NULL, q_models = q_glm(),
                               q_full_history = FALSE, save_q_functions = TRUE,
                               c_functions = NULL, c_models = NULL,
                               c_full_history = FALSE, save_c_functions = TRUE,
                               m_function = NULL, m_model = NULL,
                               m_full_history = FALSE, save_m_function = TRUE,
                               target = "value",
                               M = 4,
                               train_block_size = get_n(policy_data) / 5,
                               name = NULL,
                               min_subgroup_size = 1) {
  ## setting type to "dr":
  type <- "dr"

   ## argument input checks:
  args <- as.list(environment())
  args <- do.call(what = "policy_eval_input_checks", args)

  ## collecting the arguments to be passed on:
  args[["policy_data"]] <- NULL
  args[["M"]] <- NULL
  args[["train_block_size"]] <- NULL
  args[["future_args"]] <- NULL
  args[["variance_type"]] <- NULL
  args[["cross_fit_type"]] <- NULL
  args[["nrep"]] <- NULL

  eval <- policy_eval_on(args = args,
                         policy_data = policy_data,
                         train_block_size = train_block_size,
                         M = M)

  return(eval)
}


policy_eval_on <- function(args,
                           policy_data,
                           train_block_size,
                           M) {
  n <- get_n(policy_data)
  id <- get_id(policy_data)
  K <- get_K(policy_data)
  target <- get_element(args, "target")

  ## random index of the data (by id):
  random_index <- sample(1:n, n)

  ## setting up the sequential training and validation folds:
  sequential_index <- random_index[(train_block_size +1):n]
  sequential_index <- split(sequential_index, rep(1:M, length.out = length(sequential_index)))

  train_sequential_index <- list(random_index[1:train_block_size])
  train_sequential_index <- append(train_sequential_index, sequential_index[-M])
  if (M > 1){
    tmp <- train_sequential_index[[1]]
    for (m in 2:length(train_sequential_index)) {
      tmp <- c(tmp, train_sequential_index[[m]])
      train_sequential_index[[m]] <- tmp
    }
    rm(tmp)
  }
  train_sequential_index <- lapply(train_sequential_index, sort)
  valid_sequential_index <- sequential_index
  valid_sequential_index <- lapply(valid_sequential_index, sort)

  sequential_fits <- lapply(seq_along(train_sequential_index), function(m){
    ## training and validation ids:
    train_id <- id[train_sequential_index[[m]]]
    validation_id <- id[valid_sequential_index[[m]]]

    ## training data:
    train_policy_data <- subset_id(policy_data, train_id)
    if (get_K(train_policy_data) != K) {
      stop("The number of stages varies accross the training folds.")
    }

    ## validation data:
    valid_policy_data <- subset_id(policy_data, validation_id)
    if (get_K(valid_policy_data) != K) {
      stop("The number of stages varies accross the validation folds.")
    }


    eval_args <- append(args,
                        list(valid_policy_data = valid_policy_data,
                             train_policy_data = train_policy_data))

    valid_pe <- do.call(what = "policy_eval_type", args = eval_args)

    ## getting the in-sample variance estimate
    args[["g_models"]] <- NULL
    args[["q_models"]] <- NULL

    args[["g_functions"]] <- get_g_functions(valid_pe)
    args[["q_functions"]] <- get_q_functions(valid_pe)

    policy <- get_policy(valid_pe)
    if (is.null(policy)){
      policy <- get_element(args, "policy")
    }
    if (inherits(policy, what = "policy")) {
      policy <- list(policy)
    }

    insample_parameters <- lapply(policy, function(p){
      args[["policy"]] <- p

      eval_args <- append(args,
                          list(valid_policy_data = train_policy_data,
                               train_policy_data = train_policy_data))

      pe <- do.call(what = "policy_eval_type", args = eval_args)

      ic <- IC(pe)
      sigma2 <- var(ic)*(nrow(ic)-1)/nrow(ic)
      sigma2 <- diag(sigma2)
      names(sigma2) <- get_element(pe, "name")

      out <- list(sigma2 = sigma2)

      if (target == "subgroup"){
        subgroup_indicator <- get_element(pe, "subgroup_indicator")
        subgroup_prob <- apply(
          subgroup_indicator,
          MARGIN = 2,
          FUN = mean,
          simplify = FALSE
        )
        subgroup_prob <- unlist(subgroup_prob)
        names(subgroup_prob) <- get_element(pe, "name")
        out[["subgroup_prob"]] <- subgroup_prob
      }

      return(out)
    })

    insample_sigma2 <- lapply(insample_parameters, function(x) get_element(x, "sigma2"))
    insample_sigma2 <- unlist(insample_sigma2)

    insample_subgroup_prob <- lapply(insample_parameters, function(x) get_element(x, "subgroup_prob", check_name = FALSE))
    insample_subgroup_prob <- unlist(insample_subgroup_prob)

    out <- valid_pe
    out[["insample_sigma2"]] <- insample_sigma2
    out[["insample_subgroup_prob"]] <- insample_subgroup_prob

    return(out)
  })

  target <- get_element(args, "target")
  if (target == "value"){
    online_onestep_terms <- lapply(sequential_fits, function(x){
      IC <- get_element(x, "IC")
      coef <- get_element(x, "coef")
      Z <- apply(IC, MARGIN = 1, function(x) x + coef, simplify = FALSE)
      Z <- do.call(what = "rbind", Z)

      sigma <- unname(sqrt(get_element(x, "insample_sigma2")))

      scaled_Z <- apply(Z, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      scaled_Z <- do.call(what = "rbind", scaled_Z)

      Gamma <- Z * 0 + 1
      Gamma <- apply(Gamma, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      Gamma <- do.call(what = "rbind", Gamma)

      out <- list(scaled_Z = scaled_Z, Gamma = Gamma)
      return(out)
    })

    scaled_Z <- lapply(online_onestep_terms, function(x) get_element(x, "scaled_Z"))
    scaled_Z <- do.call(what = "rbind", scaled_Z)

    Gamma <- lapply(online_onestep_terms, function(x) get_element(x, "Gamma"))
    Gamma <- do.call(what = "rbind", Gamma)

    coef <- colSums(scaled_Z) / colSums(Gamma)
    vcov <- colMeans(Gamma)^(-2)/nrow(Gamma)
  } else if (target == "subgroup") {
    online_onestep_terms <- lapply(sequential_fits, function(x){
      subgroup_indicator <- get_element(x, "subgroup_indicator")
      Z <- get_element(x, "Z")
      subgroup_prob <- get_element(x, "insample_subgroup_prob")
      sigma <- unname(sqrt(get_element(x, "insample_sigma2")))

      blip <- Z[, 2] - Z[, 1]
      D <- apply(
        subgroup_indicator,
        MARGIN = 2,
        FUN = function(si){
          blip * si
        },
        simplify = FALSE
      )
      D <- do.call(what = "cbind", D)
      D <- apply(D, MARGIN = 1, function(x) x / subgroup_prob, simplify = FALSE)
      D <- do.call(what = "rbind", D)

      scaled_D <- apply(D, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      scaled_D <- do.call(what = "rbind", scaled_D)

      Gamma <- D * 0 + 1
      Gamma <- apply(Gamma, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      Gamma <- do.call(what = "rbind", Gamma)

      out <- list(scaled_D = scaled_D, Gamma = Gamma)
      return(out)
    })

    scaled_D <- lapply(online_onestep_terms, function(x) get_element(x, "scaled_D"))
    scaled_D <- do.call(what = "rbind", scaled_D)

    Gamma <- lapply(online_onestep_terms, function(x) get_element(x, "Gamma"))
    Gamma <- do.call(what = "rbind", Gamma)

    coef <- colSums(scaled_D) / colSums(Gamma)
    vcov <- colMeans(Gamma)^(-2)/nrow(Gamma)

  } else {
    mes <- "policy_eval_online only implemented for target = 'value' or 'subgroup'."
    stop(mes)
  }

  out <- policy_eval_online_object(
    coef = coef,
    vcov = vcov,
    type = get_element(args, "type"),
    target = target,
    id = id,
    train_sequential_index = unname(train_sequential_index),
    valid_sequential_index = unname(valid_sequential_index),
    name = get_element(sequential_fits[[1]], "name")
  )

  return(out)
}

policy_eval_online_object <- function(coef,
                                      vcov,
                                      type,
                                      target,
                                      id,
                                      name,
                                      train_sequential_index,
                                      valid_sequential_index) {
  names(coef) <- name
  out <- as.list(environment())
  out <- remove_null_elements(out)
  class(out) <- c("policy_eval_online", "policy_eval")

  return(out)
}

#' @rdname policy_eval
#' @export
vcov.policy_eval_online <- function(object, ...) {
  vcov <- get_element(object, "vcov")
  tmp <- matrix(NA, ncol = length(vcov), nrow = length(vcov))
  diag(tmp) <- vcov
  vcov <- tmp
  return(vcov)
}
