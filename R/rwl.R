#' @title Control arguments for Residual Weighted Learning
#' @description \code{control_rwl} sets the default control arguments
#' for residual learning , \code{type = "rwl"}.
#' The arguments are passed directly to [DynTxRegime::rwl()] if not
#' specified otherwise.
#' @param moPropen Propensity model of class "ModelObj", see [modelObj::modelObj].
#' @param moMain Main effects outcome model of class "ModelObj".
#' @param regime An object of class [formula] specifying the design of the policy/regime.
#' @param fSet A function or NULL defining subset structure.
#' @param lambdas Numeric or numeric vector. Penalty parameter.
#' @param cvFolds Integer. Number of folds for cross-validation of the parameters.
#' \code{"logit"}, \code{"exp"}, \code{"hinge"}, \code{"sqhinge"}, \code{"huber"}.
#' @param kernel The options are \code{"linear"}, \code{"poly"}, \code{"radial"}.
#' @param kparam Numeric. Kernel parameter
#' @param responseType Character string. Options are \code{"continuous"},
#' \code{"binary"}, \code{"count"}.
#' @param verbose Integer.
#' @returns list of (default) control arguments.
#' @export
control_rwl <- function(moPropen,
                        moMain,
                        regime,
                        fSet = NULL,
                        lambdas = 2,
                        cvFolds = 0L,
                        kernel = "linear",
                        kparam = NULL,
                        responseType = "continuous",
                        verbose = 2L){
  control <- as.list(environment())
  return(control)
}

dyntxregime_rwl <- function(policy_data,
                            alpha,
                            L,
                            moPropen,
                            moMain,
                            regime,
                            fSet,
                            lambdas,
                            cvFolds,
                            kernel,
                            kparam,
                            responseType,
                            verbose,
                            ...){
  K <- get_K(policy_data)
  if (K != 1)
    stop("rwl is only implemented for single stage problems.")
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  id_stage <- get_id_stage(policy_data)

  if (!(length(action_set) == 2))
    stop("rwl only works for binary actions.")

  if (alpha != 0)
    stop("alpha must be 0 when using rwl.")

  if (L != 1)
    stop("L must be 1 when using rwl (no cross-fitting).")

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  H <- get_H(get_history(policy_data))
  A <- actions[["A"]]
  H <- cbind(A = A, H)

  rwl_object <- DynTxRegime::rwl(moPropen = moPropen,
                                 moMain = moMain,
                                 data = H,
                                 response = utility[["U"]],
                                 txName = "A",
                                 regime = regime,
                                 fSet = fSet,
                                 lambdas = lambdas,
                                 cvFolds = cvFolds,
                                 kernel = kernel,
                                 kparam = kparam,
                                 responseType = responseType,
                                 verbose = verbose)


  out <- list(
    rwl_object = rwl_object,
    action_set = action_set,
    K = K
  )
  class(out) <- "RWL"

  return(out)
}

#' @export
get_policy.RWL <- function(object){
  rwl_object <- getElement(object, "rwl_object")
  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")

    H <- get_H(get_history(policy_data))

    pred <- DynTxRegime::optTx(rwl_object, H)
    policy_actions <- get_id_stage(policy_data)
    d <- NULL
    policy_actions[, d := pred$optimalTx]
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = "rwl")

  return(policy)
}
