#' @title Control arguments for Efficient Augmentation and Relaxation Learning
#' @description \code{control_earl} sets the default control arguments
#' for efficient augmentation and relaxation learning , \code{type = "earl"}.
#' The arguments are passed directly to [DynTxRegime::earl()] if not
#' specified otherwise.
#' @param moPropen Propensity model of class "ModelObj", see [modelObj::modelObj].
#' @param moMain Main effects outcome model of class "ModelObj".
#' @param moCont Contrast outcome model of class "ModelObj".
#' @param regime An object of class [formula] specifying the design of the policy/regime.
#' @param iter Maximum number of iterations for outcome regression.
#' @param fSet A function or NULL defining subset structure.
#' @param lambdas Numeric or numeric vector. Penalty parameter.
#' @param cvFolds Integer. Number of folds for cross-validation of the parameters.
#' @param surrogate The surrogate 0-1 loss function. The options are
#' \code{"logit"}, \code{"exp"}, \code{"hinge"}, \code{"sqhinge"}, \code{"huber"}.
#' @param kernel The options are \code{"linear"}, \code{"poly"}, \code{"radial"}.
#' @param kparam Numeric. Kernel parameter
#' @param verbose Integer.
#' @returns list of (default) control arguments.
#' @export
control_earl <- function(moPropen,
                         moMain,
                         moCont,
                         regime,
                         iter = 0L,
                         fSet = NULL,
                         lambdas = 0.5,
                         cvFolds = 0L,
                         surrogate = "hinge",
                         kernel = "linear",
                         kparam = NULL,
                         verbose = 0L){
  control <- as.list(environment())
  return(control)
}

dyntxregime_earl <- function(policy_data,
                             alpha,
                             L,
                             moPropen,
                             moMain,
                             moCont,
                             regime,
                             iter,
                             fSet,
                             lambdas,
                             cvFolds,
                             surrogate,
                             kernel,
                             kparam,
                             verbose,
                             ...){
  K <- get_K(policy_data)
  if (K != 1)
    stop("earl is only implemented for single stage problems.")
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  id_stage <- get_id_stage(policy_data)

  if (!(length(action_set) == 2))
    stop("earl only works for binary actions.")

  if (alpha != 0)
    stop("alpha must be 0 when using earl")

  if (L != 1)
    stop("L must be 1 when using earl (no cross-fitting)")

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  H <- get_H(get_history(policy_data))

  A <- actions[["A"]]
  H <- cbind(A = A, H)


  earl_object <- DynTxRegime::earl(moPropen = moPropen,
                                   moMain = moMain,
                                   moCont = moCont,
                                   data = H,
                                   response = utility[["U"]],
                                   txName = "A",
                                   regime = regime,
                                   iter = iter,
                                   fSet = fSet,
                                   lambdas = lambdas,
                                   cvFolds = cvFolds,
                                   surrogate = surrogate,
                                   kernel = kernel,
                                   kparam = kparam,
                                   verbose = verbose)


  out <- list(
    earl_object = earl_object,
    action_set = action_set,
    K = K
  )
  class(out) <- "EARL"

  return(out)
}

#' @export
get_policy.EARL <- function(object){
  earl_object <- getElement(object, "earl_object")
  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")

    H <- get_H(get_history(policy_data))

    pred <- DynTxRegime::optTx(earl_object, H)
    policy_actions <- get_id_stage(policy_data)
    d <- NULL
    policy_actions[, d := pred$optimalTx]
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }
  class(policy) <- c("policy", "function")
  return(policy)
}
