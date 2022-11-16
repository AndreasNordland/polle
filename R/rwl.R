dyntxregime_rwl <- function(policy_data,
                            alpha,
                            L,
                            moPropen,
                            moMain,
                            regime,
                            fSet = NULL,
                            lambdas = 2,
                            cvFolds = 0L,
                            kernel = "linear",
                            kparam = NULL,
                            responseType = "continuous",
                            verbose = 0,
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
    stop("alpha must be 0 when using rwl")

  if (L != 1)
    stop("L must be 1 when using rwl (no cross-fitting)")

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

    pred <- optTx(rwl_object, H)
    policy_actions <- get_id_stage(policy_data)
    d <- NULL
    policy_actions[, d := pred$optimalTx]
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }
  class(policy) <- c("policy", "function")
  return(policy)
}
