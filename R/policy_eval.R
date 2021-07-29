##' @export
coef.policy_eval <- function(object, ...) {
  object$value_estimate
}

##' @export
iid.policy_eval <- function(x, ...) {
  res <- cbind(x$iid)
  res/nrow(res)
}

##' @export
vcov.policy_eval <- function(object, ...) {
  crossprod(iid(a))
}

##' @export
print.policy_eval <- function(x, ...) {
  print(estimate(x, ...))
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
  if (type%in%c("cv", "crossfit", "cf", "cv_dr")) {
    val <- policy_evaluation_cv_dr(policy_data,
                                   policy=policy, policy_learner=policy_learner,
                                   g_models=g_models, g_functions = g_functions, g_full_history=g_full_history,
                                   q_models=q_models, q_functions = q_functions, q_full_history=q_full_history,
                                   M=M)
  }
  if (type%in%c("dr")) {
    val <- policy_evaluation_dr(policy_data,
                                policy=policy, policy_learner = policy_learner,
                                q_models=q_models, q_functions=q_functions, q_full_history=q_full_history,
                                g_models=g_models, g_functions=g_functions, g_full_history=g_full_history, ...)
  }
  if (type%in%c("or","q")) {
    val <- policy_evaluation_or(policy_data, policy=policy,
                                q_models=q_models, q_functions=q_functions, q_full_history=q_full_history)
  }
  if (type%in%c("ipw")) {
    val <- policy_evaluation_ipw(policy_data, policy=policy,
                                 g_models=g_models, g_functions=g_functions, g_full_history=g_full_history)
  }
  return(val)
}


policy_evaluation_dr_fold <- function(fold,
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
  train_pe_dr <- do.call(what = "policy_evaluation_dr", args = train_args)

  # getting the policy:
  if (is.null(policy))
    policy <- get_policy(train_pe_dr$policy_object)

  validation_args <- list(policy_data = validation_policy_data,
                          policy = policy,
                          g_functions = train_pe_dr$g_functions,
                          q_functions = train_pe_dr$q_functions)
  validation_pe_dr <- do.call(what = "policy_evaluation_dr", args = validation_args)

  return(validation_pe_dr)
}

policy_evaluation_cv_dr <- function(policy_data,
                                    policy = NULL, policy_learner = NULL,
                                    g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                                    q_models = NULL, q_functions = NULL, q_full_history = FALSE,
                                    M, ...){

  n <- get_n(policy_data)
  id <- get_id(policy_data)

  # setting up the folds
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))

  dotdotdot <- list(...)

  pe_dr_cv <- lapply(
    folds,
    FUN = policy_evaluation_dr_fold,
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

policy_evaluation_dr <- function(policy_data,
                                 policy = NULL, policy_learner = NULL,
                                 g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                                 q_models = NULL, q_functions = NULL, q_full_history = FALSE, ...){

  if ((is.null(g_models) & is.null(g_functions)) | (!is.null(g_functions) & !is.null(g_models))) stop("Provide either g-models or g-functions.")
  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
  }

  if ((is.null(q_models) & is.null(q_functions)) | (!is.null(q_models) & !is.null(q_functions))) stop("Provide either q-models or q-functions.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }

  if ((is.null(policy) & is.null(policy_learner)) | (!is.null(policy_learner) & !is.null(policy))) stop("Provide either policy or policy_learner.")

  # getting the number of stages:
  K <- get_K(policy_data)

  # getting the action set:
  action_set <- get_action_set(policy_data)

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(policy_data, models = g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data)

  # learning the policy:
  policy_object <- NULL
  if (is.null(policy)){
    policy_object <- policy_learner(
      policy_data = policy_data,
      g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
      q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
      ...
    )
    policy <- get_policy(policy_object)
  }

  # getting the policy actions:
  policy_actions <- policy(policy_data = policy_data)

  # fitting the Q-functions:
  if(is.null(q_functions)){
    if(!is.null(policy_object$q_functions)){
      q_functions <- policy_object$q_functions
    } else{
      q_functions <- fit_Q_functions(policy_data,
                                     policy_actions = policy_actions,
                                     q_models = q_models, full_history = q_full_history)
    }
  }

  # getting the Q-function values:
  q_values <- evaluate(q_functions, policy_data = policy_data)

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  IId <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  II <- (IIA == IId); rm(IIA, IId)
  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = g_values)
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  # (n) vector with entries U_i:
  U <- utility(policy_data)$U
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)
  Q <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  Q <- apply(
    Q,
    MARGIN = 2,
    function(v){
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  Q <- cbind(Q, U)

  # calculating the doubly robust score
  Zd <- Q[, 1]
  for (k in 1:K){
    Zd <- Zd + ipw_weight(II[,1:k], G = G[,1:k]) * (Q[,k+1] - Q[,k])
  }

  # getting the IPW and OR Z-score
  Zd_ipw <- ipw_weight(II, G = G) * U
  Zd_or <- Q[, 1]

  out <- list(
    value_estimate = mean(Zd),
    iid=Zd-mean(Zd),
    Zd = Zd,
    value_estimate_ipw = mean(Zd_ipw),
    value_estimate_or = mean(Zd_or),
    g_functions = g_functions,
    q_functions = q_functions,
    id = get_id(policy_data),
    policy_object = policy_object
  )
  class(out) <- c("policy_eval_dr", "policy_eval")
  return(out)
}


policy_evaluation_or <- function(policy_data, q_models = NULL, q_functions = NULL, policy, q_full_history = FALSE){
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }

  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  # getting the policy actions
  policy_actions <- policy(policy_data = policy_data)

  # fitting the q-functions
  if (!is.null(q_models)){
    q_functions <- fit_Q_functions(policy_data, policy_actions = policy_actions, q_models = q_models, full_history = q_full_history)
  }
  q_values <- evaluate(q_functions, policy_data = policy_data)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)

  # (n X K) matrix with entries Q_k(d_k(H_k), H_k)
  Q <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_or <- Q[, 1]
  out <- list(
    value_estimate = mean(phi_or),
    iid=phi_or-mean(phi_or),
    q_functions = q_functions,
    id = get_id(policy_data)
  )
  class(out) <- c("policy_eval_or", "policy_eval")
  return(out)
}

policy_evaluation_ipw <- function(policy_data, g_models = NULL, g_functions = NULL, policy, g_full_history = FALSE){
  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")

  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(policy_data, models = g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data)
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, g_values)

  # (n) vector with entries U_i:
  U <- utility(policy_data)$U

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  IIA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  IId <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  II <- (IIA == IId); rm(IIA, IId)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_ipw <- ipw_weight(II, G = G) * U

  out <- list(
    value_estimate = mean(phi_ipw),
    iid=phi_ipw-mean(phi_ipw),
    g_functions = g_functions,
    id = get_id(policy_data)
  )
  class(out) <- c("policy_eval_ipw", "policy_eval")
  return(out)
}
