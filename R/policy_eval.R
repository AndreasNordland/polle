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
policy_eval <- function(policy_data, policy,
                        g_models=NULL, q_models=NULL,
                        g_functions=NULL, q_functions=NULL,
                        g_full_history = FALSE, q_full_history = FALSE,
                        M=5, type="dr", ...) {
  type <- tolower(type)
  if (type%in%c("cv", "crossfit", "cv_dr")) {
    val <- policy_evaluation_cv_dr(policy_data, policy=policy, M=M,
                                   q_models=q_models, q_functions=q_functions, q_full_history=q_full_history,
                                   g_models=g_models, g_functions=g_functions, g_full_history=g_full_history)
  }
  if (type%in%c("dr")) {
    val <- policy_evaluation_dr(policy_data, policy=policy,
                                q_models=q_models, q_functions=q_functions, q_full_history=q_full_history,
                                g_models=g_models, g_functions=g_functions, g_full_history=g_full_history)
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


dr_fold <- function(fold, id, policy_data, policy, g_models, q_models, g_full_history, q_full_history){
  K <- policy_data$dim$K
  train_id <- id[-fold]
  validation_id <- id[fold]

  train_policy_data <- new_policy_data(
    stage_data = policy_data$stage_data[id %in% train_id],
    baseline_data = policy_data$baseline_data[id %in% train_id]
  )
  validation_policy_data <- new_policy_data(
    stage_data = policy_data$stage_data[id %in% validation_id],
    baseline_data = policy_data$baseline_data[id %in% validation_id]
  )
  if (train_policy_data$dim$K != K) stop("The maximal number of stages K in the training policy data does not equal the original policy data.")
  if (validation_policy_data$dim$K != K) stop("The maximal number of stages K in the validation policy data does not equal the original policy data.")

  train_dr <- dr(
    train_policy_data,
    policy = policy,
    g_models = g_models,
    q_models = q_models,
    g_full_history = g_full_history,
    q_full_history = q_full_history
  )

  validation_dr <- dr(
    validation_policy_data,
    policy = policy,
    g_functions = train_dr$g_functions,
    q_functions = train_dr$q_functions
  )

  return(validation_dr)
}

policy_evaluation_cv_dr <- function(policy_data, M, g_models, q_models, policy, g_full_history = FALSE, q_full_history = FALSE, ...){
  n <- policy_data$dim$n
  id <- get_id(policy_data)

  # setting up the folds
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))

  dr_list <- parallel::mclapply(
    folds,
    FUN = dr_fold,
    ...,
    id = id,
    policy_data = policy_data,
    policy = policy,
    g_models = g_models,
    q_models = q_models,
    g_full_history = g_full_history,
    q_full_history = q_full_history
    )

  id <- unlist(lapply(dr_list, function(dr) getElement(dr, "id")))
  phi_dr <- unlist(lapply(dr_list, function(dr) getElement(dr, "phi_dr")))
  phi_ipw <- unlist(lapply(dr_list, function(dr) getElement(dr, "phi_ipw")))
  phi_or <- unlist(lapply(dr_list, function(dr) getElement(dr, "phi_or")))

  phi_dr <- phi_dr[order(id)]
  phi_ipw <- phi_ipw[order(id)]
  phi_or <- phi_or[order(id)]

  id <- id[order(id)]

  out <- list(
    id = id,
    value_estimate = mean(phi_dr),
    iid=phi_dr-mean(phi_dr),
    phi_ipw = phi_ipw,
    phi_or = phi_or,
    dr_list = dr_list
  )
  class(out) <- c("policy_eval_cv_dr", "policy_eval")
  return(out)
}

policy_evaluation_dr <- function(policy_data, g_models = NULL, g_functions = NULL, q_models = NULL, q_functions = NULL, policy, g_full_history = FALSE, q_full_history = FALSE){
  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")
  if (is.null(q_models) & is.null(q_functions)) stop("Either q-models or q-functions must be provided.")
  if (!is.null(q_models) & !is.null(q_functions)) stop("q-models and q-functions can not both be provided.")
  if (!is.null(q_functions)){
    if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  }
  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
  }

  K <- policy_data$dim$K
  action_set <- policy_data$action_set

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the policy actions:
  policy_actions <- policy(policy_data = policy_data)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(policy_data, models = g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data)
  g_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = g_values)

  # fitting the Q-functions:
  if (!is.null(q_models)){
    q_functions <- fit_Q_functions(policy_data, policy_actions = policy_actions, q_models = q_models, full_history = q_full_history)
  }
  q_values <- evaluate(q_functions, policy_data = policy_data)
  q_d_values <- get_a_values(a = policy_actions$d, action_set = action_set, values = q_values)

  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  DA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  Dd <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  D <- (DA == Dd); rm(DA, Dd)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  # (n) vector with entries U_i:
  U <- utility(policy_data)$U

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  V <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])
  V <- apply(
    V,
    MARGIN = 2,
    function(v){
      v[is.na(v)] <- U[is.na(v)]
      return(v)
    }
  )
  V <- cbind(V, U)

  # calculating the doubly robust score
  phi_dr <- V[, 1]
  for (k in 1:K){
    phi_dr <- phi_dr + ipw_weight(D = D[,1:k], G = G[,1:k]) * (V[,k+1] - V[,k])
  }

  # getting the IPW and OR score
  phi_ipw <- ipw_weight(D = D, G = G) * U
  phi_or <- V[, 1]

  out <- list(
    value_estimate = mean(phi_dr),
    iid=phi_dr-mean(phi_dr),
    phi_ipw = phi_ipw,
    phi_or = phi_or,
    g_functions = g_functions,
    q_functions = q_functions,
    id = get_id(policy_data)
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
  V <- as.matrix(dcast(q_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_or <- V[, 1]
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
  DA <- as.matrix(dcast(actions, id ~ stage, value.var = "A")[, -c("id"), with = FALSE])
  Dd <- as.matrix(dcast(policy_actions, id ~ stage, value.var = "d")[, -c("id"), with = FALSE])
  D <- (DA == Dd); rm(DA, Dd)

  # (n X K) matrix with entries g_k(d_k(H_k), H_k):
  G <- as.matrix(dcast(g_d_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  phi_ipw <- ipw_weight(D = D, G = G) * U

  out <- list(
    value_estimate = mean(phi_ipw),
    iid=phi_ipw-mean(phi_ipw),
    g_functions = g_functions,
    id = get_id(policy_data)
  )
  class(out) <- c("policy_eval_ipw", "policy_eval")
  return(out)
}
