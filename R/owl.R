#' @title Control arguments for Outcome Weighted Learning
#' @description \code{control_owl()} sets the default control arguments
#' for backwards outcome weighted learning, \code{type = "owl"}.
#' The arguments are passed directly to [DTRlearn2::owl()] if not
#' specified otherwise.
#' @param policy_vars Character vector/string or list of character
#' vectors/strings. Variable names used to restrict the policy.
#' The names must be a subset of the history names, see get_history_names().
#' Not passed to \code{owl()}.
#' @param reuse_scales The history matrix passed to \code{owl()} is scaled
#' using [scale()] as advised. If \code{TRUE}, the scales of the history matrix
#' will be saved and reused when applied to (new) test data.
#' @param res.lasso If \code{TRUE} a lasso penalty is applied.
#' @param loss Loss function. The options are \code{"hinge"}, \code{"ramp"},
#' \code{"logit"}, \code{"logit.lasso"}, \code{"l2"}, \code{"l2.lasso"}.
#' @param kernel Type of kernel used by the support vector machine. The
#' options are \code{"linear"}, \code{"rbf"}.
#' @param augment If \code{TRUE} the outcomes are augmented.
#' @param c Regularization parameter.
#' @param sigma Tuning parameter.
#' @param s Slope parameter.
#' @param m Number of folds for cross-validation of the parameters.
#' @param solver Quadratic-programming backend used by
#' [DTRlearn2::owl()] for the weighted SVM when \code{loss = "hinge"}.
#' The default \code{"ipop"} uses [kernlab::ipop()] and reproduces the
#' behaviour of \code{DTRlearn2} prior to version 2.1: the fitted decision
#' function is exact and invariant to the order of the observations.
#' \code{"svm"} uses \code{WeightSVM::wsvm} (the default in
#' \code{DTRlearn2 (>= 2.1)}) and is substantially faster, but as of
#' \code{DTRlearn2 2.1} the orientation of the fitted decision function
#' depends on the order in which the observations appear, so
#' \code{policy_learn(type = "owl")} may return a partially reversed
#' policy. \code{"svm"} therefore is not recommended until this is
#' fixed upstream. Note that \code{DTRlearn2::owl()} does not forward
#' \code{solver} to its augmented path, so \code{solver} has no effect
#' when \code{augment = TRUE}. For large samples the \code{"ipop"} solver
#' can be slow (each QP scales super-linearly in \eqn{n}, and
#' \code{owl_single} performs \code{m * length(c)} QP solves per stage);
#' reducing \code{c} and/or \code{m} may be worthwhile.
#' @returns list of (default) control arguments.
#' @export
control_owl <- function(policy_vars = NULL,
                        reuse_scales = TRUE,
                        res.lasso = TRUE,
                        loss = 'hinge',
                        kernel = 'linear',
                        augment = FALSE,
                        c = 2^(-2:2),
                        sigma = c(0.03,0.05,0.07),
                        s = 2.^(-2:2),
                        m = 4,
                        solver = "ipop"){
  control <- as.list(environment())
  return(control)
}

dtrlearn2_owl <- function(policy_data,
                          alpha,
                          g_models, g_functions, g_full_history,
                          policy_vars, full_history,
                          L,
                          cross_fit_g_models, save_cross_fit_models,
                          future_args,
                          reuse_scales,
                          res.lasso, loss, kernel,
                          augment, c, sigma,
                          s, m,
                          solver,
                          ...){

  if ((is.null(g_models) & is.null(g_functions)))
    stop("Provide either g-models or g-functions.")

  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)
  id_stage <- get_id_stage(policy_data)

  if(!(length(unlist(unique(id_stage[,.N, by = "id"][, "N"]))) == 1))
    stop("owl is only implemented for a fixed number of stages.")

  for (k in seq_along(stage_action_sets)){
    if (length(stage_action_sets[[k]]) != 2)
      stop("owl only works for dichotomous stage action sets.")
  }

  if (alpha != 0)
    stop("alpha must be 0 when type = 'owl'")

  if (full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K))
      stop("policy_vars must be a list of length K, when full_history = TRUE.")
  }
  if (any(get_element(policy_data, "cens_indicator")[["indicator"]])){
    stop("policy learning with type 'owl' not implemented under right-censoring/missing outcomes.")
  }

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the rewards
  rewards <- get_rewards(policy_data)

  # constructing the folds for cross-fitting
  if (L > 1){
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
  } else{
    folds <- NULL
  }

  # (cross-)fitting the g-functions:
  g_functions_cf <- NULL
  if (!is.null(folds) & cross_fit_g_models == TRUE){
    g_cf <- crossfit_function(
      policy_data = policy_data,
      fun = fit_g_functions,
      models = g_models,
      full_history = g_full_history,
      folds = folds,
      save_cross_fit_models = save_cross_fit_models,
      future_args = future_args
    )
    g_functions_cf <- getElement(g_cf, "functions")
    g_values <- getElement(g_cf, "values")
    valid_ids <- getElement(g_cf, "valid_ids")
    rm(g_cf)
  } else {
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
    g_values <- predict(g_functions, policy_data)
  }

  # fitting g-functions for determining new realistic actions:
  if (alpha > 0){
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
  } else{
    # g-functions are not saved if alpha == 0:
    g_functions <- NULL
  }

  # (n X K) matrix with entries U_{i,k+1}:
  stage <- NULL
  R <- as.matrix(dcast(rewards[stage != 1],
                       id~stage,
                       value.var = "U")[, -c("id"), with = FALSE])
  rm(stage)

  # (n X K) matrix with entries g_k(A_k, H_k)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  g_cols <- paste("g_", action_set, sep = "")
  X_designs <- list()
  X_scales <- list()
  X <- list()
  AA <- list()
  RR <- list()
  pi <- list()
  for (k in K:1){
    # getting the policy history
    policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)
    if (full_history == TRUE){
      vars <- policy_vars[[k]]
    } else{
      vars <- policy_vars
    }
    H <- get_H(policy_history_k, vars = vars)
    if (is.null(policy_vars)){
      policy_vars <- names(H)
    }

    ### constructing the inputs for owl:
    # getting the design of the history (X) for owl:
    design_k <- get_design(formula = ~., data = H)
    x <- design_k$x
    if ((ncol(x) == 1))
      stop("DTRlearn2 has a bug. H must be a matrix with ncol(H) > 1.")
    design_k$x <- NULL
    X_designs[[k]] <- design_k
    # scaling the history
    x <- scale(x)
    X[[k]] <- x
    X_scales[[k]] <- attributes(x)[c("scaled:center", "scaled:scale")]

    stage_action_set <- stage_action_sets[[k]]
    # formatting the actions as {-1, 1}:
    aa <- A <- get_A(policy_history_k)
    aa[A == stage_action_set[1]] <- -1
    aa[A == stage_action_set[2]] <- 1
    aa <- as.numeric(aa)
    AA[[k]] <- aa

    # setting the rewards:
    RR[[k]] <- R[,k]

    # setting the action probabilities:
    pi[[k]] <- G[,k]
  }

  ## The 'solver' argument is pinned to the value chosen in control_owl()
  ## (default "ipop"). DTRlearn2 (>= 2.1) introduced solver = "svm" as its
  ## default, which routes the weighted SVM through WeightSVM::wsvm via a
  ## precomputed kernel. In that path DTRlearn2:::wsvm_solve() reconstructs
  ## the dual coefficients as alpha1[solution$index] <- solution$coefs
  ## without applying the LIBSVM sign convention implied by
  ## solution$labels[1], so the orientation of the fitted decision function
  ## depends on the order of the observations (negating AA or reordering
  ## rows does not negate the fit). This corrupts both per-fold CV in
  ## owl_single() and the multi-stage backward induction (which selects the
  ## next-stage training subset via results[[j]]$treatment == AA[[j]]).
  ## "ipop" reproduces the exact, order-invariant behaviour of DTRlearn2
  ## prior to 2.1. See NEWS 1.6.4 and control_owl()'s @param solver.
  owl_object <- DTRlearn2::owl(H = X,
                               AA = AA,
                               RR = RR,
                               pi = pi,
                               K = K,
                               n = n,
                               res.lasso=res.lasso,
                               loss=loss,
                               kernel=kernel,
                               augment=augment,
                               c=c,
                               sigma=sigma,
                               s=s,
                               m=m,
                               solver=solver)

  out <- list(
    owl_object = owl_object,
    reuse_scales = reuse_scales,
    X_scales = X_scales,
    X_designs = X_designs,
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    full_history = full_history,
    policy_vars = policy_vars,
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    K = K
  )
  class(out) <- c("owl","policy_object","list")

  return(out)
}

#' @export
get_policy.owl <- function(object, threshold = NULL){
  if (!(is.null(threshold) || identical(threshold, 0))) {
    stop("threshold is not implemented for owl.")
  }
  owl_object <- getElement(object, "owl_object")
  reuse_scales <- getElement(object, "reuse_scales")
  X_scales <- getElement(object, "X_scales")
  X_designs <- getElement(object, "X_designs")
  g_functions <- getElement(object, "g_functions")
  full_history <- getElement(object, "full_history")
  policy_vars <- getElement(object, "policy_vars")
  stage_action_sets <- getElement(object, "stage_action_sets")
  K <- getElement(object, "K")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")

    id_stage <- get_id_stage(policy_data)
    if(!(length(unlist(unique(id_stage[,.N, by = "id"][, "N"]))) == 1))
      stop("owl is only implemented for a fixed number of stages.")

    X <- list()
    for (k in K:1){
      # getting the policy history:
      policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)

      if (full_history == TRUE){
        vars <- policy_vars[[k]]
      } else{
        vars <- policy_vars
      }
      # getting the design matrix:
      H <- get_H(policy_history_k, vars = vars)
      design <- X_designs[[k]]
      x <- apply_design(design, data = H)
      if (reuse_scales == TRUE){
        x <- scale(x,
                   center = X_scales[[k]]$`scaled:center`,
                   scale = X_scales[[k]]$`scaled:scale`)
      } else{
        x <- scale(x)
      }

      X[[k]] <- x
    }

    pred <- predict(owl_object, H = X, K = K)
    policy_actions <- get_id_stage(policy_data)
    stage <- NULL
    d <- NULL
    for (k in K:1){
      stage_action_set <- stage_action_sets[[k]]
      dd <- d_ <- pred$treatment[[k]]
      d_[dd == -1] <- stage_action_set[1]
      d_[dd == 1] <- stage_action_set[2]
      policy_actions[stage == k, d := d_]
    }
    rm(stage)
    setkeyv(policy_actions, c("id", "stage"))

    return(policy_actions)
  }
  class(policy) <- c("policy", "function")
  return(policy)
}
