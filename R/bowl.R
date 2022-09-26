bowl <- function(policy_data,
                 alpha = 0,
                 g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                 full_history = FALSE, policy_vars = NULL,
                 res.lasso=TRUE, loss='hinge', kernel='linear',
                 augment=FALSE, c=2^(-2:2), sigma=c(0.03,0.05,0.07), s=2.^(-2:2), m=4,
                 ...){

  if ((is.null(g_models) & is.null(g_functions))) stop("Provide either g-models or g-functions.")

  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions"))
      stop("g-functions must be of class 'nuisance_functions'.")
  }

  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)

  if (full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K)) stop("policy_vars must be a list of length K, when full_history = TRUE")
  }

  if (!(length(action_set) == 2)) stop("bowl only works for binary actions.")

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- utility(policy_data)

  # fitting the g-functions:
  if (is.null(g_functions)){
    g_functions <- fit_g_functions(policy_data,
                                   g_models = g_models,
                                   full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K+1) matrix with entries I(d_k(H_k) = A_k) for k <= K:
  II <- matrix(nrow = n, ncol = K+1)
  II[, K+1] <- TRUE

  # (n X K) matrix with entries g_k(A_k, H_k):
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  g_cols <- paste("g_", action_set, sep = "")
  X_scales <- list()
  owl_objects <- list()
  for (k in K:1){
    # getting the policy history for stage k
    policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)

    # getting the IDs and ID-Indices at the kth stage:
    id_k <- get_id(policy_history_k)
    idx_k <- (id %in% id_k)

    # constructing the inputs for owl:
    if (full_history == TRUE)
      vars <- policy_vars[[k]]
    else
      vars <- policy_vars
    X <- get_H(policy_history_k, vars = vars)
    X <- scale(X)

    X_scales[[k]] <- attributes(X)[3:4]

    AA <- A <- get_A(policy_history_k)
    AA[A == action_set[1]] <- -1
    AA[A == action_set[2]] <- 1
    AA <- as.numeric(AA)

    RR <- merge(policy_history_k$U, utility, all.x = TRUE)
    RR <- (RR$U - RR$U_bar)
    RR <- RR * colprod(II[idx_k, (k+1):(K+1)])

    pi <- colprod(G[idx_k , k:K])

    if ((ncol(X) == 1))
      stop("DTRlearn2 has a bug. H must be a matrix with ncol(H) > 1.")
    owl_objects[[k]] <- DTRlearn2::owl(H = X, AA = AA, RR = RR, pi = pi, K = 1,
                                       n = nrow(X), res.lasso=res.lasso,
                                       loss=loss, kernel=kernel,
                                       augment=augment, c=c, sigma=sigma, s=s, m=m)

    dd <- owl_objects[[k]]$stage$treatment

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # modifying the policy to only recommend realistic actions
      dd[(g_values_k[, g_cols[1], with = FALSE] < alpha)] <- action_set[2]
      dd[(g_values_k[, g_cols[2], with = FALSE] < alpha)] <- action_set[1]
    }

    II[idx_k, k] <- (AA == dd)

  }

  names(owl_objects) <- paste("stage_", 1:K, sep = "")

  out <- list(
    owl_objects = owl_objects,
    X_scales = X_scales,
    g_functions = g_functions,
    full_history = full_history,
    policy_vars = policy_vars,
    alpha = alpha,
    action_set = action_set,
    K = K
  )
  class(out) <- "BOWL"

  return(out)
}

get_policy.BOWL <- function(object){
  owl_objects <- object$owl_objects
  X_scales <- object$X_scales
  g_functions = object$g_functions
  full_history <- object$full_history
  policy_vars <- getElement(object, "policy_vars")
  alpha <- object$alpha
  action_set <- object$action_set
  K <- object$K

  policy <- function(policy_data){
    if (policy_data$dim$K != K) stop("The policy do not have the same number of stages as the policy data object.")

    # getting the actions recommended by the OWL objects:
    policy_actions <- list()
    for (k in K:1){
      # getting the policy history:
      policy_history_k <- get_history(policy_data, stage = k, full_history = full_history)

      if (full_history == TRUE)
        vars <- policy_vars[[k]]
      else
        vars <- policy_vars
      X <- get_H(policy_history_k, vars = vars)
      X <- scale(X, center = X_scales[[k]]$`scaled:center`, scale = X_scales[[k]]$`scaled:scale`)

      dd <- d <- predict(owl_objects[[k]], H = X, K = 1)$treatment[[1]]
      d[dd == -1] <- action_set[1]
      d[dd == 1] <- action_set[2]

      pa <- get_id_stage(policy_history_k)
      pa[, d:= d]
      policy_actions[[k]] <- pa
      rm(pa, d, dd)
    }
    policy_actions <- rbindlist(policy_actions)
    setkey(policy_actions, id, stage)

    # excluding unrealistic recommendations:
    if (alpha != 0){
      g_cols <- paste("g_", action_set, sep = "")
      # evaluating the g-functions:
      g_values <- evaluate(g_functions, policy_data = policy_data)

      d_ <- policy_actions$d
      d_[(g_values[, g_cols[1], with = FALSE] < alpha)] <- action_set[2]
      d_[(g_values[, g_cols[2], with = FALSE] < alpha)] <- action_set[1]
    } else{
      d_ <- policy_actions$d
    }

    # inserting the modified actions:
    policy_actions[, d:= d_]

    return(policy_actions)
  }

  return(policy)
}
