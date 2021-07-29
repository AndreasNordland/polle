#' @export
bowl <- function(policy_data,
                 alpha = 0,
                 g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                 policy_full_history = FALSE, policy_vars = NULL, ...){

  if (is.null(g_models) & is.null(g_functions)) stop("Either g-models or g-functions must be provided.")
  if (!is.null(g_functions) & !is.null(g_models)) stop("g-models and g-functions can not both be provided.")
  if (!is.null(g_functions)){
    if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
  }

  K <- policy_data$dim$K
  n <- policy_data$dim$n
  action_set <- policy_data$action_set
  if (policy_full_history == TRUE){
    if ((!is.list(policy_vars)) | (length(policy_vars) != K)) stop("policy_vars must be a list of length K, when policy_full_history = TRUE")
  }

  if (!(length(action_set) == 2)) stop("bowl only works for binary actions.")

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # fitting the g-functions:
  if (!is.null(g_models)){
    g_functions <- fit_g_functions(policy_data = policy_data, models = g_models, full_history = g_full_history)
  }
  g_values <- evaluate(g_functions, policy_data = policy_data)
  g_A_values <- get_a_values(a = actions$A, action_set = action_set, g_values)

  # getting the IDs and the observed (complete) utility
  utility <- utility(policy_data)
  id <- utility$id

  # (n) vector with entries U_i:
  U <- utility$U

  # (n X K+1) matrix with entries I(d_k(H_k) = A_k) for k <= K:
  D <- matrix(nrow = n, ncol = K+1)
  D[, K+1] <- TRUE

  # (n X K) matrix with entries g_k(A_k, H_k):
  G <- as.matrix(dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE])

  g_cols <- paste("g_", action_set, sep = "")
  X_scales <- list()
  owl_objects <- list()
  for (k in K:1){
    # getting the policy history for stage k
    policy_history_k <- get_stage_history(policy_data, stage = k, full_history = policy_full_history)

    # getting the IDs and ID-Indices
    id_k <- policy_history_k$H$id
    idx_k <- (id %in% id_k)

    # constructing the inputs for owl
    if (policy_full_history == TRUE)
      vars <- policy_vars[[k]]
    else
      vars <- policy_vars
    X <- get_X(policy_history_k, vars = vars)
    X <- scale(X)

    X_scales[[k]] <- attributes(X)[3:4]

    AA <- A <- get_A(policy_history_k)
    AA[A == action_set[1]] <- -1
    AA[A == action_set[2]] <- 1
    AA <- as.numeric(AA)

    RR <- merge(policy_history_k$U, utility, all.x = TRUE)
    RR <- (RR$U - RR$U_bar)
    RR <- RR * colprod(D[idx_k, (k+1):(K+1)])

    pi <- colprod(G[idx_k , k:K])

    if ((ncol(X) == 1))
      stop("DTRlearn2 has a bug. H must be a matrix with ncol(H) > 1.")
    owl_objects[[k]] <- DTRlearn2::owl(H = X, AA = AA, RR = RR, pi = pi, K = 1, n = nrow(X), ...)

    dd <- owl_objects[[k]]$stage$treatment

    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # modifying the policy to only recommend realistic actions
      dd[(g_values_k[, g_cols[1], with = FALSE] < alpha)] <- action_set[2]
      dd[(g_values_k[, g_cols[2], with = FALSE] < alpha)] <- action_set[1]
    }

    D[idx_k, k] <- (AA == dd)

  }

  out <- list(
    owl_objects = owl_objects,
    X_scales = X_scales,
    g_functions = g_functions,
    policy_full_history = policy_full_history,
    policy_vars = policy_vars,
    alpha = alpha,
    action_set = action_set,
    K = K
  )
  class(out) <- "BOWL"

  return(out)
}

#' @export
get_policy.BOWL <- function(object){
  owl_objects <- object$owl_objects
  X_scales <- object$X_scales
  g_functions = object$g_functions
  policy_full_history <- object$policy_full_history
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
      policy_history_k <- get_stage_history(policy_data, stage = k, full_history = policy_full_history)

      if (policy_full_history == TRUE)
        vars <- policy_vars[[k]]
      else
        vars <- policy_vars
      X <- get_X(policy_history_k, vars = vars)
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
