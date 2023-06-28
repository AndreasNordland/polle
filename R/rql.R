rql<- function(policy_data, alpha,
               g_models, g_functions, g_full_history,
               q_models, q_full_history,
               L,
               ...){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)

  # input checks:
  if (L != 1)
    stop("L must be 1 when type = 'ql' (no cross-fitting).")
  if (!is.null(g_functions)){
    if(!inherits(g_functions, what = "g_functions"))
      stop("g-functions must be of class 'g_functions'.")
  }
  if (is.list(q_models)){
    if (length(q_models) != K)
      stop("q_models must either be a list of length K or a single Q-model.")
  }

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # (n) vector with entries U_i:
  U <- utility$U

  # fitting the g-functions if alpha > 0:
  if (alpha != 0){
    if (is.null(g_functions)){
      g_functions <- fit_g_functions(policy_data,
                                     g_models = g_models,
                                     full_history = g_full_history)
    }
    g_values <- predict(g_functions, policy_data)
  }

  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  q_cols <- paste("Q_", action_set, sep = "")
  g_cols <- paste("g_", action_set, sep = "")

  q_functions <- list()
  for (k in K:1){
    # fitting the Q-function:
    q_step_k <- q_step(
      policy_data = policy_data,
      k = k,
      full_history = q_full_history,
      Q = Q[, k+1],
      q_models = q_models
    )
    # getting the Q-function, Q-function values and the ID-index:
    q_functions[[k]] <- q_step_k$q_function
    q_values_k <- q_step_k$q_values
    idx_k <- q_step_k$idx_k

    if (alpha != 0){
      # getting the g-function values for each action:
      stage <- NULL
      g_values_k <- g_values[stage == k, ]
      rm(stage)

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values_k[, g_cols, with = FALSE], MARGIN = 1, function(x) x >= alpha))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the maximal realistic Q-function values:
      q_max_realistic_k <- apply(q_values_k[, q_cols, with = FALSE] * realistic_actions, MARGIN = 1, max, na.rm = TRUE)
    } else {
      q_max_realistic_k <- apply(q_values_k[, q_cols, with = FALSE], MARGIN = 1, max, na.rm = TRUE)
    }

    # inserting the maximal Q-function values in Q:
    Q[idx_k, k] <- q_max_realistic_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
  }
  names(q_functions) <- paste("stage_", 1:K, sep = "")
  class(q_functions) <- "nuisance_functions"
  attr(q_functions, "full_history") <- q_full_history

  # checking if deterministic rewards has been applied:
  deterministic_rewards <- policy_data[["colnames"]][["deterministic_rewards"]]
  event <- NULL
  indicator_deterministic_rewards <- !all(policy_data[["stage_data"]][event == 0,deterministic_rewards, with = FALSE] == 0)

  out <- list(
    q_functions = q_functions,
    g_functions = g_functions,
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    alpha = alpha,
    K = K,
    indicator_deterministic_rewards = indicator_deterministic_rewards,
    deterministic_rewards = deterministic_rewards
  )
  out <- remove_null_elements(out)
  class(out) <- c("ql", "policy_object", "list")

  return(out)
}

#' @export
get_policy.ql <- function(object){
  g_functions <- get_g_functions(object)
  q_functions <- get_q_functions(object)
  action_set <- getElement(object, "action_set")
  K <- getElement(object, "K")
  alpha <- getElement(object, "alpha")

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  policy <- function(policy_data){
    # evaluating the Q-functions:
    q_values <- predict(q_functions, policy_data)

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- predict(g_functions, policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(
        g_values[ , g_cols, with = FALSE],
        MARGIN = 1,
        function(x) x >= alpha
      ))
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      idx <- apply(
        q_values[ , q_cols, with = FALSE] * realistic_actions,
        MARGIN = 1,
        which.max
      )
      d <- action_set[idx]
    } else{
      # getting the action with the maximal Q-function value:
      idx <- apply(
        q_values[ , q_cols, with = FALSE],
        MARGIN = 1,
        which.max
      )
      d <- action_set[idx]
    }

    # collecting the policy actions
    policy_actions <- get_id_stage(policy_data)
    policy_actions[, d := d]

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = "ql")

  return(policy)
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.ql <- function(object, stage, include_g_values = FALSE, ...){
  action_set <- getElement(object, "action_set")
  stage_action_sets <- getElement(object, "stage_action_sets")
  stage_action_set <- stage_action_sets[[stage]]; rm(stage_action_sets)
  K <- getElement(object, "K")
  if(!((stage >= 0) & (stage <= K)))
    stop("stage must be smaller than or equal to K.")
  alpha <- getElement(object, "alpha")

  # getting the g-functions:
  g_functions <- getElement(object, "g_functions")
  if (!is.null(g_functions)){
    g_full_history <- attr(g_functions, "full_history")
    if (length(g_functions) == K){
      g_function <- g_functions[[stage]]
    }
    else{
      g_function <- g_functions[[1]]
    }
  }

  # getting the (residual) Q-functions:
  q_functions <- getElement(object, "q_functions")
  q_model <- getElement(q_functions[[stage]], "q_model")
  AH_names <- getElement(q_functions[[stage]], "AH_names")
  full_history <- attr(q_functions, "full_history")

  # getting the names of the deterministic reward variables:
  deterministic_rewards <- getElement(object, "deterministic_rewards")
  indicator_deterministic_rewards <- getElement(object, "indicator_deterministic_rewards")

  stage_policy <- function(H){
    if (is.data.frame(H))
      H <- as.data.table(H)

    # adding zero valued deterministic reward columns if missing
    if (all(!(deterministic_rewards %in% colnames(H)))){
      if (indicator_deterministic_rewards == TRUE)
        warning("deterministic reward variables are missing in 'H' - the default value is set to zero.")
      H[, deterministic_rewards] <- 0
    }
    if (!all(deterministic_rewards %in% colnames(H))){
      tmp <- deterministic_rewards[!(deterministic_rewards %in% colnames(H))]
      H[, tmp] <- 0
      rm(tmp)
    }

    # (residual) Q-model predictions for the stage action set:
    q_values <- sapply(
      stage_action_set,
      function(a) predict(q_model, new_AH = cbind(A = a, H))
    )

    # adding the deterministic rewards
    q_values <- H[, deterministic_rewards[action_set %in% stage_action_set], with = FALSE] + q_values
    colnames(q_values) <- stage_action_set

    # getting
    g_values <- NULL
    if (alpha == 0){
      dd <- apply(q_values,
                  MARGIN = 1,
                  which.max)
      d <- stage_action_set[dd]
    } else{
      # evaluating the g-function:
      if (!all(g_function$H_names %in% names(H))){
        mes <- paste(
          "H must contain the columns",
          paste(g_function$H_names, collapse = ","),
          "."
        )
        stop(mes)
      }
      g_values <- predict(g_function$g_model, new_H = H)

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values, MARGIN = 1, function(x) x >= alpha))
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")
      realistic_actions[which(realistic_actions == FALSE)] <- NA

      # getting the action with the maximal realistic Q-function value:
      dd <- apply(q_values * realistic_actions, MARGIN = 1, which.max)
      d <- stage_action_set[dd]
    }

    # including the g_values as attributes:
    if (include_g_values == TRUE)
      attr(d, "g_values") <- g_values

    return(d)
  }

  return(stage_policy)
}
