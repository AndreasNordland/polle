fit_blip_function <- function(history, Z, blip_model, valid_ids){
  action_set <- getElement(history, "action_set")
  stage_action_set <- getElement(history, "stage_action_set")
  stage <- getElement(history, "stage")
  H <- get_H(history)
  stage_id <- unlist(get_id.history(history),
                     use.names = FALSE)

  folds <- NULL
  if (!is.null(valid_ids)){
    # adapting the validation ids to the current stage:
    stage_valid_ids <- lapply(
      valid_ids,
      function(vi){
        vi[vi %in% stage_id]
      }
    )
    folds <-
      lapply(
        stage_valid_ids,
        function(vi){
          which(stage_id %in% vi)
        }
      )
  }

  # fitting the blip-model:
  z <- Z[, stage_action_set]
  blip <- z[, 2] - z[,1]
  blip_model <- blip_model(AH = H, V_res = blip, folds = folds)

  blip_function <- list(
    blip_model = blip_model,
    action_set = action_set,
    stage_action_set = stage_action_set,
    stage = stage
  )
  class(blip_function) <- "blip_function"

  return(blip_function)
}

predict.blip_function <- function(object, new_history){
  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)
  blip_model <- getElement(object, "blip_model")
  blip <- predict(blip_model, new_H)
  blip <- data.table(id_stage, blip = blip)
  setkeyv(blip, c("id", "stage"))

  return(blip)
}


#' @export
control_blip <- function(blip_models = q_glm(~.)){
  control <- as.list(environment())
  return(control)
}

blip <- function(policy_data,
                 alpha,
                 g_models, g_functions, g_full_history,
                 q_models, q_full_history,
                 blip_models, full_history,
                 L, cross_fit_g_models,
                 save_cross_fit_models, future_args,
                 ...){
  K <- get_K(policy_data)
  n <- get_n(policy_data)
  action_set <- get_action_set(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)

  # input checks:
  if (!is.null(g_functions)){
    if(!inherits(g_functions, what = "g_functions"))
      stop("g-functions must be of class 'g_functions'.")
  }
  if (is.list(q_models)){
    if (length(q_models) != K)
      stop("q_models must either be a list of length K or a single Q-model.")
  }
  if(is.null(blip_models))
    stop("blip_models are missing.")
  if (is.list(blip_models)){
    if (length(blip_models) != K) stop("blip_models must either be a list of length K or a single blip-model.")
  }
  if(!all(lapply(stage_action_sets, length) == 2))
    stop("policy_learn with type 'blip' only works for dichotomous stage action sets.")

  # getting the observed actions:
  actions <- get_actions(policy_data)

  # getting the IDs:
  id <- get_id(policy_data)

  # getting the observed (complete) utilities:
  utility <- get_utility(policy_data)

  # constructing the folds for cross-fitting:
  if (L > 1){
    folds <- split(sample(1:n, n), rep(1:L, length.out = n))
    folds <- lapply(folds, sort)
  } else{
    folds <- NULL
  }

  # (cross-)fitting the g-functions:
  valid_ids <- NULL
  g_functions_cf <- NULL
  if (!is.null(folds) & cross_fit_g_models == TRUE){
    g_cf <- fit_g_functions_cf(
      policy_data = policy_data,
      g_models = g_models,
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

  # (n) vector with entries U_i:
  U <- utility$U
  # (n X K) matrix with entries I(d_k(H_k) = A_k):
  II <- matrix(nrow = n, ncol = K)
  # (n X K) matrix with entries g_k(A_k, H_k)
  g_A_values <- get_a_values(a = actions$A,
                             action_set = action_set,
                             g_values)
  G <- dcast(g_A_values, id ~ stage, value.var = "P")[, -c("id"), with = FALSE]
  G <- as.matrix(G)
  # (n X K+1) matrix with entries Q_k(H_{k,i}, d_k(H_{k,i})), Q_{K+1} = U:
  Q <- matrix(nrow = n, ncol = K+1)
  Q[, K+1] <- U

  g_cols <- paste("g_", action_set, sep = "")
  q_cols <- paste("Q_", action_set, sep = "")

  q_functions <- list()
  q_functions_cf <- list()
  blip_functions <- list()

  for (k in K:1){
    if (is.null(folds)){
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
    } else{
      q_step_cf_k <- q_step_cf(
        folds = folds,
        policy_data = policy_data,
        k = k,
        full_history = q_full_history,
        Q = Q[, k+1],
        q_models = q_models,
        save_cross_fit_models = save_cross_fit_models,
        future_args = future_args
      )
      q_functions_cf[[k]] <- getElement(q_step_cf_k, "q_functions_cf")
      q_values_k <- getElement(q_step_cf_k, "q_values")
      idx_k <- getElement(q_step_cf_k, "idx_k")
      rm(q_step_cf_k)
    }

    # getting the action matrix for stage k:
    stage <- NULL
    A_k <- actions[stage == k, ]$A
    IA_k <- action_matrix(A_k, action_set)
    rm(stage)

    # calculating the Z-matrix
    Z_1 <- Q_k <- as.matrix(q_values_k[, q_cols, with = FALSE])
    Z_2 <- (IA_k / G[idx_k, k]) * (Q[idx_k, k+1] - Q_k)
    Z_3 <- 0
    if (k != K){
      for (r in (k+1):K){
        Z_3 <- Z_3 + ipw_weight(II[idx_k,(k+1):r], G[idx_k,(k+1):r]) *
          (Q[idx_k, r+1] - Q[idx_k, r])
      }
      Z_3 <- (IA_k / G[idx_k, k]) * Z_3
    }
    Z <- Z_1 + Z_2 + Z_3
    colnames(Z) <- action_set

    # getting the history for the blip model:
    blip_history_k <- get_history(policy_data,
                                  stage = k,
                                  full_history = full_history)
    # fitting the blip-function:
    if (is.list(blip_models)){
      blip_model_k <- blip_models[[k]]
    } else{
      blip_model_k <- blip_models
    }
    blip_function_k <- fit_blip_function(blip_history_k,
                                         Z = Z,
                                         blip_model = blip_model_k,
                                         valid_ids = valid_ids)
    blip_functions[[k]] <- blip_function_k

    # getting the blip-function values:
    blip_k <- predict(blip_function_k, new_history = blip_history_k)

    # getting the stage action with a positive blip:
    stage_action_set <- stage_action_sets[[k]]
    if (alpha != 0){
      # getting the g-function values for each action:
      g_values_k <- g_values[stage == k, ]

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values_k[, g_cols, with = FALSE],
                                   MARGIN = 1,
                                   function(x) x >= alpha))
      realistic_actions <- realistic_actions[, action_set %in% stage_action_set]
      # checking that a realistic action exists:
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")

      # calculating the optimal actions:
      dd <- (blip_k$blip > 0) + 1

      # overruling unrealistic actions:
      realistic <- apply(realistic_actions, 1, prod)
      dd <- dd * realistic + (realistic == 0) * apply(realistic_actions,1,which.max)
    } else {
      dd <- (blip_k$blip > 0) + 1
    }
    d <- stage_action_set[dd]

    q_d_k <- get_a_values(a = d, action_set = action_set, q_values_k)$P
    Q[idx_k, k] <- q_d_k
    Q[!idx_k, k] <- Q[!idx_k, k+1]
    II[idx_k, k] <- (A_k == d)
    II[!idx_k, k] <- TRUE
    G[!idx_k,k] <- TRUE
  }

  if (length(q_functions) > 0){
    class(q_functions) <- "nuisance_functions"
    attr(q_functions, "full_history") <- q_full_history
    names(q_functions) <- paste("stage_", 1:K, sep = "")
  } else{
    q_functions <- NULL
  }
  if (length(q_functions_cf) == 0){
    q_functions_cf <- NULL
  }
  class(blip_functions) <- "nuisance_functions"
  attr(blip_functions, "full_history") <- full_history
  names(blip_functions) <- paste("stage_", 1:K, sep = "")

  out <- list(
    blip_functions = blip_functions,
    q_functions = q_functions,
    q_functions_cf = q_functions_cf,
    g_functions = g_functions,
    g_functions_cf = g_functions_cf,
    action_set = action_set,
    stage_action_sets = stage_action_sets,
    alpha = alpha,
    K = K,
    folds = folds
  )
  out <- remove_null_elements(out)
  class(out) <- c("blip","policy_object","list")

  return(out)
}

#' @rdname get_policy_functions
#' @export
get_policy_functions.blip <- function(object, stage, include_g_values = FALSE){
  stage_action_sets <- getElement(object, "stage_action_sets")
  stage_action_set <- stage_action_sets[[stage]]; rm(stage_action_sets)
  K <- getElement(object, "K")
  if(!((stage >= 0) & (stage <= K)))
    stop("stage must be smaller than or equal to K.")
  alpha <- getElement(object, "alpha")
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
  blip_functions <- getElement(object, "blip_functions")
  blip_model <- getElement(blip_functions[[stage]], "blip_model")
  full_history <- attr(blip_functions, "full_history")
  stage_policy <- function(H){
    blip <- predict(blip_model, H)
    g_values <- NULL
    if (alpha == 0){
      dd <- (blip > 0) + 1
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
      # g-function values on the stage action set:
      g_values <- predict(g_function$g_model, new_H = H)

      # calculating the realistic actions:
      realistic_actions <- t(apply(g_values, MARGIN = 1, function(x) x >= alpha))
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")

      # calculating the optimal actions:
      dd <- (blip > 0) + 1
      realistic <- apply(realistic_actions, 1, prod)
      dd <- dd * realistic + (realistic == 0) * apply(realistic_actions,1,which.max)
      d <- stage_action_set[dd]
    }

    # including the g_values as attributes:
    if (include_g_values == TRUE)
      attr(d, "g_values") <- g_values

    return(d)
  }

  return(stage_policy)
}

#' @export
get_policy.blip <- function(object){
  g_functions <- get_g_functions(object)
  blip_functions <- getElement(object, "blip_functions")

  action_set <- getElement(object, "action_set")
  stage_action_sets <- getElement(object, "stage_action_sets")
  K <- getElement(object, "K")
  alpha <- getElement(object, "alpha")

  g_cols <- paste("g_", action_set, sep = "")

  policy <- function(policy_data){
    if (get_K(policy_data) != K)
      stop("The policy do not have the same number of stages as the policy data object.")
    # evaluating the blip-functions:
    blip_values <- predict(blip_functions, policy_data)

    # getting the stage actions (sa) associated with the blip:
    sa <- as.data.table(do.call(what = "rbind", stage_action_sets))
    colnames(sa) <- c("ref_action", "alt_action")
    sa <- cbind(stage = 1:K, sa)
    setkeyv(sa, "stage")

    if (alpha != 0){
      # evaluating the g-functions:
      g_values <- predict(g_functions, policy_data)
      # calculating the realistic actions:
      realistic_actions <- t(apply(
        g_values[ , g_cols, with = FALSE],
        MARGIN = 1,
        function(x) x >= alpha
      ))
      if (any(apply(realistic_actions, MARGIN = 1, sum) == 0))
        stop("Cases with no realistic actions occur. Consider resetting the alpha level.")

      realistic <- apply(realistic_actions, MARGIN = 1, function(x) action_set[x], simplify = FALSE)
      realistic_indicator <- unlist(lapply(realistic, function(x) length(x) == 2))
      realistic[realistic_indicator] <- NA
      realistic <- unlist(realistic)

      # getting the optimal stage action:
      blip_values <- merge(blip_values, sa, by = "stage")
      setkeyv(blip_values, c("id", "stage"))
      blip_values[, 'd' := ifelse(blip > 0, alt_action, ref_action)]
      blip_values[, 'ri' := realistic_indicator]
      blip_values[, 'r' := realistic]
      blip_values[ ,'d' := ifelse(ri, d, r)]
    } else{
      # getting the optimal stage action:
      blip_values <- merge(blip_values, sa, by = "stage")
      setkeyv(blip_values, c("id", "stage"))
      blip_values[, 'd' := ifelse(blip > 0, alt_action, ref_action)]
    }

    # collecting the policy actions
    policy_actions <- blip_values[, c("id", "stage", "d")]

    return(policy_actions)
  }

  # setting class and attributes:
  policy <- new_policy(policy, name = "drql")

  return(policy)
}

# plot function
plot_policy_object.blip <- function(
    policy_object,
    policy_data,
    id,
    continuous_variable_1,
    range_1,
    continuous_variable_2,
    range_2,
    stage = NULL,
    granularity = 20
){
  id_ <- id
  action_set <- get_action_set(policy_data)
  policy_data <- subset_id(policy_data,
                           id = id_,
                           preserve_action_set = FALSE)
  stage_ <- stage; rm(stage)

  # constructing the grid for the continuous variables:
  seq_variable_1 <- seq(from = range_1[1],
                        to = range_1[2],
                        length.out = granularity)
  seq_variable_2 <- seq(from = range_2[1],
                        to = range_2[2],
                        length.out = granularity)
  grid <- data.table::CJ(seq_variable_1, seq_variable_2)
  names(grid) <- c(continuous_variable_1, continuous_variable_2)

  # getting the number of stages for the given observation:
  K <- get_K(policy_data)

  # setting the default value of 'stage' (all stages):
  if (is.null(stage_))
    stage_ <- 1:K

  # taking the intersection of the observed stages and the input stages:
  stage_ <- intersect(1:K, stage_)
  k <- length(stage_)

  # getting the g-functions
  g_functions <- get_g_functions(policy_object)
  g_full_history <- attr(g_functions, "full_history")

  plot_data <- list()
  plot_data_2 <- list()
  for (s_ in 1:k){

    # full history:
    blip_functions <- getElement(policy_object, "blip_functions")
    full_history <- attr(blip_functions, "full_history")
    stopifnot(full_history == g_full_history)

    # getting the history matrix
    his <- get_history(policy_data,
                       stage = stage_[s_],
                       full_history = full_history)

    H <- his[["H"]]

    # getting the observed action
    A <- his[["A"]]
    A <- merge(A, H)

    # checks
    stopifnot(
      continuous_variable_1 %in% names(H),
      continuous_variable_2 %in% names(H)
    )

    H <- H[rep(1, each = granularity^2)]
    H[,(continuous_variable_1) := NULL]
    H[,(continuous_variable_2) := NULL]
    H <- cbind(H, grid)

    # getting the policy actions
    pf <- get_policy_functions(policy_object,
                               stage = stage_[s_],
                               include_g_values = TRUE)
    d <- pf(H)

    # getting the realistic actions
    g_values <- attr(d, "g_values")
    colnames(g_values) <- paste("g_", action_set, sep = "")
    alpha <- getElement(policy_object, "alpha")
    realistic_actions <- t(apply(g_values, MARGIN = 1, function(x) x >= alpha))
    realistic <- apply(realistic_actions, 1, prod)

    H <- cbind(H, d = d, realistic = realistic, g_values)

    plot_data[[s_]] <- H
    plot_data_2[[s_]] <- A
  }
  plot_data <- data.table::rbindlist(plot_data)
  plot_data$realistic <- as.factor(as.logical(plot_data$realistic))
  plot_data$d <- factor(plot_data$d, levels = action_set)

  plot_data_2 <- data.table::rbindlist(plot_data_2)
  plot_data_2$A <- factor(plot_data_2$A, levels = action_set)

  # plot of the policy actions:
  p1 <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(ggplot2::aes_string(x = {{continuous_variable_1}},
                                            y = {{continuous_variable_2}},
                                            color = "d",
                                            alpha = "realistic")) +
    ggplot2::geom_point(data = plot_data_2, ggplot2::aes_string(x = {{continuous_variable_1}},
                                                                y = {{continuous_variable_2}},
                                                                fill = "A"),
                        size = 2,
                        color = "black",
                        pch=21) +
    ggplot2::scale_alpha_discrete(range = c(0.35, 1)) +
    ggplot2::scale_fill_discrete(drop=FALSE) +
    ggplot2::facet_wrap(~stage, labeller = "label_both") +
    ggplot2::theme_bw()

  return(p1)
}

