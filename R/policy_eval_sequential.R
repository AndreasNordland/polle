#' @export
policy_eval_sequential <- function(policy_data,
                                   policy = NULL, policy_learn = NULL,
                                   g_functions = NULL, g_models = g_glm(),
                                   g_full_history = FALSE, save_g_functions = TRUE,
                                   q_functions = NULL, q_models = q_glm(),
                                   q_full_history = FALSE, save_q_functions = TRUE,
                                   target = "value",
                                   type = "dr",
                                   M = 4,
                                   train_block_size = get_n(policy_data) / 5,
                                   name = NULL,
                                   min_subgroup_size = 1
                                   ) {
  ## input checks:
  if (!inherits(policy_data, what = "policy_data"))
    stop("policy_data must be of inherited class 'policy_data'.")
  if (!is.null(policy)) {
    if (!inherits(policy, what = "policy")) {
      stop("policy must be of inherited class 'policy'.")
    }
  }
  if ((is.null(policy) && is.null(policy_learn)) ||
      (!is.null(policy_learn) && !is.null(policy))) {
    stop("Provide either policy or policy_learn.")
  }
  if (is.null(policy) && !is.null(policy_learn)) {
    if (!inherits(policy_learn, what = "policy_learn")) {
      stop("policy_learn must be of inherited class 'policy_learn'.")
    }
  }
  if (!is.null(g_functions)) {
    if (!(inherits(g_functions, "g_functions"))) {
      stop("g_functions must be of class 'g_functions'.")
    }
  }
  if (!(is.logical(g_full_history) && (length(g_full_history) == 1))) {
    stop("g_full_history must be TRUE or FALSE")
  }
  if (!is.null(q_functions)) {
    if (!(inherits(q_functions, "q_functions"))) {
      stop("q-functions must be of class 'q_functions'.")
    }
  }
  if (!(is.logical(q_full_history) && (length(q_full_history) == 1))) {
    stop("q_full_history must be TRUE or FALSE")
  }
  if (!(is.numeric(M) && (length(M) == 1))) {
    stop("M must be an integer greater than 0.")
  }
  if (!(M %% 1 == 0)) {
    stop("M must be an integer greater than 0.")
  }
  if (M <= 0) {
    stop("M must be an integer greater than 0.")
  }
  if (!is.null(name)) {
    name <- as.character(name)
    if (length(name) != 1) {
      stop("name must be a character string.")
    }
  }
  if (!(min_subgroup_size %% 1 == 0)) {
    stop("min_subgroup_size must be an integer greater than 0.")
  }
  if (min_subgroup_size <= 0) {
    stop("min_subgroup_size must be an integer greater than 0.")
  }
  target <- tolower(target)
  if (length(target) != 1) {
    stop("target must be a character string.")
  }
  if (target %in% c(
                    "value",
                    "policy_value"
                  )) {
    target <- "value"
  } else if (target %in% c(
                           "subgroup",
                           "sub_effect",
                           "subeffect",
                           "subvalue",
                           "sub_value"
                         )) {
    target <- "subgroup"
  } else {
    stop("target must be either 'value' or 'subgroup'.")
  }
  type <- tolower(type)
  if (length(type) != 1) {
    stop("type must be a character string.")
  }
  if (type %in% c("dr", "aipw")) {
    type <- "dr"
  } else if (type %in% c("ipw")) {
    type <- "ipw"
  } else if (type %in% c("or", "q")) {
    type <- "or"
  } else {
    stop("type must be either 'dr', 'ipw' or  'or'.")
  }

  ## editing name:
  if (is.null(name)) {
    if (target == "value") {
      name <- "E[U(d)]"
    }

    if (target == "subgroup") {
      as <- get_action_set(policy_data)
      name1 <- paste0("E[U(", as[2], ")-U(", as[1], ")|d=", as[2], "]")
      name2 <- paste0("E[U(", as[2], ")-U(", as[1], ")|d=", as[1], "]")
      name <- c(name1, name2)
      rm(as, name1, name2)
    }
  }

  ## collecting the arguments to be passed on:
  args <- as.list(environment())
  args[["policy_data"]] <- NULL
  args[["M"]] <- NULL
  args[["train_block_size"]] <- NULL

  eval <- policy_eval_seq(args = args,
                          policy_data = policy_data,
                          train_block_size = train_block_size,
                          M = M)

  return(eval)
}


policy_eval_seq <- function(args,
                            policy_data,
                            train_block_size,
                            M) {
  n <- get_n(policy_data)
  id <- get_id(policy_data)
  K <- get_K(policy_data)
  target <- get_element(args, "target")

  ## random index of the data (by id):
  random_index <- sample(1:n, n)

  ## setting up the sequential training and validation folds:
  sequential_index <- random_index[(train_block_size +1):n]
  sequential_index <- split(sequential_index, rep(1:M, length.out = length(sequential_index)))

  train_sequential_index <- list(random_index[1:train_block_size])
  train_sequential_index <- append(train_sequential_index, sequential_index[-M])
  if (M > 1){
    tmp <- train_sequential_index[[1]]
    for (m in 2:length(train_sequential_index)) {
      tmp <- c(tmp, train_sequential_index[[m]])
      train_sequential_index[[m]] <- tmp
    }
    rm(tmp)
  }
  train_sequential_index <- lapply(train_sequential_index, sort)
  valid_sequential_index <- sequential_index
  valid_sequential_index <- lapply(valid_sequential_index, sort)

  sequential_fits <- lapply(seq_along(train_sequential_index), function(m){
    ## training and validation ids:
    train_id <- id[train_sequential_index[[m]]]
    validation_id <- id[valid_sequential_index[[m]]]

    ## training data:
    train_policy_data <- subset_id(policy_data, train_id)
    if (get_K(train_policy_data) != K) {
      stop("The number of stages varies accross the training folds.")
    }

    ## validation data:
    valid_policy_data <- subset_id(policy_data, validation_id)
    if (get_K(valid_policy_data) != K) {
      stop("The number of stages varies accross the validation folds.")
    }


    eval_args <- append(args,
                        list(valid_policy_data = valid_policy_data,
                             train_policy_data = train_policy_data))

    valid_pe <- do.call(what = "policy_eval_type", args = eval_args)

    ## getting the in-sample variance estimate
    args[["g_models"]] <- NULL
    args[["q_models"]] <- NULL

    args[["g_functions"]] <- get_g_functions(valid_pe)
    args[["q_functions"]] <- get_q_functions(valid_pe)

    policy <- get_policy(valid_pe)
    if (is.null(policy)){
      policy <- get_element(args, "policy")
    }
    if (inherits(policy, what = "policy")) {
      policy <- list(policy)
    }

    insample_parameters <- lapply(policy, function(p){
      args[["policy"]] <- p

      eval_args <- append(args,
                          list(valid_policy_data = train_policy_data,
                               train_policy_data = train_policy_data))

      pe <- do.call(what = "policy_eval_type", args = eval_args)

      sigma2 <- var(IC(pe))
      sigma2 <- diag(sigma2)
      names(sigma2) <- get_element(pe, "name")

      out <- list(sigma2 = sigma2)

      if (target == "subgroup"){
        subgroup_indicator <- get_element(pe, "subgroup_indicator")
        subgroup_prob <- apply(
          subgroup_indicator,
          MARGIN = 2,
          FUN = mean,
          simplify = FALSE
        )
        subgroup_prob <- unlist(subgroup_prob)
        names(subgroup_prob) <- get_element(pe, "name")
        out[["subgroup_prob"]] <- subgroup_prob
      }

      return(out)
    })

    insample_sigma2 <- lapply(insample_parameters, function(x) get_element(x, "sigma2"))
    insample_sigma2 <- unlist(insample_sigma2)

    insample_subgroup_prob <- lapply(insample_parameters, function(x) get_element(x, "subgroup_prob", check_name = FALSE))
    insample_subgroup_prob <- unlist(insample_subgroup_prob)

    out <- valid_pe
    out[["insample_sigma2"]] <- insample_sigma2
    out[["insample_subgroup_prob"]] <- insample_subgroup_prob

    return(out)
  })

  target <- get_element(args, "target")
  if (target == "value"){
    online_onestep_terms <- lapply(sequential_fits, function(x){
      IC <- get_element(x, "IC")
      coef <- get_element(x, "coef")
      Z <- apply(IC, MARGIN = 1, function(x) x + coef, simplify = FALSE)
      Z <- do.call(what = "rbind", Z)

      sigma <- unname(sqrt(get_element(x, "insample_sigma2")))

      scaled_Z <- apply(Z, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      scaled_Z <- do.call(what = "rbind", scaled_Z)

      Gamma <- Z * 0 + 1
      Gamma <- apply(Gamma, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      Gamma <- do.call(what = "rbind", Gamma)

      out <- list(scaled_Z = scaled_Z, Gamma = Gamma)
      return(out)
    })

    scaled_Z <- lapply(online_onestep_terms, function(x) get_element(x, "scaled_Z"))
    scaled_Z <- do.call(what = "rbind", scaled_Z)

    Gamma <- lapply(online_onestep_terms, function(x) get_element(x, "Gamma"))
    Gamma <- do.call(what = "rbind", Gamma)

    coef <- colSums(scaled_Z) / colSums(Gamma)
    vcov <- colMeans(Gamma)^(-2)/nrow(Gamma)
  } else if (target == "subgroup") {
    online_onestep_terms <- lapply(sequential_fits, function(x){
      subgroup_indicator <- get_element(x, "subgroup_indicator")
      Z <- get_element(x, "Z")
      subgroup_prob <- get_element(x, "insample_subgroup_prob")
      sigma <- unname(sqrt(get_element(x, "insample_sigma2")))

      blip <- Z[, 2] - Z[, 1]
      D <- apply(
        subgroup_indicator,
        MARGIN = 2,
        FUN = function(si){
          blip * si
        },
        simplify = FALSE
      )
      D <- do.call(what = "cbind", D)
      scaled_D <- apply(D, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      scaled_D <- do.call(what = "rbind", scaled_D)

      Gamma <- D * 0 + 1
      Gamma <- apply(Gamma, MARGIN = 1, function(x) x / sigma, simplify = FALSE)
      Gamma <- do.call(what = "rbind", Gamma)

      out <- list(scaled_D = scaled_D, Gamma = Gamma)
      return(out)
    })

    scaled_D <- lapply(online_onestep_terms, function(x) get_element(x, "scaled_D"))
    scaled_D <- do.call(what = "rbind", scaled_D)

    Gamma <- lapply(online_onestep_terms, function(x) get_element(x, "Gamma"))
    Gamma <- do.call(what = "rbind", Gamma)

    coef <- colSums(scaled_D) / colSums(Gamma)
    vcov <- colMeans(Gamma)^(-2)/nrow(Gamma)

  } else {
    mes <- "policy_eval_sequential only implemented for target = 'value' or 'subgroup'."
    stop(mes)
  }

  out <- policy_eval_sequential_object(
    coef = coef,
    vcov = vcov,
    type = get_element(args, "type"),
    target = target,
    id = id,
    train_sequential_index = train_sequential_index,
    valid_sequential_index = valid_sequential_index,
    name = get_element(sequential_fits[[1]], "name")
  )

  return(out)
}

policy_eval_sequential_object <- function(coef,
                                          vcov,
                                          type,
                                          target,
                                          id,
                                          name,
                                          train_sequential_index,
                                          valid_sequential_index) {
  names(coef) <- name
  out <- as.list(environment())
  out <- remove_null_elements(out)
  class(out) <- c("policy_eval_sequential", "policy_eval")

  return(out)
}

#' @rdname policy_eval
#' @export
vcov.policy_eval_sequential <- function(object, ...) {
  vcov <- get_element(object, "vcov")
  tmp <- matrix(NA, ncol = length(vcov), nrow = length(vcov))
  diag(tmp) <- vcov
  vcov <- tmp
  return(vcov)
}
