#' Fit Single c-function
#'
#' \code{fit_c_function} is used to fit a single right-censoring model.
#' \code{c_model} can eiter be of inherited class c_model or g_model for
#' discrete right-censoring.
#' @param history History object created by [get_history()]
#' @param c_model Censoring model, c-model/g-model, created by ?? or similar functions.
#' @returns Object of class "c_function".
#' @noRd
fit_c_function <- function(history, c_model){
  stage <- get_element(history, "stage", check_name = FALSE)
  c_model_class <- class(c_model)

  ## getting the event, time and the model matrix (H):
  event <- get_event(history)
  time <- get_time(history)
  time2 <- get_time2(history)
  H <- get_H(history)

  ## checking if right-censoring occur
  no_censoring <- all(event != 2)

  ## overwriting the c-model if no censoring occur:
  if (no_censoring == TRUE) {
    c_model <- c_no_censoring()
    c_model_class <- class(c_model)
  }

  ## fitting the model:
  if (inherits(c_model, "g_model")) {
    A <- as.numeric(!(event == c(2)))
    tryCatch({
      c_model <- c_model(A = A, H = H, action_set = c(0,1))
    }, error = function(e) {
      stop("Error fitting c_model: ", e$message)
    }, warning = function(w) {
      warning("Warning in c_model fitting: ", w$message)
    })
  } else {
    c_model <- c_model(event = event, time = time, time2 = time2, H = H)
  }

  c_function <- list(
    c_model = c_model,
    c_model_class = c_model_class,
    H_names = colnames(H),
    stage = stage
  )
  class(c_function) <- "c_function"

  return(c_function)
}

#' @export
predict.c_function <- function(object, new_history, ...){
  c_model <- get_element(object, "c_model")
  c_model_class <- get_element(object, "c_model_class")
  H_names <- get_element(object, "H_names")

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  ## time points for evaluation of the c-model:
  new_time <- get_time(new_history)
  new_time2 <- get_time2(new_history)

  if ("g_model" %in% c_model_class) {
    surv <- matrix(predict(c_model, new_H = new_H)[, 2])
    surv <- cbind(1, surv)
    colnames(surv) <- c("surv_time", "surv_time2")
  } else {
    surv_time <- predict(c_model, new_H = new_H, new_time = new_time)
    surv_time2 <- predict(c_model, new_H = new_H, new_time = new_time2)
    surv <- cbind(surv_time, surv_time2)
    colnames(surv) <- c("surv_time", "surv_time2")
  }

  if (!all(complete.cases(surv))){
    mes <- "The c-function predictions have missing values."
    stop(mes)
  }

  if (!all((surv) >= 0 & (surv <= 1))){
    mes <- "The c-function predictions are not in [0,1]."
    stop(mes)
  }

  # including the id's and stage number(s)
  c_values <- data.table(id_stage, surv)
  setkeyv(c_values, c("id", "stage"))

  return(c_values)
}

#' Fit Censoring Functions
#'
#' Fits right-censoring models for each stage or a single model across all stages.
#'
#' @param policy_data Policy data object created by [policy_data()]
#' @param c_models Single c_model or list of K+1 c_models
#' @param full_history Logical; use full history (TRUE) or Markov-type history (FALSE)
#'
#' @return List of fitted censoring functions with class "c_functions"
#'
#' @details
#' The function handles two scenarios:
#' * Multiple models: One model per stage (length K+1)
#' * Single model: Same model applied across all stages
#'
#' @export
fit_c_functions <- function(policy_data, c_models, full_history = FALSE){
  K <- get_K(policy_data)

  # input checks:
  if (!(is.logical(full_history) & (length(full_history) == 1)))
    stop("full_history must be TRUE or FALSE")
  if (is.null(c_models))
    stop("Please provide c_models.")

  mes <- "c_models must be a single c_model (or g_model) or a list of K+1 c_model's (or g_model's)."
  if (is.list(c_models)) {
    ## updating class:
    c_models <- lapply(c_models, function(cm) {
      if (inherits(cm, "g_model")) {
        class(cm) <- c("c_model", class(cm))
      }
      return(cm)
    })
    tmp <- all(unlist(lapply(c_models, function(cm) inherits(cm, "c_model"))))
    if (!tmp)
      stop(mes)
    rm(tmp)
    if (length(c_models) != (K+1))
      stop(mes)
  } else {
    if (inherits(c_models, "g_model")) {
      ## updating class:
      class(c_models) <- c("c_model", class(c_models))
    }
    if (!inherits(c_models, "c_model"))
      stop(mes)
    if (full_history == TRUE)
      stop("full_history must be FALSE when a single c-model is provided.")
  }

  # fitting the c-models:
  if (is.list(c_models)){
    history <- lapply(1:(K+1),
                      function(s) get_history(policy_data,
                                              stage = s,
                                              full_history = full_history,
                                              event_set = c(0,1,2)))
    c_functions <- mapply(history,
                          c_models,
                          FUN = function(h, cm) fit_c_function(history = h,
                                                               c_model = cm),
                          SIMPLIFY = FALSE)
    names(c_functions) <- paste("stage_", 1:(K+1), sep = "")
  } else{
    history <- get_history(policy_data,
                           stage = NULL,
                           full_history = FALSE,
                           event_set = c(0,1,2))
    c_functions <- list(all_stages = fit_c_function(history, c_models))
  }

  class(c_functions) <- c("c_functions")
  attr(c_functions, "full_history") <- full_history

  return(c_functions)
}

#' @export
predict.c_functions <- function(object, new_policy_data, ...){
  K <- get_K(new_policy_data)
  full_history <- attr(object, "full_history")

  if (length(object) == (K+1)){
    history <- lapply(1:(K+1), function(s) get_history(new_policy_data,
                                                       stage = s,
                                                       full_history = full_history,
                                                       event_set = c(0,1,2)))
    values <- mapply(history,
                     object,
                     FUN = function(h, f) predict(f, h),
                     SIMPLIFY = FALSE)
    values <- rbindlist(values)
    setkeyv(values, c("id", "stage"))
  } else if (length(object) == 1){
    ## state history across all stages 1,...,K+1:
    history <- get_history(new_policy_data,
                           stage = NULL,
                           full_history = FALSE,
                           event_set = c(0,1,2))
    values <- predict(object[[1]], history)
  } else{
    stop("Provide either 1 or K+1 c-functions for evaluation.")
  }

  return(values)
}
