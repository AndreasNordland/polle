#' Fit Single c-function
#'
#' \code{fit_c_function} is used to fit a single time depending coarsening model, i.e.,
#' right-censoring model.
#' @param history History object created by [get_history()]
#' @param c_model Censoring model/g-model created by ?? or similar functions.
#' @returns Object of class "g_function".
#' @noRd
fit_c_function <- function(history, c_model){
  stage <- get_element(history, "stage", check_name = FALSE)

  ## getting the event, time and the model matrix (H):
  event <- get_event(history)
  time <- get_time(history)
  time2 <- get_time2(history)
  H <- get_H(history)

  ## fitting the model:
  c_model <- c_model(event = event, time = time, time2 = time2, H = H)

  c_function <- list(
    c_model = c_model,
    H_names = colnames(H),
    stage = stage
  )
  class(c_function) <- "c_function"

  return(c_function)
}

#' @export
predict.c_function <- function(object, new_history, ...){
  c_model <- getElement(object, "c_model")
  H_names <- getElement(object, "H_names")

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  ## time points for evaluation of the c-model:

  new_time <- get_time(new_history)
  new_time2 <- get_time2(new_history)

  surv_time <- predict(c_model, new_H = new_H, new_time = new_time)
  surv_time2 <- predict(c_model, new_H = new_H, new_time = new_time2)
  surv <- cbind(surv_time, surv_time2)
  colnames(surv) <- c("surv_time", "surv_time2")

  if (!all(complete.cases(surv))){
    if(!is.null(stage)){
      mes <- paste("The c-function predictions at stage ",
                   stage,
                   " have missing values.",
                   sep = "")
    } else {
      mes <- "The c-function predictions have missing values."
    }
    stop(mes)
  }

  if (!all((surv) >= 0 & (surv <= 1))){
    if(!is.null(stage)){
      mes <- paste("The c-function predictions at stage ",
                   stage,
                   " are not in [0,1].",
                   sep = "")
    } else {
      mes <- "The c-function predictions are not in [0,1]."
    }
    stop(mes)
  }


  # including the id's and stage number(s)
  c_values <- data.table(id_stage, surv)
  setkeyv(c_values, c("id", "stage"))

  return(c_values)
}

#' @export
fit_c_functions <- function(policy_data, c_models, full_history = FALSE){
  K <- get_K(policy_data)

  # input checks:
  if (!(is.logical(full_history) & (length(full_history) == 1)))
    stop("full_history must be TRUE or FALSE")
  if (is.null(c_models))
    stop("Please provide c_models.")
  mes <- "c_models must be a single c_model or a list of K+1 c_models's."
  if (is.list(c_models)){
    tmp <- all(unlist(lapply(c_models, function(cm) inherits(cm, "c_model"))))
    if (!tmp)
      stop(mes)
    rm(tmp)
    if (length(c_models) != (K+1))
      stop(mes)
  } else{
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
                                              type = "event"))
    c_functions <- mapply(history,
                          c_models,
                          FUN = function(h, cm) fit_c_function(history = h,
                                                               c_model = cm),
                          SIMPLIFY = FALSE)
    names(c_functions) <- paste("stage_", 1:(K+1), sep = "")
  } else{
    history <- get_history(policy_data, stage = NULL, full_history = FALSE, type = "event")
    c_functions <- list(all_stages = fit_c_function(history, c_models))
  }

  class(c_functions) <- c("c_functions")
  attr(c_functions, "full_history") <- full_history

  return(c_functions)
}
