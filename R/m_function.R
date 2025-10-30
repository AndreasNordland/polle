fit_m_function <- function(policy_data,
                           m_model,
                           full_history = FALSE) {
  K <- get_K(policy_data)

  # input checks:
  if (!is.null(m_model)){
    mes <- "m_model must be a single q_model."
    if (!inherits(m_model, "q_model")) {
      stop(mes)
    }
  }

  ## check if missing final outcomes occur (at stage K+1)
  missing <- get_element(policy_data, "cens_indicator")
  indicator <- stage <- NULL
  missing <- missing[get("stage") == (K+1), list(get("indicator"))]
  missing <- unlist(missing)

  if (missing == FALSE){
    m_function <- NULL
  } else {
    if (is.null(m_model)) {
      stop("right-censoring/missing (final) outcome occur at stage K+1, please provide an m_model.")
    }

    ## getting the IDs:
    id <- get_id(policy_data)

    ## getting the history at stage K+1:
    his <- get_history(policy_data,
                       stage = K+1,
                       full_history = full_history,
                       event_set = c(0,1,2))
    H <- get_H(his)[get_event(his) == 1, ]
    id_not_missing <- get_id(his)[get_event(his) == 1][["id"]]

    if(length(id_not_missing) == 0) {
      stop("Unable to fit m_model: all utility outcomes are missing")
    }

    ## getting the observed (complete) utility:
    utility <- get_utility(policy_data)
    ## vector with non-missing entries U_i:
    U <- utility$U[(id %in% id_not_missing)]
    stopifnot(any(!is.na(U)))
    stopifnot(length(U) == nrow(H))

    ## fitting the m-model:
    tryCatch({
      m_model <- m_model(AH = H, V_res = U)
    }, error = function(e) {
      stop("Error fitting m_model: ", e$message)
    }, warning = function(w) {
      warning("Warning in m_model fitting: ", w$message)
      ## Continue with the model despite warning
    })

    ## setting S3 class and attributes:
    m_function <- list(
      m_model = m_model,
      H_names = colnames(H),
      stage = K+1
    )
    class(m_function) <- "m_function"
    attr(m_function, "full_history") <- full_history
  }

  return(m_function)
}

#' @export
predict.m_function <- function(object, new_policy_data, ...) {
  K <- get_K(new_policy_data)
  ## check:
  if (get_element(object, "stage") != K+1){
    stop("The number of stages in m_function and new_policy_data does not match.")
  }

  ## getting the full history attribute:
  full_history <- attr(object, "full_history")

  ## creating the event history object:
  new_history <- get_history(new_policy_data,
                     full_history = full_history,
                     stage = K+1,
                     event_set = c(0,1,2))


  m_model <- getElement(object, "m_model")
  ## H_names <- getElement(object, "H_names")

  id_stage <- get_id_stage(new_history)
  new_H <- get_H(new_history)

  q_values <- id_stage
  set(q_values, j = "Q", value = predict(m_model, new_AH = new_H))

  return(q_values)
}
