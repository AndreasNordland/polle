#' Fit Single c-function
#'
#' \code{fit_c_function} is used to fit a single c-model.
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
