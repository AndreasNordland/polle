#' Fit Single c-function
#'
#' \code{fit_c_function} is used to fit a single c-model.
#' @param history History object created by [get_history()]
#' @param c_model Censoring model/g-model created by ?? or similar functions.
#' @returns Object of class "g_function".
#' @noRd
fit_c_function <- function(history, c_model){
  browser()

  stage <- getElement(history, "stage")

  # getting the action (A) and the model matrix (H):
  event <- get_event(history)
  H <- get_H(history)

  # fitting the model:
  g_model <- g_model(A = A, H = H, action_set = stage_action_set)

  g_function <- list(
    g_model = g_model,
    H_names = colnames(H),
    action_set = action_set,
    stage_action_set = stage_action_set,
    stage = stage
  )
  class(g_function) <- "g_function"

  return(g_function)
}
