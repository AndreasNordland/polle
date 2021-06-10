#' @export
simulate_single_stage <- function(n, a, par){
  u <- function(Z, L, A, par){
    n <- length(Z)
    gamma <- par$gamma
    alpha <- par$alpha
    beta <- par$beta

    Z + L + A*(gamma * Z + alpha * L + beta) + rnorm(n)
  }

  Z <- runif(n)
  L <- runif(n)
  A <- a(Z = Z, L = L, par = par)
  U <- u(Z = Z, L = L, A = A, par = par)

  stage_data_1 <- data.table(id = 1:n, stage = 1, event = 0, Z = Z, L = L, A = as.character(A), U = 0, U_0 = 0, U_1 = 0)
  stage_data_2 <- data.table(id = 1:n, stage = 2, event = 1, U =  U)

  stage_data <- rbindlist(list(stage_data_1, stage_data_2), fill = TRUE)

  return(stage_data)
}
