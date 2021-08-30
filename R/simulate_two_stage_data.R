
#' simulate_two_stage_data_wide <- function(n, par, a_1, a_2){
#' # stage 1
#' L_1 <- par$mu_L[1] + rnorm(n = n, 0, par$sigma_L)
#' C_1 <- par$gamma_C[1] * L_1 + par$mu_C[1] + rnorm(n = n, 0, par$sigma_C)
#' U_1 <- L_1
#' A_1 <- a_1(data.table(C_1 = C_1, L_1 = L_1, U_1 = U_1), par)
#'
#'
#' # stage 2
#' L_2 <- (par$mu_L[2] + rnorm(n = n, 0, par$sigma_L))
#' C_2 <- (par$gamma_C[2] * L_1 + rnorm(n = n, 0, par$sigma_C))
#' U_2 <- A_1 * (C_1 + L_2)
#' A_2 <- a_2(data.table(C_1 = C_1, L_1 = L_1, A_1 = A_1, L_2 = L_2, C_2 = C_2, U_1 = U_1, U_2 = U_2), par)
#'
#' L_2 <- ifelse(A_1 == 1, L_2, NA)
#' C_2 <- ifelse(A_1 == 1, C_2, NA)
#' A_2 <- ifelse(A_1 == 1, A_2, NA)
#'
#' L_3 <- A_2 * (par$mu_L[3] + rnorm(n = n, 0, par$sigma_L))
#' U_3 <- A_2 * (C_2 + L_3)
#' L_3 <- ifelse(A_2 == 1, L_3, NA)
#'
#' data <- data.table(
#'   L_1 = L_1,
#'   C_1 = C_1,
#'   A_1 = A_1,
#'   L_2 = L_2,
#'   C_2 = C_2,
#'   A_2 = A_2,
#'   L_3 = L_3,
#'   U_1 = U_1,
#'   U_2 = U_2,
#'   U_3 = U_3
#' )
#'
#' return(data)
#' }
#'
#' #' @export
#' simulate_two_stage_data <- function(n, par, a_1, a_2){
#'   wide_data <- simulate_two_stage_data_wide(n = n, par = par, a_1 = a_1, a_2 = a_2)
#'   A_cols <- c("A_1", "A_2")
#'   X_cols <- list(
#'     L = c("L_1", "L_2"),
#'     C = c("C_1", "C_2")
#'   )
#'   X_value_names <- c("L", "C")
#'   U_cols <- c("U_1", "U_2", "U_3")
#'   long_data <- wide_stage_data_to_long(wide_data, A_cols = A_cols, X_cols = X_cols, U_cols = U_cols)
#'
#'   long_data[event == 0, ("U_0") := 0]
#'   long_data[event == 0, ("U_1") := C]
#'   setindex(long_data, NULL)
#'
#'   return(long_data)
#' }
#'
#' E_truncated_gauss <- function(mu, sigma, a, b){
#'   gamma <- (a - mu) / sigma
#'   alpha <- (b - mu) / sigma
#'   Z <- pnorm(alpha) - pnorm(gamma)
#'   if (any(Z == 0))
#'     stop("parameter gamma_A too high.")
#'
#'   mu + (dnorm(gamma) - dnorm(alpha)) / Z * sigma
#' }
#'
#' kappa_1 <- function(l_1, par) {
#'   stopifnot(
#'     !(par$alpha >= 0.5 | par$alpha < 0)
#'   )
#'
#'   a <- max(lava::logit(1- par$alpha) / par$gamma_A, -par$mu_L[3])
#'   b <- lava::logit(par$alpha) / par$gamma_A
#'   if (a > b) {
#'     out <- 0
#'   } else{
#'     mu <- par$gamma_C[2] * l_1
#'     sigma <- par$sigma_C
#'     out <- (pnorm(q = b, mean = mu, sd = sigma) - pnorm(q = a, mean = mu, sd = sigma)) * (E_truncated_gauss(mu = mu, sigma = sigma, a = a, b = b) + par$mu_L[3])
#'   }
#'
#'   return(out)
#' }
#'
#' kappa_2 <- function(l_1, par){
#'   stopifnot(
#'     !(par$alpha >= 0.5 | par$alpha < 0)
#'   )
#'
#'   if (par$alpha > 0) {
#'     b <- lava::logit(1 - par$alpha) / par$gamma_A
#'     a <- -Inf
#'     mu <- par$gamma_C[2] * l_1
#'     sigma <- par$sigma_C
#'     out <- pnorm(q = b, mean = mu, sd = sigma) * (E_truncated_gauss(mu = mu, sigma = sigma, a = a, b = b) + par$mu_L[3])
#'   } else {
#'     out <- 0
#'   }
#'
#'   return(out)
#' }
