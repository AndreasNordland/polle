#' @import data.table

#' @export
simulate_two_stage_data <- function(n, par, a_1, a_2){
  # stage 1
  L_1 <- par$mu_L[1] + rnorm(n = n, 0, par$sigma_L)
  C_1 <- par$gamma_C[1] * L_1 + par$mu_C[1] + rnorm(n = n, 0, par$sigma_C)
  U_1 <- L_1
  A_1 <- a_1(data.table(C_1 = C_1, L_1 = L_1, U_1 = U_1), par)


  # stage 2
  L_2 <- A_1 * (par$mu_L[2] + rnorm(n = n, 0, par$sigma_L))
  C_2 <- A_1 * (par$gamma_C[2] * L_1 + rnorm(n = n, 0, par$sigma_C))
  U_2 <- A_1 * (C_1 + L_2)
  A_2 <- a_2(data.table(C_1 = C_1, L_1 = L_1, A_1 = A_1, L_2 = L_2, C_2 = C_2, U_1 = U_1, U_2 = U_2), par)

  L_3 <- A_2 * (par$mu_L[3] + rnorm(n = n, 0, par$sigma_L))
  U_3 <- A_2 * (C_2 + L_3)

  # utility
  U <- U_1 + U_2 + U_3

  data <- data.table(
    L_1 = L_1,
    C_1 = C_1,
    A_1 = A_1,
    L_2 = L_2,
    C_2 = C_2,
    A_2 = A_2,
    L_3 = L_3,
    U_1 = U_1,
    U_2 = U_2,
    U_3 = U_3,
    U = U
  )

  data[, id := 1:.N]
  col_a <- c("A_1", "A_2")
  col_l <- c("L_1", "L_2", "L_3")
  col_c <- c("C_1", "C_2")
  col_u <- c("U_1", "U_2", "U_3")
  stage_data <- melt(data, id.vars = c("id"), measure = list(col_a, col_l, col_c, col_u), value.name = c("A", "L", "C", "U"), variable.name = "stage")
  stage_data[ , stage := as.numeric(as.character(stage))]
  stage_data[, A := as.character(A)]
  # set key
  setkey(stage_data, id, stage)

  # calculating U_1 and U_0
  stage_data[, U_1 := C]
  stage_data[, U_0 := 0]
  # remove rows
  stage_data[, L_lead := shift(L), id]
  stage_data <- stage_data[!(stage == 3 & (L_lead == 0))]
  stage_data[, L_lead := NULL]

  stage_data[stage == 1, event := 0]
  stage_data[(stage == 2) & (A == "1"), event := 0]
  stage_data[(stage == 2) & (A == "0") & (L != 0), event := 0]
  stage_data[(stage == 2) & (A == "0") & (L == 0), event := 1]
  stage_data[stage == 3, event := 1]

  return(stage_data)
}

E_truncated_gauss <- function(mu, sigma, a, b){
  gamma <- (a - mu) / sigma
  alpha <- (b - mu) / sigma
  Z <- pnorm(alpha) - pnorm(gamma)

  mu + (dnorm(gamma) - dnorm(alpha)) / Z * sigma
}

kappa_1 <- function(l_1, par) {
  stopifnot(
    !(par$alpha >= 0.5 | par$alpha < 0)
  )

  a <- max(lava::logit(1- par$alpha) / par$gamma_A, -par$mu_L[3])
  b <- lava::logit(par$alpha) / par$gamma_A
  if (a > b) {
    out <- 0
  } else{
    mu <- par$gamma_C[2] * l_1
    sigma <- par$sigma_C
    out <- (pnorm(q = b, mean = mu, sd = sigma) - pnorm(q = a, mean = mu, sd = sigma)) * (E_truncated_gauss(mu = mu, sigma = sigma, a = a, b = b) + par$mu_L[3])
  }

  return(out)
}

kappa_2 <- function(l_1, par){
  stopifnot(
    !(par$alpha >= 0.5 | par$alpha < 0)
  )

  if (par$alpha > 0) {
    b <- lava::logit(1 - par$alpha) / par$gamma_A
    a <- -Inf
    mu <- par$gamma_C[2] * l_1
    sigma <- par$sigma_C
    out <- pnorm(q = b, mean = mu, sd = sigma) * (E_truncated_gauss(mu = mu, sigma = sigma, a = a, b = b) + par$mu_L[3])
  } else {
    out <- 0
  }

  return(out)
}
