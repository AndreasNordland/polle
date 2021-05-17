library(polle)

par0 <- list(
  mu_L = c(-1.4, 1.1, 1.3),
  mu_C = c(-2.1),
  gamma_C = c(-1.3,1.5),
  gamma_A = -1.2, # gamma_A must be negative
  sigma_L = 1.5,
  sigma_C = 2,
  alpha = 0.02
)

# Defining functions for sampling data
a_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  n <- length(C_1)
  a_1 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_1))

  return(a_1)
}

a_20 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  A_1 <- data$A_1
  C_2 <- data$C_2
  n <- length(C_1)
  a_2 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_2)) * A_1

  return(a_2)
}

d_alpha_opt_20 <- function(data, par){
  C_2 <- data$C_2
  A_1 <- data$A_1

  fit_2 <- lava::expit(par$gamma_A * C_2)
  ((fit_2 > 1 - par$alpha) + (fit_2 >= par$alpha & fit_2 <= 1 - par$alpha) * (C_2 + par$mu_L[3] > 0)) * A_1
}

d_alpha_opt_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1

  fit_1 <- lava::expit(par$gamma_A * C_1)
  out <- ((fit_1 >= par$alpha & fit_1 <= 1 - par$alpha) * (C_1 + par$mu_L[2] + kappa_1(l_1 = L_1, par = par) + kappa_2(l_1 = L_1, par = par)) + (fit_1 > 1 - par$alpha)) > 0

  out <- as.numeric(out)
  return(out)
}


# use_data ----------------------------------------------------------------

# set.seed(1)
# d <- simulate_two_stage_data(n = 1e63, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))])
# usethis::use_data(two_stage_policy_data, overwrite = TRUE)

