library(data.table)
sim_two_stage <- function(n=1e4,
                          par=c(gamma = 1,  beta = .5),
                          seed=NULL,
                          action_model_1 = function(C_1, beta, ...)
                            rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1)),
                          action_model_2 = function(C_2, beta, ...)
                            rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_2)),
                          deterministic_rewards = FALSE){
  if (!is.null(seed)) set.seed(seed)

  gamma <- par[1]
  beta <- par[2]

  # stage 1
  L_1 <- rnorm(n = n)
  C_1 <- L_1 + rnorm(n = n)
  U_1 <- L_1
  A_1 <- action_model_1(C_1 = C_1, L_1 = L_1, beta = beta, gamma = gamma)
  U_1_0 <- C_1 * 0
  U_1_1 <- C_1

  # stage 2
  L_2 <- rnorm(n = n, 0, 1)
  C_2 <- (gamma * L_1 + A_1) + rnorm(n = n)
  U_2 <- A_1 * C_1 + L_2
  A_2 <- action_model_2(C_1 = C_1, L_1 = L_1, C_2 = C_2, L_2 = L_2, A_1 = A_1, beta = beta, gamma = gamma)
  U_2_0 <- C_2 * 0
  U_2_1 <- C_2

  L_3 <- rnorm(n = n)
  U_3 <- A_2 * C_2 + L_3

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
    U_3 = U_3
  )
  if (deterministic_rewards == TRUE){
    data[, U_1_A0 := U_1_0]
    data[, U_1_A1 := U_1_1]
    data[, U_2_A0 := U_2_0]
    data[, U_2_A1 := U_2_1]
  }

  return(data)

}

kappa <- function(mu){
  Z <- 1 - pnorm(-mu)
  (1 - pnorm(q = -mu)) * (mu + dnorm(-mu) / Z)
}
