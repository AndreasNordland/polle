library(data.table)

sim_two_stage_multi_actions <- function(n=1e3,
                                        par=list(gamma = 0.5,  beta = 1, prob = c(0.2, 0.4, 0.4)),
                                        seed=NULL,
                                        action_model_1 = function(C_1, beta, ...)
                                          rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1)),
                                        deterministic_rewards = FALSE){
  if (!is.null(seed)) set.seed(seed)

  gamma <- getElement(par, "gamma")
  beta <- getElement(par, "beta")
  prob <- getElement(par, "prob")

  # baseline
  B <- rnorm(n = n)
  BB <- sample(c("group1", "group2", "group3"), n, replace = TRUE)

  # stage 1
  L_1 <- rnorm(n = n)
  C_1 <- L_1 + rnorm(n = n)
  U_1 <- L_1
  a_1 <- action_model_1(C_1 = C_1, L_1 = L_1, beta = beta, gamma = gamma)
  A_1 <- ifelse(a_1, "yes", "no")
  U_1_0 <- C_1 * 0
  U_1_1 <- C_1

  # stage 2
  L_2 <- rnorm(n = n, 0, 1)
  C_2 <- (gamma * L_1 + a_1) + rnorm(n = n)
  U_2 <- a_1 * C_1 + L_2
  A_2 <- sample(c("yes", "no", "default"), prob = prob, size = n, replace = TRUE)
  U_2_0 <- C_2 * 0
  U_2_1 <- C_2

  L_3 <- rnorm(n = n)
  U_3 <- (A_2 == "yes") * C_2 + L_3

  data <- data.table(
    B = B,
    BB = BB,
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

  return(data)

}
