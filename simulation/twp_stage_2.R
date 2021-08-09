library(data.table)

par0 <- list(
  gamma = 0.5,
  beta = 1
)

kappa <- function(mu){
  Z <- 1 - pnorm(-mu)
  (1 - pnorm(q = -mu)) * (mu + dnorm(-mu) / Z)
}

a_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  n <- length(C_1)
  a_1 <- rbinom(n = n, size = 1, prob = lava::expit(par$beta * C_1))

  return(a_1)
}

a_20 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  A_1 <- data$A_1
  C_2 <- data$C_2
  n <- length(C_1)
  a_2 <- rbinom(n = n, size = 1, prob = lava::expit(par$beta * C_2))

  return(a_2)
}

m2 <- function(n, par, a_1, a_2){
  # stage 1
  L_1 <- rnorm(n = n)
  C_1 <- L_1 + rnorm(n = n)
  U_1 <- L_1
  A_1 <- a_1(data.table(C_1 = C_1, L_1 = L_1, U_1 = U_1), par)
  U_1_0 <- C_1 * 0
  U_1_1 <- C_1

  # stage 2
  L_2 <- rnorm(n = n, 0, 1)
  C_2 <- (par$gamma * L_1 + A_1) + rnorm(n = n)
  U_2 <- A_1 * C_1 + L_2
  A_2 <- a_2(data.table(C_1 = C_1, L_1 = L_1, A_1 = A_1, L_2 = L_2, C_2 = C_2, U_1 = U_1, U_2 = U_2), par)
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
    U_1_0 = U_1_0,
    U_1_1 = U_1_1,
    U_2 = U_2,
    U_2_0 = U_2_0,
    U_2_1 = U_2_1,
    U_3 = U_3
  )
  return(data)
}

# data20 <- m2(n = 2e5, par = par0, a_1 = a_10, a_2 = a_20)
# pd2 <- policy_data(data20,
#                   action = c("A_1", "A_2"),
#                   covariates = list(L = c("L_1", "L_2"),
#                                     C = c("C_1", "C_2")),
#                   utility = c("U_1", "U_2", "U_3"),
#                   deterministic_utility = list(
#                     U_0 = c("U_1_0", "U_2_0"),
#                     U_1 = c("U_1_1", "U_2_1")))
# pd2

d_opt_2_2 <- function(data, par){
  C_2 <- data[["C_2"]]

  (C_2 > 0) * 1
}

d_opt_2_1 <- function(data, par){
  C_1 <- data[["C_1"]]
  L_1 <- data[["L_1"]]

  mu_1 <- (par$gamma * L_1 + 1)
  mu_0 <- (par$gamma * L_1 + 0)

  ((C_1 + kappa(mu_1) - kappa(mu_0)) > 0) * 1
}

data_opt_2_0 <- m2(n = 2e6, par = par0, a_1 = d_opt_2_1, a_2 = d_opt_2_2)
data_opt_2_0[, .(mean(U_1 + U_2 + U_3))]

pd_2_opt <- policy_data(data_opt_2_0,
                  action = c("A_1", "A_2"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"),
                  deterministic_utility = list(
                    U_0 = c("U_1_0", "U_2_0"),
                    U_1 = c("U_1_1", "U_2_1")))
pd_2_opt

pol_opt <- policy_def(
  list(
    d_1 = dynamic_policy(function(C_1, L_1, ...){
      mu_1 <- (par0$gamma * L_1 + 1)
      mu_0 <- (par0$gamma * L_1 + 0)
      ((C_1 + kappa(mu_1) - kappa(mu_0)) > 0) * 1}),
    d_2 = dynamic_policy(function(C_2, ...){
      (C_2 > 0) * 1
    })
  )
)

# TODO check class policy_data
pe <- policy_eval(
  pd,
  policy = pol_opt,
  g_models = g_glm(),
  q_models = q_glm()
)
pe

# rql
pe_rql <- policy_eval(
  pd,
  policy_learner = policy_learn(type = "rql"),
  g_models = g_glm(),
  q_models = q_glm()
)
pe_rql

pe_rql_actions <- get_policy(pe_rql$policy_object)(pd)

# policy_tree
data_2_0 <- m2(n = 2e5, par = par0, a_1 = a_10, a_2 = a_20)
mean(data_2_0$C_2)
lm(C_2 ~ L_1 + A_1, data = data0)
pd_2_0 <- policy_data(data_2_0,
                   action = c("A_1", "A_2"),
                   covariates = list(L = c("L_1", "L_2"),
                                     C = c("C_1", "C_2")),
                   utility = c("U_1", "U_2", "U_3"),
                   deterministic_utility = list(
                     U_0 = c("U_1_0", "U_2_0"),
                     U_1 = c("U_1_1", "U_2_1")))
pe_ptl <- policy_eval(
  pd0,
  policy_learner = policy_learn(type = "ptl"),
  g_models = g_glm(),
  q_models = q_glm()
)
pe_ptl

d_1_ptl <- function(data, par){c(0,1)[predict(pe_ptl$policy_object$ptl_objects[[1]], newdata = data.frame(L = data$L_1, C = data$C_1))]}
d_2_ptl <- function(data, par){c(0,1)[predict(pe_ptl$policy_object$ptl_objects[[2]], newdata = data.frame(L = data$L_2, C = data$C_2))]}

data_ptl0 <- m2(n = 2e4, par = par0, a_1 = d_1_ptl, a_2 = d_2_ptl)
pd_ptl0 <- policy_data(data_ptl0,
                   action = c("A_1", "A_2"),
                   covariates = list(L = c("L_1", "L_2"),
                                     C = c("C_1", "C_2")),
                   utility = c("U_1", "U_2", "U_3"),
                   deterministic_utility = list(
                     U_0 = c("U_1_0", "U_2_0"),
                     U_1 = c("U_1_1", "U_2_1")))
mean(utility(pd_ptl0)$U)
