# TODO: bug if history object contains 1 observation:

d_obs <- function(stage, t, x, z, beta){
  prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
  rbinom(n = 1, size = 1, prob = prob)
}

d_1 <- function(stage, t, x, z, beta){
  return(1)
}

d_0 <- function(stage, t, x, z, beta){
  return(0)
}

d_1_stage_4_obs <- function(stage, t, x, z, beta){
  if (stage <= 4){
    out <- 1
  } else{
    prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
    out <- rbinom(n = 1, size = 1, prob = prob)
  }
  return(out)
}

args0 <- list(
  d = d_obs,
  tau = 10,
  lambda = 0, # exp(-lambda) = mean, rate = 1 / mean
  alpha =  c( # distribution of x
    0, # intercept
    0.5, # t,
    0.1, # t^2, x (the cost) will increase with t
    -0.5, # x_lead
    0.4 # z
  ),
  beta = c( # distribution of a
    0.3, # intercept
    -0.1, # t
    -0.1, # x
    0.3 # z == "a"
  ),
  sigma = 1,
  gamma = -0.1,
  psi = 1, # minimum time increment
  rho = -0.4 # Cox parameter for X_lead (the cost), if negative, the rate will decrease
)

# policies ----------------------------------------------------------------

# always_treat_stage_policy <- function(history){
#   pol <- history$AH[, c("id", "stage")]
#   pol[, d := "1"]
#   return(pol)
# }
#
# always_treat_policy <- policy_def(
#   stage_policies = always_treat_stage_policy,
#   full_history = FALSE,
#   replicate = TRUE
# )

# approximations ------------------------------------------------------
# n <- 5e5
#
# pd <- simulate_multi_stage_data(n, args0)
# pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
# true_value_observed <- mean(utility(pd)$U)
# rm(pd)
#
# args1 <- args0
# args1$d <- d_1
# pd1 <- simulate_multi_stage_data(n, args1)
# pd1 <- new_policy_data(stage_data = pd1$stage_data, baseline_data = pd1$baseline_data)
# true_value_always_treat <- mean(utility(pd1)$U)
# rm(pd1)
#
# args1 <- args0
# args1$d <- d_1_stage_4_obs
# pd1 <- simulate_multi_stage_data(n, args1)
# pd1 <- new_policy_data(stage_data = pd1$stage_data, baseline_data = pd1$baseline_data)
# true_value_always_treat_policy_partial_4 <- mean(utility(pd1)$U)
# rm(pd1)
# rm(args1)
# rm(n)

# ipw -------------------------------------------------

# # always treat policy:
# n <- 2e3
# set.seed(1)
# multi_stage_policy_data <- simulate_multi_stage_data(n, args0)
# multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
#
# tmp <- ipw(
#   multi_stage_policy_data,
#   g_models = new_g_glm(),
#   policy = always_treat_policy
# )
# tmp$value_estimate
# true_value_always_treat
# tmp$g_functions[[1]]
# args0$beta
# rm(tmp)
#
# set.seed(1)
# m <- 1e3
# n <- 2e3
# pb <- progress::progress_bar$new(
#                                format = " simulating [:bar] :percent eta: :eta",
#                                total = m, clear = FALSE, width= 60)
# estimate_always_treat <- vector(mode = "numeric", length = m)
# for (i in seq_along(estimate_always_treat)){
#
#   pd <- simulate_multi_stage_data(n, args0)
#   pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
#
#   tmp <- ipw(
#     pd,
#     g_models = new_g_glm(),
#     policy = always_treat_policy
#   )
#   estimate_always_treat[i] <- tmp$value_estimate
#   pb$tick()
#   rm(tmp)
# }
# mean(estimate_always_treat)
# sd(estimate_always_treat)
# true_value_always_treat

# dr -------------------------------------------------

# # always treat policy up to stage 4
# KK <- 4
# n <- 2e4
# set.seed(3)
# multi_stage_policy_data <- simulate_multi_stage_data(n, args0)
# multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
# multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)
#
# tmp <- dr(
#   multi_stage_policy_data,
#   g_models = new_g_glm(),
#   q_models = q_linear,
#   policy = always_treat_policy
# )
# tmp$value_estimate
# mean(tmp$phi_ipw)
# mean(tmp$phi_or)
# true_value_always_treat_policy_partial_4
#
# set.seed(1)
# m <- 1e2
# pb <- progress::progress_bar$new(
#   format = " simulating [:bar] :percent eta: :eta",
#   total = m, clear = FALSE, width= 60)
# estimate_always_treat_policy_partial_4 <- vector(mode = "numeric", length = m)
# for (i in 1:m){
#   pb$tick()
#   pd <- simulate_multi_stage_data(2e3, args0)
#   pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
#   pd <- partial(pd, K = KK)
#
#   tmp <- dr(
#     pd,
#     g_models = new_g_glm(),
#     q_models = q_linear,
#     policy = always_treat_policy
#   )
#   estimate_always_treat_policy_partial_4[i] <- tmp$value_estimate
#   rm(tmp)
# }
# mean(estimate_always_treat_policy_partial_4)
# sd(estimate_always_treat_policy_partial_4)
# true_value_always_treat_policy_partial_4

# Realistic Q-learning ----------------------------------------------------

# # always treat policy up to stage 4
# KK <- 4
# n <- 2e4
# set.seed(1)
# multi_stage_policy_data <- simulate_multi_stage_data(n, args0)
# multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
# multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)
#
# RQL_partial_4 <- rql(
#   multi_stage_policy_data,
#   alpha = 0.05,
#   g_models = new_g_glm(),
#   q_models = q_linear
# )
# RQL_partial_4$value_estimate
#
# set.seed(1)
# m <- 1e2
# n <- 2e3
# pb <- progress::progress_bar$new(
#   format = " simulating [:bar] :percent eta: :eta",
#   total = m, clear = FALSE, width= 60)
# estimate_RQL_partial_4 <- vector(mode = "numeric", length = m)
# for (i in 1:m){
#   pb$tick()
#   pd <- simulate_multi_stage_data(2e3, args0)
#   pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
#   pd <- partial(pd, K = KK)
#
#   tmp <- dr(
#     pd,
#     g_models = new_g_glm(),
#     q_models = q_linear,
#     policy = get_policy(RQL_partial_4)
#   )
#   estimate_RQL_partial_4[i] <- tmp$value
#   rm(tmp)
# }
# mean(estimate_RQL_partial_4)
# # true_value_observed
# # true_value_always_treat
# # true_value_always_treat_policy_partial_4

# policy tree -------------------------------------------------------------

# KK <- 4
# n <- 2e3
# set.seed(1)
# multi_stage_policy_data <- simulate_multi_stage_data(2e3, args0)
# multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
# multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)
#
# ptl_object <- ptl(
#   multi_stage_policy_data,
#   g_models = new_g_glm(),
#   q_models = q_linear,
#   policy_full_history = FALSE,
#   depth = 2
# )
#
# set.seed(1)
# m <- 1e1
# n <- 2e3
# pb <- progress::progress_bar$new(
#   format = " simulating [:bar] :percent eta: :eta",
#   total = m, clear = FALSE, width= 60)
# estimate_ptl_4 <- vector(mode = "numeric", length = m)
# for (i in 1:m){
#   pb$tick()
#   pd <- simulate_multi_stage_data(n, args0)
#   pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
#   pd <- partial(pd, K = KK)
#
#   tmp <- dr(
#     pd,
#     g_models = new_g_glm(),
#     q_models = q_linear,
#     policy = get_policy(ptl_object)
#   )
#   estimate_ptl_4[i] <- tmp$value_estimate
#   rm(tmp)
# }
# mean(estimate_ptl_4)
# # true_value_observed
# # true_value_always_treat_policy_partial_4

