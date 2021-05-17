library(polle)

d_obs0 <- function(stage, t, x, z, beta){
  prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
  rbinom(n = 1, size = 1, prob = prob)
}

args0 <- list(
  d = d_obs0,
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
  rho = -0.5 # Cox parameter for X_lead (the cost), if negative, the rate will decrease
)

# use_this ----------------------------------------------------------------

# set.seed(1)
# d <- simulate_multi_stage_data(2e3, args0)
# multi_stage_policy_data <- new_policy_data(stage_data = d$stage_data, baseline_data = d$baseline_data)
#
# usethis::use_data(multi_stage_policy_data, overwrite = TRUE)
