
library(polle)
library(data.table)
library(lava)
library(future.apply)



par0 <- list(a = 0, b = 0, c = 0) # no treatment effect
sim_d <- function(n,
                  par = par0,
                  potential_outcomes = FALSE,
                  action_function = function(H){
                    n <- nrow(H)
                    rbinom(n = n, size = 1, prob = 0.5)
                  }) {
  W <- runif(n = n, min = -1, max = 1)
  L <- runif(n = n, min = -1, max = 1)
  H <- data.table(W = W, L = L)
  A <- as.numeric(action_function(H))
  U1 <- W + L + (par$c*W + par$a*L + par$b) # U^1
  U0 <- W + L # U^0
  U <- A * U1 + (1 - A) * U0 + rnorm(n = n)
  out <- data.table(W = W, L = L, A = A, U = U)
  if (potential_outcomes == TRUE) {
    out$U0 <- U0
    out$U1 <- U1
  }
  return(out)
}
pl <- policy_learn(
  type = "blip",
  control = control_blip(blip_models = q_glm(~ W + L), quantile_prob_threshold = 0.5),
  threshold=NULL
)



onerun_cross <- function(n = 1e3, par = par0) {

  d <- sim_d(n = n, par = par, potential_outcomes = F)
  pd <- policy_data(
    d,
    action = "A",
    covariates = list("W", "L"),
    utility = "U"
  )
  pes <- policy_eval(
    policy_data = pd,
    policy_learn = pl,
    target = "value",
    g_models = g_glm(~1),
    q_models = q_glm(),
    M = 5
  )

  est <- estimate(pes)
  est <- estimate(est, null = 0)

  se <- sqrt(vcov(est))

  out <- c(
    est = coef(est)[[1]],
    se = se,
    p = est$coefmat[1, 'P-value']
  )
  return(out)
}

test_that("policy_eval does NOT have the correct coverage for a non-converging poluicy (under the null).", {
  set.seed(1)
  plan("multicore")
  ## library(progressr)
  ## handlers(global = TRUE)
  R <- 1e3
  res_cross <- sim(onerun_cross, R = R, args = list(par = par0), seed = TRUE)
  plan("sequential")
  summar_cross <- summary(res_cross, estimate=c(1),se=c(2), true = 0)

  ratio.delta <- 0.1
  expect_true(abs(1-summar_cross["SE/SD", ]) > ratio.delta)

})


onerun_online <- function(n = 1e3, par = par0) {

  d <- sim_d(n = n, par = par, potential_outcomes = F)
  pd <- policy_data(
    d,
    action = "A",
    covariates = list("W", "L"),
    utility = "U"
  )
  pes <- policy_eval_online(
    policy_data = pd,
    policy_learn = pl,
    target = "value",
    g_models = g_glm(~1),
    q_models = q_glm(),
    M = 5
  )

  est <- estimate(pes)
  est <- estimate(est, null = 0)

  se <- sqrt(vcov(est))

  out <- c(
    est = coef(est)[[1]],
    se = se,
    p = est$coefmat[1, 'P-value']
  )
  return(out)
}

test_that("policy_eval_online does have the correct coverage for a non-converging policy (under the null).", {
  set.seed(1)
  plan("multicore")
  ## library(progressr)
  ## handlers(global = TRUE)
  R <- 1e3
  res_online <- sim(onerun_online, R = R, args = list(par = par0), seed = TRUE)
  plan("sequential")
  summar_online <- summary(res_online, estimate=c(1),se=c(2), true = 0)

  ratio.delta <- 0.01
  expect_true(abs(1-summar_online["SE/SD", ]) < ratio.delta)

})
