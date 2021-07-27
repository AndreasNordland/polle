library(ggplot2)
library(DTRlearn2)

a0 <- function(Z, L, B, par){
  kappa <- par$kappa
  xi <- par$xi
  n <- length(Z)
  rbinom(n, 1, lava::expit(kappa * (Z + L - 1) * Z^(-2) + (B == "a") * xi))
}
par0 <- list(
  kappa = 0.1,
  # kappa = 0,
  gamma = 3,
  alpha = 1,
  beta = -2.5,
  xi = 0.5
)

d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d); rm(d)

plot_data <- data.table(state_history(single_stage_policy_data)$H, U = utility(single_stage_policy_data)$U)
plot_data[, g_fit := lava::expit(par0$kappa * (Z + L - 1) * Z^(-2) + (B == "a") * par0$xi)]

ggplot(plot_data) +
  geom_point(aes(x = L, y = Z, color = U)) +
  facet_wrap(~A) +
  theme_bw()

ggplot(plot_data) +
  geom_point(aes(x = L, y = Z, color = g_fit)) +
  theme_bw()

ggplot(plot_data) +
  geom_point(aes(x = L, y = Z, color = A)) +
  theme_bw()

# policies ----------------------------------------------------------------

d_opt <- function(Z, L, B, par){
  gamma <- par$gamma
  alpha <- par$alpha
  beta <- par$beta

  as.numeric((gamma * Z + alpha * L + beta) > 0)
}
optimal_policy <- function(history){
  pol <- history$H
  pol[, d := as.character(d_opt(Z = Z, L = L, par = par0))]

  return(pol[, c("id", "stage", "d"), with = FALSE])
}
optimal_policy <- new_policy(
  stage_policies = optimal_policy,
  full_history = FALSE,
  replicate = TRUE
)


d_linear<- function(Z, L, ...){
as.numeric((Z + L - 1 > 0))
}

linear_policy <- function(history){
  pol <- history$H
  pol[, d := as.character(d_linear(Z = Z, L = L))]

  return(pol[, c("id", "stage", "d"), with = FALSE])
}
linear_policy <- new_policy(
  stage_policies = linear_policy,
  full_history = FALSE,
  replicate = TRUE
)

d_treat <- function(Z, L, ...){
  as.numeric(1)
}
treat_policy <- function(history){
  pol <- history$H
  pol[, d := as.character(1)]

  return(pol[, c("id", "stage", "d"), with = FALSE])
}
treat_policy <- new_policy(
  stage_policies = treat_policy,
  full_history = FALSE,
  replicate = TRUE
)


set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

plot_data <- data.table(state_history(single_stage_policy_data)$H, U = utility(single_stage_policy_data)$U)

plot_data[, d_opt := d_opt(Z = Z, L = L, par = par0)]
ggplot(plot_data) +
  geom_point(aes(x = L, y = Z, color = d_opt)) +
  geom_abline(intercept = -par0$beta/par0$gamma, slope = -par0$alpha/par0$gamma) +
  theme_bw()

plot_data[, d_linear := d_linear(Z = Z, L = L)]
ggplot(plot_data) +
  geom_point(aes(x = L, y = Z, color = d_linear)) +
  theme_bw()

head(linear_policy(single_stage_policy_data))

# approximations ----------------------------------------------------------

# d <- simulate_single_stage(n = 2e6, a = a0, par = par0)
# single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# observed_utility <- mean(utility(single_stage_policy_data)$U)
# rm(single_stage_policy_data)
#
# # approximated mean utility under the optimal policy
# n <- 2e6
# set.seed(1)
# d <- simulate_single_stage(n = n, par = par0, a = d_opt)
# simulate_single_stage <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# optimal_utility <- mean(utility(simulate_single_stage)$U)
# rm(simulate_single_stage)
#
# # approximated mean utility under the linear policy
# n <- 5e6
# set.seed(1)
# d <- simulate_single_stage(n = n, par = par0, a = d_linear)
# simulate_single_stage <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# linear_utility <- mean(utility(simulate_single_stage)$U)
# rm(simulate_single_stage)

always_treat_utility <- 0.5 + 0.5 + (par0$gamma * 0.5 + par0$alpha * 0.5 + par0$beta)

# g-models -----------------------------------------------------------------

# True g-model
g0 <- function(A, X){
  out <- list()
  class(out) <- "g0"

  return(out)
}
predict.g0 <- function(object, new_X){
  n <- nrow(new_X)

  stopifnot(
    all(colnames(new_X) == c("Z", "L", "B"))
  )

  Z <- new_X$Z
  L <- new_X$L
  B <- new_X$B

  fit <- lava::expit(par0$kappa * (Z + L - 1) * Z^(-2) + (B == "a") * par0$xi)

  preds <- matrix(
    c(
      1 - fit,
      fit
    ),
    ncol = 2
  )

  return(preds)
}

set.seed(2)
d <- simulate_single_stage(n = 2e4, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_history(single_stage_policy_data)

# data.table
get_X(history = his)

# g_model0 <- new_g_glm(formula = ~ .)
g_model0 <- new_g_glmnet(formula = ~ Z + L)
tmp <- fit_g_function(his, g_model = g_model0)
tmp2 <- fit_g_function(his, g_model = g0)

tmp$g_model$glm_model
tmp$g_model$glmnet_model$glmnet.fit$beta

# tmp$g_model$glm_model
# par0$kappa # correct if kappa = 0

evaluate(tmp, new_history = his)
evaluate(tmp2, new_history = his)

rm(his, tmp)

# test: missing levels
set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
d <- d[, if (any(B == "b", na.rm = TRUE)) .SD, id]
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_history(single_stage_policy_data)

# g_model0 <- new_g_glm(~ L + Z) # ~. will cause an error
g_model0 <- new_g_glmnet(~ L + Z) # ~. will cause an error
tmp <- fit_g_function(his, g_model = g_model0)
rm(his)

set.seed(2)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
new_his <- state_history(single_stage_policy_data)

evaluate(tmp, new_history = new_his)
rm(tmp, new_his, single_stage_policy_data)

set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_history(single_stage_policy_data)

# g_model0 <- new_g_glm()
g_model0 <- new_g_glmnet()
tmp <- fit_g_function(his, g_model = g_model0)
rm(his)

set.seed(2)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
d <- d[, if (any(B == "b", na.rm = TRUE)) .SD, id]
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
new_his <- state_history(single_stage_policy_data)

evaluate(tmp, new_history = new_his)
rm(tmp, new_his, single_stage_policy_data)

# IPW ---------------------------------------------------------------------

set.seed(1)
d <- simulate_single_stage(n = 2e5, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- ipw(
  single_stage_policy_data,
  g_models = new_g_glm(),
  policy = linear_policy,
  g_full_history = FALSE
)
tmp$value_estimate
linear_utility

# Q-models ----------------------------------------------------------------

# True Q-model
q0 <- function(V_res, AX){
  out <- list()
  class(out) <- "q0"

  return(out)
}
predict.q0 <- function(q_model, new_AX){
  n <- nrow(new_AX)

  stopifnot(
    all(colnames(new_AX) == c("A", "Z", "L", "B"))
  )

  Z <- new_AX[, colnames(new_AX) == "Z"]
  L <- new_AX[, colnames(new_AX) == "L"]

  fit_1 <- Z + L + (par0$gamma * Z + par0$alpha * L + par0$beta)
  fit_0 <- Z + L

  preds <- matrix(
    c(
      fit_0,
      fit_1
    ),
    ncol = 2
  )

  return(preds)
}

set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_stage_history(single_stage_policy_data, stage = 1)

V <- utility(single_stage_policy_data)$U
tmp <- fit_Q_function(
  his,
  V,
  # q_model = new_q_glm(~Z + L)
  # q_model = new_q_glm()
  q_model = q0
)
tmp$q_model

evaluate.Q_function(tmp, new_history = his)

# V <- utility(single_stage_policy_data)$U
# tmp <- fit_Q_function(
#   his,
#   V,
#   q_model = new_q_glmnet()
# )
# tmp$q_model$glmnet_model
# tmp$q_model$glmnet_model$nzero
# coef(tmp$q_model$glmnet_model, s = "lambda.min")
# par0
# rm(tmp, V)
# rm(his)

# OR ----------------------------------------------------------------------

# set.seed(2)
# d <- simulate_single_stage(n = 2e5, a = a0, par = par0)
# single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# tmp <- or(
#   policy_data = single_stage_policy_data,
#   q_models = new_q_glm(),
#   policy = linear_policy,
#   q_full_history = FALSE
# )
# tmp$value_estimate
# linear_utility
# rm(tmp)

# DR ----------------------------------------------------------------------

n <- 2e4
set.seed(2)
d <- simulate_single_stage(n = n, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- dr(
  single_stage_policy_data,
  q_models = new_q_glm(),
  g_models = new_g_glm(),
  policy = treat_policy
)
tmp$value_estimate

tmp$g_functions[[1]]$g_model
tmp$q_functions[[1]]$q_model

sd(tmp$phi_or)
sd(tmp$phi_ipw)
sd(tmp$phi_dr)

mean(tmp$phi_ipw)
mean(tmp$phi_or)
rm(tmp)

# cross-fitting:
tmp <- cv_dr(
  single_stage_policy_data,
  q_models = new_q_glm(),
  g_models = new_g_glm(),
  policy = linear_policy,
  M = 3,
  mc.cores = 3
)
mean(tmp$phi_dr)

# coverage
res <- replicate(
  1e3,
  expr = {
    n <- 2e3
    d <- simulate_single_stage(n = n, a = a0, par = par0)
    sspd <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
    ssdr <- dr(
      sspd,
      q_models = new_q_glm(),
      g_models = new_g_glm(),
      policy = treat_policy
    )

    out <- list(
      est = ssdr$value_estimate,
      est_var = mean((ssdr$phi_dr - ssdr$value_estimate)^2),
      n = 2e3
    )
    return(out)
  }
)

cov_res <- apply(
  res,
  MARGIN = 2,
  FUN = function(x){
    coef_inter <- x[[1]] + c(-1,1) * 1.96 * sqrt(x[[2]]) / sqrt(x[[3]])
    (always_treat_utility <= coef_inter[2] & always_treat_utility >= coef_inter[1])
  }
)
mean(cov_res)

# OWL ---------------------------------------------------------------------

# set.seed(14)
# d <- simulate_single_stage(n = 500, a = a0, par = par0)
# single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# # single_stage_history <- state_history(single_stage_policy_data)
# # H <- polle:::get_X.history(single_stage_history)
# # H <- scale(H)
# # AA <- as.numeric(polle:::get_A.history(single_stage_history))
# # AA[AA == 0] <- -1
# # RR <- utility(single_stage_policy_data)$U
# #
# # Z <- single_stage_history$H$Z
# # L <- single_stage_history$H$L
# # pi <-lava::expit(par0$kappa * (Z + L - 1) * Z^(-2))
# #
# # single_stage_owl_test <- owl(H = H, AA = AA, RR = RR, pi = pi, K = 1, n = length(RR))
# # single_stage_owl_test$valuefun # sensitive to the scaling of H
# # optimal_utility
# # head(single_stage_owl_test$pi[[1]])
# # single_stage_owl_test$type
# # # DTRlearn2:::predict.owl_svmlinear
# # single_stage_owl_results <- single_stage_owl_test$stage
# # beta0 <- single_stage_owl_results$beta0 # intercept
# # beta <- single_stage_owl_results$beta # coefficients
# # # owl fit
# # owl_fit <- beta0 + H %*% beta
# # d_opt_owl <- sign(owl_fit)
# # pred_owl <- predict.owl(single_stage_owl_test, H = H, K = 1)
# # all(pred_owl$treatment[[1]] == d_opt_owl)
# # plot_data <- data.table(state_history(single_stage_policy_data)$H, U = utility(single_stage_policy_data)$U)
# # plot_data[, d_opt_owl := d_opt_owl]
# #
# # ggplot(plot_data) +
# #   geom_point(aes(x = L, y = Z, color = d_opt_owl)) +
# #   geom_abline(intercept = -par0$beta/par0$gamma, slope = -par0$alpha/par0$gamma) +
# #   theme_bw()
#
# single_stage_owl <- bowl(
#   single_stage_policy_data,
#   g_models = g0,
#   g_full_history = FALSE,
#   policy_full_history = FALSE
# )
# single_stage_owl_policy <- get_policy(single_stage_owl)
#
# set.seed(2)
# d <- simulate_single_stage(n = 1e3, a = a0, par = par0)
# single_stage_policy_data_new <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# plot_data <- state_history(single_stage_policy_data_new)$H
# plot_data <- merge(plot_data, single_stage_owl_policy(single_stage_policy_data_new), all.x = TRUE)
# optimal_policy_actions <- optimal_policy(single_stage_policy_data_new)
# setnames(optimal_policy_actions, "d", "d_opt")
# plot_data <- merge(plot_data, optimal_policy_actions, all.x = TRUE)
#
# # seems biased, consistently below the optimal line. Has it something to do with the scaling?
# ggplot(plot_data) +
#   geom_point(aes(x = L, y = Z, color = d)) +
#   geom_abline(intercept = -par0$beta/par0$gamma, slope = -par0$alpha/par0$gamma) +
#   theme_bw()

# PLT ---------------------------------------------------------------------

# n <- 1e4
# set.seed(3)
# d <- simulate_single_stage(n = n, a = a0, par = par0)
# single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# single_stage_policy_tree <- ptl(
#   policy_data = single_stage_policy_data,
#   g_models = g0,
#   q_models = q0,
#   g_full_history = FALSE,
#   q_full_history = FALSE,
#   policy_full_history = FALSE
# )
#
# policy_tree_policy <- get_policy(single_stage_policy_tree)
# rm(single_stage_policy_data)
#
# set.seed(54353)
# d <- simulate_single_stage(n = 1e3, a = a0, par = par0)
# single_stage_policy_data_new <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# plot_data <- state_history(single_stage_policy_data_new)$H
# plot_data <- merge(plot_data, policy_tree_policy(single_stage_policy_data_new), all.x = TRUE)
#
# library(ggplot2)
#
# ggplot(plot_data) +
#   geom_point(aes(x = L, y = Z, color = d)) +
#   geom_abline(intercept = -par0$beta/par0$gamma, slope = -par0$alpha/par0$gamma) +
#   theme_bw()

