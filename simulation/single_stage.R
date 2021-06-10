library(ggplot2)
library(DTRlearn2)

a0 <- function(Z, L, par){
  kappa <- par$kappa
  n <- length(Z)
  rbinom(n, 1, lava::expit(kappa * (Z + L - 1) * Z^(-2)))
}
par0 <- list(
  # kappa = 0.1,
  kappa = 0,
  gamma = 3,
  alpha = 1,
  beta = -2.5
)

d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d); rm(d)

plot_data <- data.table(state_history(single_stage_policy_data)$H, U = utility(single_stage_policy_data)$U)
plot_data[, g_fit := lava::expit(par0$kappa * (Z + L - 1) * Z^(-2))]

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

d_opt <- function(Z, L, par){
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

d <- simulate_single_stage(n = 2e6, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
observed_utility <- mean(utility(single_stage_policy_data)$U)
rm(single_stage_policy_data)

# approximated mean utility under the optimal policy
n <- 2e6
set.seed(1)
d <- simulate_single_stage(n = n, par = par0, a = d_opt)
simulate_single_stage <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
optimal_utility <- mean(utility(simulate_single_stage)$U)
rm(simulate_single_stage)

# approximated mean utility under the linear policy
n <- 5e6
set.seed(1)
d <- simulate_single_stage(n = n, par = par0, a = d_linear)
simulate_single_stage <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
linear_utility <- mean(utility(simulate_single_stage)$U)
rm(simulate_single_stage)

# g-models -----------------------------------------------------------------

# binomial model
g_binomial_linear <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ ., data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "g_binomial"
  return(bm)
}

# g-model with intercept
g_binomial_intercept <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ 1, data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "g_binomial"
  return(bm)
}

predict.g_binomial <- function(object, new_X){
  glm_model <- object$glm_model

  # model matrix as data.frame
  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")

  probs <- cbind((1-fit), fit)

  return(probs)
}

g0 <- function(A, X){
  out <- list()
  class(out) <- "g0"

  return(out)
}
predict.g0 <- function(object, new_X){
  n <- nrow(new_X)

  stopifnot(
    all(colnames(new_X) == c("Z", "L"))
  )

  Z <- new_X[, colnames(new_X) == "Z"]
  L <- new_X[, colnames(new_X) == "L"]

  fit <- lava::expit(par0$kappa * (Z + L - 1) * Z^(-2))

  preds <- matrix(
    c(
      1 - fit,
      fit
    ),
    ncol = 2
  )

  return(preds)
}

set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_history(single_stage_policy_data)

tmp <- fit_g_function(his, g_model = g_binomial_linear)
tmp$g_model$glm_model
par0$kappa # correct if kappa = 0
rm(tmp)

head(predict.G_function(G, new_history = his))
rm(his)

# IPW ---------------------------------------------------------------------

set.seed(1)
d <- simulate_single_stage(n = 2e3, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- ipw(
  single_stage_policy_data,
  g_models = g_binomial_linear,
  policy = linear_policy,
  g_full_history = FALSE
)
tmp$value
linear_utility

# Q-models ----------------------------------------------------------------

Q_linear <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ A * ., data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "Q_linear"
  return(m)

  return(m)
}

Q_interept <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ 1, data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "Q_linear"
  return(m)

  return(m)
}

predict.Q_linear <- function(object, new_A, new_X){
  lm_model <- object$lm_model

  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }

  data <- cbind(A = new_A, new_X)
  pred <- predict(lm_model, newdata = data, type = "response")

  return(pred)
}

q0 <- function(V_res, A, X){
  out <- list()
  class(out) <- "q0"

  return(out)
}
predict.q0 <- function(q_model, new_A, new_X){
  n <- nrow(new_X)

  stopifnot(
    all(colnames(new_X) == c("Z", "L"))
  )

  Z <- new_X[, colnames(new_X) == "Z"]
  L <- new_X[, colnames(new_X) == "L"]

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
d <- simulate_single_stage(n = 2e4, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
his <- state_stage_history(single_stage_policy_data, stage = 1)

V <- utility(single_stage_policy_data)$U
tmp <- fit_Q_function(
  his,
  V,
  q_model = Q_linear
)
tmp$q_model$lm_model
par0
rm(tmp, V)
rm(his)

# OR ----------------------------------------------------------------------

set.seed(1)
d <- simulate_single_stage(n = 2e5, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- or(
  single_stage_policy_data,
  q_models = Q_linear,
  policy = linear_policy,
  q_full_history = FALSE
)
tmp$value_estimate
linear_utility
rm(tmp)

# DR ----------------------------------------------------------------------

set.seed(1)
d <- simulate_single_stage(n = 1e4, a = a0, par = par0)
single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- dr(
  single_stage_policy_data,
  q_models = Q_linear,
  g_models = g_binomial_linear,
  policy = linear_policy
)

tmp$value_estimate
mean(tmp$phi_ipw)
mean(tmp$phi_or)
linear_utility

rm(tmp, single_stage_policy_data)

# OWL ---------------------------------------------------------------------

# DTRlearn2:::owl_single
# DTRlearn2:::wsvm_solve # uses ipop from kernlab

# set.seed(14)
# d <- simulate_single_stage(n = 500, a = a0, par = par0)
# single_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

# single_stage_history <- state_history(single_stage_policy_data)
# H <- polle:::get_X.history(single_stage_history)
# H <- scale(H)
# AA <- as.numeric(polle:::get_A.history(single_stage_history))
# AA[AA == 0] <- -1
# RR <- utility(single_stage_policy_data)$U
#
# Z <- single_stage_history$H$Z
# L <- single_stage_history$H$L
# pi <-lava::expit(par0$kappa * (Z + L - 1) * Z^(-2))
#
# single_stage_owl_test <- owl(H = H, AA = AA, RR = RR, pi = pi, K = 1, n = length(RR))
# single_stage_owl_test$valuefun # sensitive to the scaling of H
# optimal_utility
# head(single_stage_owl_test$pi[[1]])
# single_stage_owl_test$type
# # DTRlearn2:::predict.owl_svmlinear
# single_stage_owl_results <- single_stage_owl_test$stage
# beta0 <- single_stage_owl_results$beta0 # intercept
# beta <- single_stage_owl_results$beta # coefficients
# # owl fit
# owl_fit <- beta0 + H %*% beta
# d_opt_owl <- sign(owl_fit)
# pred_owl <- predict.owl(single_stage_owl_test, H = H, K = 1)
# all(pred_owl$treatment[[1]] == d_opt_owl)
# plot_data <- data.table(state_history(single_stage_policy_data)$H, U = utility(single_stage_policy_data)$U)
# plot_data[, d_opt_owl := d_opt_owl]
#
# ggplot(plot_data) +
#   geom_point(aes(x = L, y = Z, color = d_opt_owl)) +
#   geom_abline(intercept = -par0$beta/par0$gamma, slope = -par0$alpha/par0$gamma) +
#   theme_bw()

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

# set.seed(3)
# d <- simulate_single_stage(n = 1e4, a = a0, par = par0)
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

