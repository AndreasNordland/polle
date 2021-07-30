library(polle)
library(data.table)

par0 <- list(
  mu_L = c(-1.4, 1.1, 1.3),
  mu_C = c(-2.1),
  gamma_C = c(-1.3,1.5),
  # gamma_A = -1, # gamma_A must be negative
  gamma_A = -0.02,
  sigma_L = 1.5,
  sigma_C = 2,
  alpha = 0 # 0 corresponds to no "protection"
)

# observed action at stage 1
a_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  n <- length(C_1)
  a_1 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_1))

  return(a_1)
}

# observed action at stage 2
a_20 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  A_1 <- data$A_1
  C_2 <- data$C_2
  n <- length(C_1)
  a_2 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_2)) * A_1


  return(a_2)
}

# optimal action at stage 2
d_alpha_opt_20 <- function(data, par){
  C_2 <- data$C_2
  A_1 <- data$A_1

  fit_2 <- lava::expit(par$gamma_A * C_2)
  ((fit_2 > 1 - par$alpha) + (fit_2 >= par$alpha & fit_2 <= 1 - par$alpha) * (C_2 + par$mu_L[3] > 0)) * A_1
}

# optimal action at stage 1
d_alpha_opt_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1

  fit_1 <- lava::expit(par$gamma_A * C_1)
  out <- ((fit_1 >= par$alpha & fit_1 <= 1 - par$alpha) * (C_1 + par$mu_L[2] + polle:::kappa_1(l_1 = L_1, par = par) + polle:::kappa_2(l_1 = L_1, par = par)) + (fit_1 > 1 - par$alpha)) > 0

  out <- as.numeric(out)
  return(out)
}

# approximations ----------------------------------------------------------

# # approximated mean utility
# n <- 2e6
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# observed_utility <- mean(utility(two_stage_policy_data)$U)
# rm(two_stage_policy_data)

# approximated mean utility under the optimal policy
# n <- 2e6
# set.seed(2)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = d_alpha_opt_10, a_2 = d_alpha_opt_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# optimal_utility <- mean(utility(two_stage_policy_data)$U)
# rm(two_stage_policy_data)


# g-models ----------------------------------------------------------------

g0 <- function(A, X){

  out <- list()
  class(out) <- "g0"

  return(out)
}

predict.g0 <- function(object, new_X){
  stopifnot(
    all(colnames(new_X) == c("L", "C"))
  )

  C <- new_X$C
  fit <- lava::expit(par0$gamma_A * C)

  preds <- matrix(
    c(
      1 - fit,
      fit
    ),
    ncol = 2
  )

  return(preds)
}

# n <- 2e3
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
#
# two_stage_policy_data <- new_policy_data(stage_data = d); rm(d)
#
# two_stage_state_history <- state_stage_history(two_stage_policy_data, stage = 2)
# two_stage_g_function <- fit_g_function(two_stage_state_history, new_g_glm())
# # two_stage_g_function <- fit_g_function(two_stage_state_history, g0)
# evaluate(two_stage_g_function, new_history = two_stage_state_history)
# rm( two_stage_g_function)
#
# # two_stage_g_functions <- fit_g_functions(two_stage_policy_data, g_models = new_g_glm(), full_history = FALSE)
# two_stage_g_functions <- fit_g_functions(two_stage_policy_data, g_models = g0, full_history = FALSE)
# # two_stage_g_functions <- fit_g_functions(two_stage_policy_data, g_models = list(new_g_glm(), new_g_glm()), full_history = FALSE)
# evaluate(two_stage_g_functions, two_stage_policy_data)
#
# two_stage_g_functions[[1]]$g_model$glm_model$coefficients
# par0$gamma_A # C parameter

# policies ----------------------------------------------------------------

always_treat_stage_policy <- function(history){
  pol <- history$H[, c("id", "stage")]
  pol[, d := "1"]
  return(pol)
}

always_treat_policy <- new_policy(
  stage_policies = always_treat_stage_policy,
  full_history = FALSE,
  replicate = TRUE
)

optimal_stage_policy_stage_1 <- function(history){
  stopifnot(
    all(c("C_1", "L_1") %in% colnames(history$H))
  )
  d <- d_alpha_opt_10(data = data.table(C_1 = history$H$C_1, L_1 = history$H$L_1), par = par0)
  pol <- history$H[, c("id", "stage")]
  pol[, d := as.character(d)]
  return(pol)
}

optimal_stage_policy_stage_2 <- function(history){
  stopifnot(
    all(c("C_2", "A_1") %in% colnames(history$H))
  )
  d <- d_alpha_opt_20(data = data.table(C_2 = history$H$C_2, A_1 = as.numeric(history$H$A_1)), par = par0)
  pol <- history$H[, c("id", "stage")]
  pol[, d := as.character(d)]
  return(pol)
}

optimal_policy <- new_policy(
  stage_policies = list(optimal_stage_policy_stage_1, optimal_stage_policy_stage_2),
  full_history = TRUE
)

# n <- 2e3
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# # always treat policy
# always_treat_policy(two_stage_policy_data)
#
# optimal_policy(two_stage_policy_data)
# rm(two_stage_policy_data)


# ipw ---------------------------------------------------------------------

# n <- 2e5
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d); rm(d)
#
# tmp <- ipw(
#   two_stage_policy_data,
#   policy = optimal_policy,
#   g_models = new_g_glm(),
#   g_full_history = FALSE
# )
# tmp$value_estimate
# # optimal_utility
# rm(tmp)
#
# tmp <- ipw(
#   two_stage_policy_data,
#   policy = optimal_policy,
#   g_models = g0
# )
# tmp$value_estimate
# # optimal_utility
# rm(tmp)
# rm(two_stage_policy_data)

# Q-models ----------------------------------------------------------------

q0_2 <- function(V_res, AX){
  out <- list()
  class(out) <- "q0_2"

  return(out)
}

predict.q0_2 <- function(q_model, new_AX){
  stopifnot(all(new_AX$A %in% c("0", "1")))

  n <- nrow(new_AX)

  pred <- (new_AX$A == "1") * rep(par0$mu_L[3], n)

  return(pred)
}

q0_1 <- function(V_res, AX){
  out <- list()
  class(out) <- "q0_1"

  return(out)
}
predict.q0_1 <- function(q_model, new_AX){
  stopifnot(all(colnames(new_AX) == c("A", "L", "C")))
  stopifnot(all(new_AX$A %in% c("0", "1")))

  n <- nrow(new_AX)

  l_1 <- new_AX$L
  a_1 <- new_AX$A
  pred <- (a_1 == "1") * (
    par0$mu_L[2] +
      kappa_1(l_1 = l_1, par = par0) +
      kappa_2(l_1 = l_1, par = par0)
  )

  return(pred)
}

qbias <- function(V_res, AX){
  out <- list()
  class(out) <- "qbias"

  return(out)
}
predict.qbias <- function(q_model, new_AX){
  n <- nrow(new_AX)

  pred <- rep(-2, times = n)

  return(pred)
}

# n <- 2e4
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
#
# two_stage_policy_data <- new_policy_data(stage_data = d); rm(d)
#
# tmp <- fit_Q_functions(
#   two_stage_policy_data,
#   # q_models = list(q0_1, q0_2),
#   q_models = new_q_glm(),
#   optimal_policy(two_stage_policy_data),
#   full_history = FALSE
#   )
# tmp[[1]]
# tmp[[2]]
# par0$mu_L[3] # A parameter
#
# attr(tmp, "full_history")
# evaluate(
#   tmp,
#   two_stage_policy_data
#   )

# OR ----------------------------------------------------------------------

# n <- 2e3
# set.seed(2)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d); rm(d)
#
# or0 <- or(
#   two_stage_policy_data,
#   policy = optimal_policy,
#   q_models = list(q0_1, q0_2),
#   q_full_history = TRUE
# )
# or0$value_estimate
# # optimal_utility
#
# tmp <- or(
#   two_stage_policy_data,
#   policy = optimal_policy,
#   q_functions = or0$q_functions,
#   q_full_history = TRUE
# )
# tmp$value_estimate
# # optimal_utility
# rm(tmp)
# rm(two_stage_policy_data)
#
# n <- 5e5
# set.seed(3)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d); rm(d)
#
# tmp <- or(
#   two_stage_policy_data,
#   policy = optimal_policy,
#   q_functions = or0$q_functions,
#   q_full_history = TRUE
# )
# tmp$value
# # optimal_utility
# rm(tmp)
# rm(two_stage_policy_data)


# DR ----------------------------------------------------------------------

n <- 2e3
set.seed(3)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

print(two_stage_policy_data)

two_stage_policy_data$stage_data[event == 0,][, .N, stage]

print.policy_data(two_stage_policy_data)

tmp <- policy_eval(
  type = "dr",
  two_stage_policy_data,
  # policy = optimal_policy,
  policy_learner = rqvl,
  qv_models = list(q_glm(formula = ~L+C), q_glm(formula = ~L+C)),
  alpha = 0,
  # policy = optimal_policy,
  g_models = g_glm(),
  # g_models = list(g_glm(~L_1), g_glm(~L_1)),
  # q_models = list(q0_1, q0_2),
  q_models = q_glm(),
  g_full_history = FALSE,
  q_full_history = FALSE,
  mc.cores = 3,
  M = 3,
)
tmp

tmp2 <- policy_eval(
  type = "ipw",
  two_stage_policy_data,
  policy = optimal_policy,
  # policy_learner = rqvl,
  qv_models = list(new_q_glm(formula = ~L+C), new_q_glm(formula = ~L+C)),
  alpha = 0,
  # policy = optimal_policy,
  g_models = g_glm(),
  # q_models = list(q0_1, q0_2),
  q_models = new_q_glm(),
  g_full_history = FALSE,
  q_full_history = FALSE,
  mc.cores = 3,
  M = 3,
)
tmp2

# all.equal(get_policy(tmp$policy_object)(two_stage_policy_data), optimal_policy(two_stage_policy_data))

tmp$g_functions
tmp$q_functions

tmp2 <- policy_eval(
  two_stage_policy_data,
  policy = optimal_policy,
  g_functions = tmp$g_functions,
  q_functions = tmp$q_functions
)
tmp2

# optimal_utility
rm(tmp, tmp2, two_stage_policy_data)

# RQL ----------------------------------------------------

# n <- 2e3
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# par0$alpha
# tmp_rql <- rql(
#   two_stage_policy_data,
#   alpha = par0$alpha,
#   # g_models = new_g_glm(),
#   # g_models = list(g0, g0),
#   # q_models = Q_linear,
#   q_models = list(q0_1, q0_2)
# )
# tmp_rql$value
# optimal_utility
#
# rql_policy <- get_policy(tmp_rql)
# rm(tmp_rql, two_stage_policy_data)
#
# n <- 2e3
# set.seed(2)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data_new <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# rql_policy_actions <- rql_policy(two_stage_policy_data_new)
# optimal_policy_actions_new <- optimal_policy(two_stage_policy_data_new)
# all.equal(rql_policy_actions, optimal_policy_actions_new)
# rm(optimal_policy_actions_new, rql_policy_actions, two_stage_policy_data_new)

# BOWL --------------------------------------------------------------------

# n <- 500
# set.seed(7)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# tmp_bowl <- bowl(
#   alpha = par0$alpha,
#   policy_data = two_stage_policy_data,
#   g_models = g0,
#   g_full_history = FALSE,
#   policy_full_history = TRUE,
#   # policy_vars = c("C", "L")
#   policy_vars = list(c("C_1", "L_1"), c("C_2", "L_2")) # note that DTRlearn2 has a bug, ncol(H)>1 must be TRUE.
# )
# tmp_bowl_policy <- get_policy(tmp_bowl)
#
# tmp_bowl_policy_actions <- tmp_bowl_policy(two_stage_policy_data)
# optimal_policy_actions <- optimal_policy(two_stage_policy_data)
# setnames(optimal_policy_actions, "d", "d_opt")
#
# plot_data <- state_history(two_stage_policy_data)$H
# plot_data <- merge(plot_data, tmp_bowl_policy_actions, all.x = TRUE)
# plot_data <- merge(plot_data, optimal_policy_actions, all.x = TRUE)
#
# plot_data[, g_1 := lava::expit(par0$gamma_A * C)]
# plot_data[, ap := (g_1 >= par0$alpha) & (g_1 <= (1-par0$alpha))]
#
# library(ggplot2)
# stage_ <- 1
# g_1 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d_opt, shape = ap)) +
#   theme_bw()
#
# g_2 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d, shape = ap)) +
#   theme_bw()
#
# gridExtra::grid.arrange(g_1, g_2)
#
# stage_ <- 2
# g_2 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d, shape = ap)) +
#   geom_hline(yintercept = -par0$mu_L[3]) +
#   theme_bw()
# g_2

# PTL ----------------------------------------------------------------------
# (doubly robust) policy tree learning

# n <- 2e3
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# get_history_names(two_stage_policy_data, stage = 1)
#
# tmp_ptl <- ptl(
#   policy_data = two_stage_policy_data,
#   g_models = new_g_glm(),
#   q_models = qbias,
#   g_full_history = FALSE,
#   q_full_history = FALSE,
#   policy_full_history = FALSE,
#   M = 2,
#   # policy_vars = list(c("C_1"), c("C_2", "L_2"))
#   policy_vars = c("L")
#   # policy_vars = NULL
# )
#
# tmp_ptl$ptl_objects[[1]]
# tmp_ptl$ptl_objects[[2]]
# # tmp_ptl$q_functions[[1]]$q_model
# # tmp_ptl$q_functions[[1]]$q_model$lm_model
# # par0$mu_L[[3]]
#
# tmp_policy <- get_policy(tmp_ptl)
# rm(two_stage_policy_data, tmp_ptl)
#
# n <- 1e3
# set.seed(2)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data_new <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
#
# tmp_policy_actions <- tmp_policy(two_stage_policy_data_new)
# optimal_policy_actions <- optimal_policy(two_stage_policy_data_new)
# setnames(optimal_policy_actions, "d", "d_opt")
#
# plot_data <- state_history(two_stage_policy_data_new)$H
# plot_data <- merge(plot_data, tmp_policy_actions, all.x = TRUE)
# plot_data <- merge(plot_data, optimal_policy_actions, all.x = TRUE)
#
# library(ggplot2)
# stage_ <- 1
# g_1 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d_opt)) +
#   theme_bw()
#
# g_2 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d)) +
#   theme_bw()
#
# gridExtra::grid.arrange(g_1, g_2)
#
# stage_ <- 2
# g_1 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d_opt)) +
#   geom_hline(yintercept = -par0$mu_L[3]) +
#   theme_bw()
#
# g_2 <- ggplot(plot_data[stage == stage_, ]) +
#   geom_point(aes(x = L, y = C, color = d)) +
#   geom_hline(yintercept = -par0$mu_L[3]) +
#   theme_bw()
#
# gridExtra::grid.arrange(g_1, g_2)


# RQVL --------------------------------------------------------------------

# realistic QV learning

n <- 2e3
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

get_X_names(two_stage_policy_data, stage = 2)

tmprqvl <- rqvl(
  policy_data = two_stage_policy_data,
  alpha = 0,
  g_models = new_g_glm(),
  # q_models = qbias,
  # q_models = new_q_glm(),
  q_models = list(q0_1, q0_2),
  g_full_history = FALSE,
  q_full_history = FALSE,
  qv_models = list(new_q_glm(formula = ~L+C), new_q_glm(formula = ~L+C)),
  qv_full_history = FALSE,
  M = 3
)
tmprqvl$value_estimate
tmprqvl$qv_functions[[1]]

tmp_policy <- get_policy(tmprqvl)
rm(two_stage_policy_data)
rm(tmprqvl)

n <- 1e3
set.seed(2)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data_new <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

policy_eval(
  two_stage_policy_data_new,
  type = "dr",
  g_models = new_g_glm(),
  q_models = list(q0_1, q0_2),
  policy = tmp_policy
)

tmp_policy_actions <- tmp_policy(two_stage_policy_data_new)
optimal_policy_actions <- optimal_policy(two_stage_policy_data_new)
setnames(optimal_policy_actions, "d", "d_opt")

plot_data <- state_history(two_stage_policy_data_new)$H
plot_data <- merge(plot_data, tmp_policy_actions, all.x = TRUE)
plot_data <- merge(plot_data, optimal_policy_actions, all.x = TRUE)

library(ggplot2)
stage_ <- 1
g_1 <- ggplot(plot_data[stage == stage_, ]) +
  geom_point(aes(x = L, y = C, color = d_opt)) +
  theme_bw()

g_2 <- ggplot(plot_data[stage == stage_, ]) +
  geom_point(aes(x = L, y = C, color = d)) +
  theme_bw()

gridExtra::grid.arrange(g_1, g_2)

stage_ <- 2
g_1 <- ggplot(plot_data[stage == stage_, ]) +
  geom_point(aes(x = L, y = C, color = d_opt)) +
  geom_hline(yintercept = -par0$mu_L[3]) +
  theme_bw()

g_2 <- ggplot(plot_data[stage == stage_, ]) +
  geom_point(aes(x = L, y = C, color = d)) +
  geom_hline(yintercept = -par0$mu_L[3]) +
  theme_bw()

gridExtra::grid.arrange(g_1, g_2)
