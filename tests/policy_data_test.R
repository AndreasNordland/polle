# library("polle")
#
# # Single stage ------------------------------------------------------------
#
# source(system.file("sim", "single_stage.R", package="polle"))
#
# par0 <- c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, s = 1)
# d <- sim_single_stage(5e2, seed=1, par=par0)
# head(d)
# names(d) <- c("Z", "L", "B", "treat", "reward")
#
#
# # d$ID <- paste("id", 1:nrow(d), sep = "_")
# # d$id <- 1:nrow(d)
# d$id <- paste("id", 1:nrow(d), sep = "_")
# # d$A <- rbinom(n = 5e2, size = 1, prob = 0.5)
# # d$event <- 1:nrow(d)
# # d$stage <- nrow(d):1
#
# pd <- policy_data(
#   d,
#   action = c("treat"),
#   # covariates = list(Z = c("Z"), L = c("L")),
#   covariates = c("Z", "L"),
#   baseline = list("B"),
#   # baseline_covariates = list("B", "treat"),
#   # baseline_covariates = 2,
#   # utility = "reward",
#   utility = "reward",
#   id = "id",
# )
#
# head(pd$stage_data, 10)
# head(d, 10)
#
# pd$baseline_data[id == "id_10"]
#
# # Two stage ---------------------------------------------------------------
#
# source(system.file("sim", "two_stage.R", package="polle"))
#
# par0 <- c(gamma = 0.5, beta = 1)
# d <- sim_two_stage(2e3, seed=1, par=par0, deterministic_rewards = FALSE)
#
# d$ID <- paste("id", 1:nrow(d), sep = "_")
# head(d, 3)
# d$A <- d$A_2
# d$event <- 1:nrow(d)
#
# pd <- policy_data(
#   d,
#   action = c("A_1", "A_2"),
#   covariates = list(L = c("L_1", "L_2"),
#                     C = c("C_1", "C_2")),
#   # utility = "U_k3")
#   utility = c("U_1", "U_2", "U_3"),
#   id = NULL
# )
# head(d, 10)
# head(pd$stage_data)
# rm(d)
#
# d <- sim_two_stage(2e3, seed=1, par=par0, deterministic_rewards = TRUE)
# head(d, 3)
#
# pd <- policy_data(d,
#                   action = c("A_1", "A_2"),
#                   covariates = list(L = c("L_1", "L_2"),
#                                     C = c("C_1", "C_2")),
#                   utility = c("U_1", "U_2", "U_3"),
#                   deterministic_rewards = list(
#                     U_A1 = c("U_1_A1", "U_2_A1")
#                   ), verbose = TRUE)
#
# pd
# head(pd$stage_data)
# head(d)
#
#
#
# # Two stage A/B ---------------------------------------------------------------
#
# d_ab <- sim_two_stage(2e3, seed=1, par=par0, deterministic_rewards = TRUE)
# head(d_ab, 3)
# d_ab[,A_1 := as.character(A_1)]
# d_ab[,A_2 := as.character(A_2)]
# d_ab[A_1 == "0", A_1 := "b"]
# d_ab[A_1 == "1", A_1 := "a"]
# d_ab[A_2 == "0", A_2 := "b"]
# d_ab[A_2 == "1", A_2 := "a"]
# setnames(d_ab, old = "U_1_A1", new = "U_1_Aa")
# setnames(d_ab, old = "U_2_A1", new = "U_2_Aa")
#
# pd2 <- policy_data(d_ab,
#                     action = c("A_1", "A_2"),
#                     covariates = list(L = c("L_1", "L_2"),
#                                       C = c("C_1", "C_2")),
#                     # utility = "U_3")
#                     utility = c("U_1", "U_2", "U_3"),
#                     deterministic_rewards = list(
#                       U_Aa = c("U_1_Aa", "U_2_Aa")
#                     ), verbose = TRUE)
#
# head(pd2$stage_data == pd$stage_data)
# pd2$stage_data
# head(d)
#
# # Multiple stages ---------------------------------------------------------
# source(system.file("sim", "multi_stage.R", package="polle"))
#
# a_obs <- function(t, x, beta, ...){
#   prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x))
#   rbinom(n = 1, size = 1, prob = prob)
# }
#
# par0 <- list(
#   tau = 10,
#   lambda = c( # # exp(-lambda) = mean, rate = 1 / mean
#     0, # intercept
#     -0.4, # x
#     0.3 # w
#   ),
#   alpha =  c( # dist2devtribution of x
#     0, # intercept
#     0.5, # t,
#     0.1, # t^2, x (the cost) will increase with t
#     -0.5, # x_lead
#     0.4 # b
#   ),
#   beta = c( # distribution of a
#     0.3, # intercept
#     0, # t
#     -0.5 # x
#   ),
#   sigma = 1,
#   xi = 0.3, # binary baseline probability
#   psi = 1 # minimum increment
# )
#
# n <- 5e3
# d <- sim_multi_stage(n, par0, a = a_obs, seed = 1)
# stage_data <- d$stage_data
# bd <- data.table(ID = 1:5000, B = 5000:1)
#
# # stage_data$ID <- stage_data$id
#
# stage_data$U_A0 <- NULL
# # stage_data$U_A1 <- NULL
#
# setnames(stage_data, old = c("id", "A", "stage", "event", "U"), new = c("ID", "action", "k", "status", "reward"))
#
# # stage_data$A <- stage_data$action
#
# # names(stage_data) <- c("ID", "k", "ev", "treat", "X")
#
# (pd <- policy_data(data = stage_data, type = "long", id = "ID", action = "action", stage = "k", event = "status", utility = "reward", verbose = TRUE))

