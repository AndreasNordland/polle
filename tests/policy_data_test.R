
# single stage

source(system.file("sim", "single_stage.R", package="polle"))

par0 <- c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, s = 1)
d <- sim_single_stage(5e2, seed=1, par=par0)
head(d)
names(d) <- c("Z", "L", "B", "treat", "reward")

d$ID <- paste("id", 1:nrow(d), sep = "_")

tmp <- policy_data(d, action = c("treat"), covariates = list(Z = c("Z"), L = c("L")), utility = "reward", id = "ID")

tmp

head(tmp$stage_data)
head(d)

# two stage

source(system.file("sim", "two_stage.R", package="polle"))

par0 <- c(gamma = 0.5, beta = 1)
d <- sim_two_stage(2e3, seed=1, par=par0, deterministic_utilities = FALSE)
head(d, 3)

tmp1 <- policy_data(d,
                   action = c("A_1", "A_2"),
                   covariates = list(L = c("L_1", "L_2"),
                                     C = c("C_1", "C_2")),
                   # utility = "U_k3")
                   utility = c("U_1", "U_2", "U_3"))
head(d)
head(tmp1$stage_data)
rm(d)


par0 <- c(gamma = 0.5, beta = 1)
d <- sim_two_stage(2e3, seed=1, par=par0, deterministic_utilities = TRUE)
head(d, 3)

tmp <- policy_data(d,
                  action = c("A_1", "A_2"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"),
                  deterministic_utility = list(
                    U_1 = c("U_1_1", "U_2_1")
                  ), verbose = TRUE)

tmp
head(tmp$stage_data)
head(d)

# two stage A/B

d_ab <- sim_two_stage(2e3, seed=1, par=par0, deterministic_utilities = TRUE)
head(d_ab, 3)
d_ab[,A_1 := as.character(A_1)]
d_ab[,A_2 := as.character(A_2)]
d_ab[A_1 == "0", A_1 := "b"]
d_ab[A_1 == "1", A_1 := "a"]
d_ab[A_2 == "0", A_2 := "b"]
d_ab[A_2 == "1", A_2 := "a"]

tmp2 <- policy_data(d,
                    action = c("A_1", "A_2"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    # utility = "U_3")
                    utility = c("U_1", "U_2", "U_3"),
                    deterministic_utility = list(
                      U_b = c("U_1_0", "U_1_1"),
                      U_a = c("U_2_0", "U_2_1")
                    ))
tmp2$stage_data
head(d)
