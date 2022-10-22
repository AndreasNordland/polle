# Single stage ------------------------------------------------------------

library("polle")
source(system.file("sim", "single_stage.R", package="polle"))
d1 <- sim_single_stage(1e3, seed=1)
pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")

# V-restricted Policy Tree Learning
pl1 <- policy_learn(type = "ptl",
                    policy_vars = c("Z", "L"),
                    L = 2,
                    alpha = 0.05)

# V-restricted (Doubly Robust) Q-learning
qv1 <- policy_learn(type = "rqvl",
                    qv_models = q_glm(formula = ~ Z + L),
                    L = 2,
                    alpha = 0.05)

# Q-learning
ql <- policy_learn(type = "rql",
                   L = 2,
                   alpha = 0.05)


set.seed(1)
pe1 <- list(
  pl = policy_eval(policy_data = pd1,
                   policy_learn = pl1,
                   q_models = q_glm(),
                   g_models = g_glm()),
  qv = policy_eval(policy_data = pd1,
                   policy_learn = qv1,
                   q_models = q_glm(),
                   g_models = g_glm()),
  ql = policy_eval(policy_data = pd1,
                   policy_learn = ql,
                   q_models = q_glm(),
                   g_models = g_glm())
)

# saveRDS(pe1, file = "inst/examples/policy_learn_test_pe1.Rds")
# pe1_master <- readRDS(file = "inst/examples/policy_learn_test_pe1.Rds")

# all(unlist(lapply(tmp1, coef)) == unlist(lapply(pe1, coef)))
# all(unlist(lapply(tmp1, IC)) == unlist(lapply(pe1, IC)))


# Two stage ---------------------------------------------------------------

rm(list = ls())
source(system.file("sim", "two_stage.R", package="polle"))
d2 <- sim_two_stage(2e3, seed=1)
pd2 <- policy_data(d2,
                  action = c("A_1", "A_2"),
                  baseline = c("BB"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))

# V-restricted Policy Tree Learning
pl2 <- policy_learn(type = "ptl",
                    policy_vars = c("L"),
                    L = 2,
                    alpha = 0.05)

# V-restricted (Doubly Robust) Q-learning
qv2 <- policy_learn(type = "rqvl",
                    qv_models = q_glm(formula = ~ L + BB),
                    L = 2,
                    alpha = 0.05)

# Q-learning
ql <- policy_learn(type = "rql",
                   L = 2,
                   alpha = 0.05)

set.seed(1)
pe2 <- list(
  pl = policy_eval(policy_data = pd2,
                   policy_learn = pl2,
                   q_models = q_glm(),
                   g_models = g_glm()),
  qv = policy_eval(policy_data = pd2,
                   policy_learn = qv2,
                   q_models = q_glm(),
                   g_models = g_glm()),
  ql = policy_eval(policy_data = pd2,
                   policy_learn = ql,
                   q_models = q_glm(),
                   g_models = g_glm())
)

# pe2_master <- readRDS(file = "inst/examples/policy_learn_test_pe2.Rds")

all(unlist(lapply(pe2_master, coef)) == unlist(lapply(pe2, coef)))
all(unlist(lapply(pe2_master, IC)) == unlist(lapply(pe2, IC)))
