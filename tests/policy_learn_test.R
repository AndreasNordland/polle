# Single stage ------------------------------------------------------------

library("polle")
source(system.file("sim", "single_stage.R", package="polle"))
d1 <- sim_single_stage(1e3, seed=1)
pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")

# V-restricted Policy Tree Learning
pl1 <- policy_learn(type = "ptl",
                    policy_vars = c("Z", "L"), L = 2)

# V-restricted (Doubly Robust) Q-learning
qv1 <- policy_learn(type = "rqvl",
                   qv_models = q_glm(formula = ~ Z + L), L = 2)


set.seed(1)
pe1 <- list(
  pl = policy_eval(policy_data = pd1,
                   policy_learn = pl1,
                   q_models = q_glm(),
                   g_models = g_glm()),
  qv = policy_eval(policy_data = pd1,
                   policy_learn = qv1,
                   q_models = q_glm(),
                   g_models = g_glm())
)

# tmp1 <- pe1

# all(unlist(lapply(tmp1, coef)) == unlist(lapply(pe1, coef)))
# all(unlist(lapply(tmp1, IC)) == unlist(lapply(pe1, IC)))
