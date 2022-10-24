library("polle")

# Single stage ------------------------------------------------------------
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
one_run <- function(fun, policy_data){
  set.seed(1)
  pe <- policy_eval(policy_data = policy_data,
                    policy_learn = fun,
                    q_models = q_glm(),
                    g_models = g_glm())
  return(pe)
}

source(system.file("sim", "two_stage.R", package="polle"))
d <- sim_two_stage(2e3, seed=1)
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  baseline = c("BB"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))

### V-restricted (Doubly Robust) Q-learning
qv1 <- policy_learn(type = "rqvl",
                    qv_models = q_glm(formula = ~ L + BB),
                    L = 2,
                    alpha = 0.05)
qv2 <- policy_learn(type = "rqvl",
                    qv_models = q_glm(formula = ~ L + BB),
                    L = 1,
                    alpha = 0.05)
qv3 <- policy_learn(type = "rqvl",
                    qv_models = q_glm(formula = ~ L + BB),
                    L = 2,
                    alpha = 0)

pe2_qv <- lapply(
  list(qv1 = qv1, qv2 = qv2, qv3 = qv3),
  one_run,
  policy_data = pd
)

# saveRDS(pe2_qv, file = "inst/examples/policy_learn_test_pe2_qv.Rds")
pe2_qv_master <- readRDS(file = "inst/examples/policy_learn_test_pe2_qv.Rds")

stopifnot(
  all(unlist(lapply(pe2_qv_master, coef)) == unlist(lapply(pe2_qv, coef))),
  all(unlist(lapply(pe2_qv_master, IC)) == unlist(lapply(pe2_qv, IC)))
)

### V-restricted Policy Tree Learning
ptl1 <- policy_learn(type = "ptl",
                     policy_vars = c("L"),
                     L = 2,
                     alpha = 0.05)

ptl2 <- policy_learn(type = "ptl",
                     policy_vars = c("L", "C"),
                     L = 2,
                     alpha = 0.05)

ptl3 <- policy_learn(type = "ptl",
                     policy_vars = c("L", "C"),
                     L = 2,
                     alpha = 0,
                     depth = 2)

ptl4 <- policy_learn(type = "ptl",
                     policy_vars = c("L", "C"),
                     L = 2,
                     alpha = 0,
                     hybrid = TRUE,
                     depth = 3)

pe2_ptl <- lapply(
  list(ptl1 = ptl1, ptl2 = ptl2, ptl3 = ptl3, ptl4 = ptl4),
  one_run,
  policy_data = pd
)


# saveRDS(pe2_ptl, file = "inst/examples/policy_learn_test_pe2_ptl.Rds")
pe2_ptl_master <- readRDS(file = "inst/examples/policy_learn_test_pe2_ptl.Rds")

stopifnot(
  all(unlist(lapply(pe2_ptl_master, coef)) == unlist(lapply(pe2_ptl, coef))),
  all(unlist(lapply(pe2_ptl_master, IC)) == unlist(lapply(pe2_ptl, IC)))
)

# Q-learning
ql1 <- policy_learn(type = "rql",
                   L = 2,
                   alpha = 0.05)

pe2_ql <- lapply(
  list(ql1 = ql1),
  one_run,
  policy_data = pd
)

# saveRDS(pe2_ql, file = "inst/examples/policy_learn_test_pe2_ql.Rds")
pe2_ql_master <- readRDS(file = "inst/examples/policy_learn_test_pe2_ql.Rds")

stopifnot(
  all(unlist(lapply(pe2_ql_master, coef)) == unlist(lapply(pe2_ql, coef))),
  all(unlist(lapply(pe2_ql_master, IC)) == unlist(lapply(pe2_ql, IC)))
)

