pl_static <- policy_def(
  policy_functions = static_policy(1),
  reuse = TRUE
)

# Single stage ------------------------------------------------------------

library("polle")
source(system.file("sim", "single_stage.R", package="polle"))
d1 <- sim_single_stage(5e2, seed=1)
pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")

# policies
pl1_dynamic <- policy_def(
  policy_functions = dynamic_policy(function(L) (L>0) ),
  reuse = TRUE
)

set.seed(1)
pe1 <- list(
  ipw_static =  policy_eval(type = "ipw",
                     policy_data = pd1,
                     policy = pl_static,
                     g_models = g_glm(),
                     M = 1),
  or_static = policy_eval(type = "or",
                          policy_data = pd1,
                          policy = pl_static,
                          q_models = q_glm(),
                          M = 1),
  dr_static = policy_eval(type = "dr",
                          policy_data = pd1,
                          policy = pl_static,
                          g_models = g_glm(),
                          q_models = q_glm(),
                          M = 1),
  ipw_dyn =  policy_eval(type = "ipw",
                            policy_data = pd1,
                            policy = pl1_dynamic,
                            g_models = g_glm(),
                            M = 1),
  or_dyn = policy_eval(type = "or",
                          policy_data = pd1,
                          policy = pl1_dynamic,
                          q_models = q_glm(),
                          M = 1),
  dr_dyn = policy_eval(type = "dr",
                          policy_data = pd1,
                          policy = pl1_dynamic,
                          g_models = g_glm(),
                          q_models = q_glm(),
                          M = 1),
  ipw_cross =  policy_eval(type = "ipw",
                         policy_data = pd1,
                         policy = pl1_dynamic,
                         g_models = g_glm(),
                         M = 2),
  or_cross = policy_eval(type = "or",
                       policy_data = pd1,
                       policy = pl1_dynamic,
                       q_models = q_glm(),
                       M = 2),
  dr_cross = policy_eval(type = "dr",
                       policy_data = pd1,
                       policy = pl1_dynamic,
                       g_models = g_glm(),
                       q_models = q_glm(),
                       M = 2)
)

# tmp <- pe1

# all(unlist(lapply(tmp, coef)) == unlist(lapply(pe1, coef)))
# all(unlist(lapply(tmp, IC)) == unlist(lapply(pe1, IC)))

# Two stages ---------------------------------------------------------------

source(system.file("sim", "two_stage.R", package="polle"))
d2 <- sim_two_stage(1e3, seed=1)
pd2 <- policy_data(d2,
                   action = c("A_1", "A_2"),
                   baseline = c("B"),
                   covariates = list(L = c("L_1", "L_2"),
                                     C = c("C_1", "C_2")),
                   utility = c("U_1", "U_2", "U_3"))

# policies
pl2_dynamic <- policy_def(
  policy_functions = list(dynamic_policy(function(L_1) (L_1>0)),
                          dynamic_policy(function(C_2) (C_2>0))),
  reuse = FALSE
)

set.seed(1)
pe2 <- list(
  ipw =  policy_eval(type = "ipw",
                     policy_data = pd2,
                     policy = pl2_dynamic,
                     g_models = g_glm(),
                     M = 1),
  or = policy_eval(type = "or",
                   policy_data = pd2,
                   policy = pl2_dynamic,
                   q_models = q_glm(),
                   M = 1),
  dr = policy_eval(type = "dr",
                   policy_data = pd2,
                   policy = pl2_dynamic,
                   g_models = g_glm(),
                   q_models = q_glm(),
                   M = 1),
  ipw_full =  policy_eval(type = "ipw",
                          policy_data = pd2,
                          policy = pl2_dynamic,
                          g_models = list(g_glm(), g_glm()),
                          g_full_history = TRUE,
                          M = 1),
  or_full = policy_eval(type = "or",
                        policy_data = pd2,
                        policy = pl2_dynamic,
                        q_models = list(q_glm(), q_glm()),
                        q_full_history = TRUE,
                        M = 1),
  dr_full = policy_eval(type = "dr",
                        policy_data = pd2,
                        policy = pl2_dynamic,
                        g_models = list(g_glm(), g_glm()),
                        g_full_history = TRUE,
                        q_models = list(q_glm(), q_glm()),
                        q_full_history = TRUE,
                        M = 1),
  ipw_cross =  policy_eval(type = "ipw",
                           policy_data = pd2,
                           policy = pl2_dynamic,
                           g_models = list(g_glm(), g_glm()),
                           g_full_history = TRUE,
                           q_models = list(q_glm(), q_glm()),
                           q_full_history = TRUE,
                           M = 2),
  or_cross = policy_eval(type = "or",
                         policy_data = pd2,
                         policy = pl2_dynamic,
                         g_models = list(g_glm(), g_glm()),
                         g_full_history = TRUE,
                         q_models = list(q_glm(), q_glm()),
                         q_full_history = TRUE,
                         M = 2),
  dr_cross = policy_eval(type = "dr",
                         policy_data = pd2,
                         policy = pl2_dynamic,
                         g_models = list(g_glm(), g_glm()),
                         g_full_history = TRUE,
                         q_models = list(q_glm(), q_glm()),
                         q_full_history = TRUE,
                         M = 2)
)

# tmp2 <- pe2

# all(unlist(lapply(tmp2, coef)) == unlist(lapply(pe2, coef)))
# all(unlist(lapply(tmp2, IC)) == unlist(lapply(pe2, IC)))

# Multiple stages ---------------------------------------------------------

source(system.file("sim", "multi_stage.R", package="polle"))
d3 <- sim_multi_stage(2e3, seed = 1)
# constructing policy_data object:
pd3 <- policy_data(data = d3$stage_data,
                   baseline_data = d3$baseline_data,
                   type = "long")
pd3 <- partial(pd3, K = 2)

# policies
pl3_dynamic <- policy_def(
  policy_functions = dynamic_policy(function(X) (X>0)),
  reuse = TRUE
)

get_history_names(pd3, stage = 2)

pe3 <- list(
  ipw =  policy_eval(type = "ipw",
                     policy_data = pd3,
                     policy = pl3_dynamic,
                     g_models = g_glm(),
                     M = 1),
  or = policy_eval(type = "or",
                   policy_data = pd3,
                   policy = pl3_dynamic,
                   q_models = q_glm(),
                   M = 1),
  dr = policy_eval(type = "dr",
                   policy_data = pd3,
                   policy = pl3_dynamic,
                   g_models = g_glm(),
                   q_models = q_glm(),
                   M = 1),
  ipw_full =  policy_eval(type = "ipw",
                     policy_data = pd3,
                     policy = pl3_dynamic,
                     g_models = list(g_glm(~ t_1 + X_1), g_glm(~ t_2 + X_2)),
                     g_full_history = TRUE,
                     M = 2),
  or_full = policy_eval(type = "or",
                   policy_data = pd3,
                   policy = pl3_dynamic,
                   q_models = list(q_glm(~ t_1 + X_1 + B), q_glm(~ t_2 + X_2 + B)),
                   q_full_history = TRUE,
                   M = 2),
  dr_full = policy_eval(type = "dr",
                   policy_data = pd3,
                   policy = pl3_dynamic,
                   g_models = list(g_glm(~ t_1 + X_1), g_glm(~ t_2 + X_2)),
                   g_full_history = TRUE,
                   q_models = list(q_glm(~ t_1 + X_1 + B), q_glm(~ t_2 + X_2 + B)),
                   q_full_history = TRUE,
                   M = 2)
)

# tmp3 <- pe3

# all(unlist(lapply(tmp3, coef)) == unlist(lapply(pe3, coef)))
# all(unlist(lapply(tmp3, IC)) == unlist(lapply(pe3, IC)))
