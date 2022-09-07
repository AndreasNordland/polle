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


tmp <- pe1



# Two stage ---------------------------------------------------------------




source(system.file("sim", "two_stage.R", package="polle"))
par0 <- c(gamma = 0.5, beta = 1)
d <- sim_two_stage(2e3, seed=1, par=par0)
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))


tmp <- polle:::fit_g_functions(
  policy_data = pd,
  g_models = g_rf(num.trees = c(500, 750)),
  full_history = FALSE
)

tmp$all_stages$g_model

evaluate(tmp, pd)



pl <- policy_learn(type = "rqvl",
                   qv_models = list(q_glm(~C_1), q_glm(~C_1+C_2)),
                   qv_full_history = TRUE,
                   L = 3,
                   save_cross_fit_models = TRUE)



# pl <- policy_learn(type = "ptl",
#                    policy_vars = list(c("C_1"), c("C_2", "C_2")),
#                    policy_full_history = TRUE,
#                    L = 3,
#                    save_cross_fit_models = TRUE)

q_functions <- polle:::fit_Q_functions(
  policy_data = pd,
  policy_actions = pl_static(pd),
  q_models = list(q_glm(), q_glm()),
  full_history = TRUE
)


set.seed(1)
pe_ipw <- policy_eval(type = "ipw",
                  policy_data = pd,
                  policy_learn = pl,
                  q_models = q_glm(),
                  g_models = g_glm(),
                  M = 2)

pe_or <- policy_eval(type = "or",
                      policy_data = pd,
                      policy_learn = pl,
                      q_models = q_glm(),
                      g_models = g_glm(),
                      M = 2)

pe_dr <- policy_eval(type = "dr",
                     policy_data = pd,
                     policy_learn = pl,
                     q_models = q_glm(),
                     g_models = list(g_glm(), g_glm()),
                     g_full_history = TRUE,
                     M = 2)

# set.seed(1)
# policy_eval(type = "dr",
#             policy_data = pd,
#             policy = pl_static,
#             q_functions = q_functions,
#             q_models = q_glm(),
#             g_models = g_glm(),
#             M = 2)


# set.seed(1)
# pe_ipw_new <- policy_eval(type = "ipw",
#                       policy_data = pd,
#                       policy_learn = pl,
#                       q_models = q_glm(),
#                       g_models = g_glm(),
#                       M = 2)
#
#
# pe_or_new <- policy_eval(type = "or",
#                      policy_data = pd,
#                      policy_learn = pl,
#                      q_models = q_glm(),
#                      g_models = g_glm(),
#                      M = 2)
#
# pe_dr_new <- policy_eval(type = "dr",
#                      policy_data = pd,
#                      policy_learn = pl,
#                      q_models = q_glm(),
#                      g_models = g_glm(),
#                      M = 2)
#
#
# pe_ipw_new$type <- NULL
# all.equal(pe_ipw, pe_ipw_new) # remove type in output of policy_eval_type
#
# pe_or_new$type <- NULL
# all.equal(pe_or, pe_or_new) # remove type in output of policy_eval_type
#
# pe_dr_new$type <- NULL
# all.equal(pe_dr, pe_dr_new) # remove type in output of policy_eval_type

