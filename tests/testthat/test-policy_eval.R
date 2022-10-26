# Two stages --------------------------------------------------------------

source(system.file("sim", "two_stage.R", package="polle"))
d <- sim_two_stage(2e3, seed=1)
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  baseline = c("BB", "B"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))

p_dynamic <- policy_def(
  policy_functions = list(dynamic_policy(function(L_1) (L_1>0)),
                          dynamic_policy(function(C_2) (C_2>0))),
  reuse = FALSE
)

## DR -------------------------------------------------------------

test_that("input to policy_eval with type dr handles incorrect arguments",{
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           g_models = g_glm(formula = A~X)), "The g-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           g_models = g_sl(formula = a~X)), "The g-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           g_models = g_rf(formula = ~X)), "The g-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           g_models = g_glmnet(formula = ~X)), "The g-model formula ~X is invalid.")

  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glm(formula = Y~X)), "The Q-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_sl(formula = res~X)), "The Q-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_rf(formula = V_res~X * (.))))
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glmnet(formula = Y~X)), "The Q-model formula ~X is invalid.")
})
