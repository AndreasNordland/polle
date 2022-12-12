
# Single-stage ------------------------------------------------------------

test_that("policy_eval evaluates on a subset of the data (with missing actions)",{

  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(1e2, seed=1)
  pd1 <- policy_data(d1, action = "A", covariates = c("Z"), utility = "U")

  pd2 <- subset(pd1, id = get_id(pd1)[d1$A == "0"])

  ### ipw
  expect_error(
    pe1_ipw <- policy_eval(pd1, policy = policy_def(1), type = "ipw"),
    NA
  )

  expect_error(
    pe2_ipw <- policy_eval(policy_data = pd2,
                           g_functions = get_g_functions(pe1_ipw),
                           policy = policy_def(1),
                           type = "ipw"),
    NA
  )
  expect_equal(
    pe2_ipw$value_estimate,
    0
  )

  expect_equal(
    pe2_ipw$IC,
    (pe1_ipw$IC[d1$A == "0"] + pe1_ipw$value_estimate)
  )

  ### or

  expect_error(
    pe1_or <- policy_eval(pd1, policy = policy_def(1), type = "or"),
    NA
  )

  expect_error(
    pe2_or <- policy_eval(policy_data = pd2,
                          q_functions = get_q_functions(pe1_or),
                          policy = policy_def(1),
                          type = "or"),
    NA
  )
  expect_equal(
    pe2_or$IC + pe2_or$value_estimate,
    (pe1_or$IC[d1$A == "0"] + pe1_or$value_estimate)
  )

})

# Two-stage --------------------------------------------------------------

## DR -------------------------------------------------------------

test_that("policy_eval handles missing covariates", {
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(2e3, seed=1)
  d$C_1 <- NULL
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c(NA, "C_2")), # C_1 is missing
                    utility = c("U_1", "U_2", "U_3"))
  p <- policy_def(1, reuse = TRUE)

  expect_error(
    policy_eval(policy_data = pd,
                policy = p)
  )

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = g_glm(~L),
                q_models = q_glm(~L)),
    NA
  )

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = list(g_glm(~L), g_glm()),
                q_models = list(q_glm(~L), q_glm())),
    NA
  )
})

test_that("input to policy_eval with type dr handles incorrect arguments",{
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  p_dynamic <- policy_def(
    policy_functions = list(function(L_1) (L_1>0)*1,
                            function(C_2) (C_2>0)*1),
    reuse = FALSE
  )

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
