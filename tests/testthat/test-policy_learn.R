
# Two stages --------------------------------------------------------------



## QV-learning -------------------------------------------------------------

test_that("policy_learn works as intended",{
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  qv <- policy_learn(type = "rqvl",
                     qv_models = q_glm(formula = ~ .))

  expect_error(
    qv(policy_data = pd, g_models = g_glm(), q_models = q_glm()),
    NA
  )

  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)
})

test_that("input to policy_learn with type rqvl handles incorrect arguments",{

  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  qv <- policy_learn(type = "rqvl",
                     qv_models = q_glm(formula = Y ~ .))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)

  qv <- policy_learn(type = "rqvl",
                     qv_models = q_glm(formula = Y ~ BB))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)

  qv <- policy_learn(type = "rqvl",
                      qv_models = q_glm(formula = ~ X))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), "The QV-model formula ~X is invalid.")

  qv <- policy_learn(type = "rqvl",
                     qv_models = q_glm(formula = Y ~ X))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), "The QV-model formula ~X is invalid.")

  # q_glm formula default is A * (.), and A is not used when fitting the
  # QV-model.
  qv <- policy_learn(type = "rqvl",
                     qv_models = q_glm())
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv))
})
