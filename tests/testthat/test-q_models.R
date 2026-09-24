test_that("fit_q_functions handles varying stage-action sets", {

  d <- sim_two_stage_multi_actions(n = 1e2)
  expect_error(
    pd <- policy_data(data = d,
                      action = c("A_1", "A_2"),
                      baseline = c("B", "BB"),
                      covariates = list(L = c("L_1", "L_2"),
                                        C = c("C_1", "C_2")),
                      utility = c("U_1", "U_2", "U_3")),
    NA
  )

  p <- policy_def(
    c("yes", "no")
  )

  expect_error(
    qfit <- fit_Q_functions(pd, policy_actions = p(pd), q_models = list(q_glm(), q_glm()), full_history = FALSE, m_function = NULL),
    NA
  )
  expect_error(
    tmp <- predict(qfit, pd),
    NA
  )

  tmp2 <- predict(qfit$stage_1$q_model$model,
                  newdata = cbind(A = "yes", get_history(pd, stage = 1)$H[1,]))
  tmp2 <- tmp2 + get_history(pd, stage = 1)$U$U_bar[1]
  expect_equal(unname(tmp2), unname(unlist(tmp[1, "Q_yes"])))

  tmp2 <- predict(qfit$stage_1$q_model$model,
                  newdata = cbind(A = "no", get_history(pd, stage = 1)$H[1,]))
  tmp2 <- tmp2 + get_history(pd, stage = 1)$U$U_bar[1]
  expect_equal(unname(tmp2), unname(unlist(tmp[1, "Q_no"])))

})

test_that("q_models checks formula input", {
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

  ## glm
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glm(formula = Y~X)),
               "variable 'X' is not found in data when calling 'q_glm' with formula: ~X")
  ## ns() lives in the base 'splines' package. Previously attached
  ## transitively via SuperLearner -> gam; load it explicitly now that
  ## SuperLearner is no longer a hard dependency of polle.
  library("splines")
  expect_no_error(policy_eval(policy_data = pd,
                              policy = p_dynamic,
                              q_models = q_glm(formula = ~ns(C))))

  ## xgboost
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_xgboost(formula = Y~X, nrounds = 2)),
               "variable 'X' is not found in data when calling 'q_xgboost' with formula: ~X")

})

test_that("q_models checks formula input 2", {

  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
                    action = "A",
                    covariates = list("Z", "B", "L"),
                    utility = "U"
                    )

  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(formula = ~ A*.))
  )
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = g_glm()),
    "Error in blip_model: variable 'A' is not found in data when calling 'q_glm' with formula: ~A \\+ Z \\+ B \\+ L \\+ A:Z \\+ A:B \\+ A:L"
  )

  ## A in global environment (formula defined in the global environment):
  A <- d$A

  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(formula = ~ A*.))
  )
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = g_glm()),
    "Error in blip_model: variable 'A' is not found in data when calling 'q_glm' with formula: ~A \\+ Z \\+ B \\+ L \\+ A:Z \\+ A:B \\+ A:L"
  )

})

test_that("q_rf formats data correctly via the formula",{
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    pe <- policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "ql", alpha = 0.05),
      g_models = g_glm(),
      g_full_history = FALSE,
      q_models = q_rf()
    ),
    NA
  )
})

test_that("q_sl() errors informatively while the SuperLearner interface is unavailable", {
  expect_error(
    q_sl(),
    "SuperLearner"
  )
})


# missing data ------------------------------------------------------------


test_that("q_glm handles missing covariates", {
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
                policy = p),
    "NA/NaN/Inf in 'x'"
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glm(~L)),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = list(q_glm(~L), q_glm())),
    NA
  )
})
