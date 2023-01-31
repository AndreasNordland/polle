test_that("get_policy.ptl returns a policy", {
  d <- sim_single_stage(200, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  pl <- policy_learn(type = "ptl", control = control_ptl())
  expect_error({
    p <- get_policy(pl(pd, q_models = q_glm(), g_models = g_glm()))
  },
  NA
  )

  expect_true(
    inherits(p, what = "policy")
  )

  expect_error(
    p(pd),
    NA
  )
})

test_that("policy_learn with type ptl works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "ptl",
                     control = control_ptl(),
                     alpha = 0,
                     L = 1)

  expect_error({
    po <- pl(policy_data = pd,
             g_models = g_glm(),
             q_models = q_glm())
  }, NA)

  expect_s3_class(get_policy(po)(pd),
                  "data.table")


  expect_error({
    pl <- policy_learn(type = "ptl",
                       control = control_ptl(policy_vars = c("L", "BB"),
                                             depth = 3,
                                             hybrid = TRUE),
                       alpha = 0,
                       L = 1)
  }, NA)

  expect_error({
    po <- pl(policy_data = pd,
             g_models = g_glm(),
             q_models = q_glm())
  }, NA)

  expect_s3_class(get_policy(po)(pd), "data.table")


})

test_that("policy_learn with type ptl handles varying action sets",{
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "ptl",
                     control = control_ptl())
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    NA
  )
  expect_error(
    pa <- get_policy(po)(pd),
    NA
  )

  expect_error(
    pe <- policy_eval(pd, policy = get_policy(po),
                      g_models = list(g_glm(), g_rf())),
    NA
  )

  # realistic actions
  d2 <- copy(d)
  d2$A_2[d2$A_2 == "no"] <- "yes"
  pd2 <- policy_data(data = d2,
                     action = c("A_1", "A_2"),
                     baseline = c("B", "BB"),
                     covariates = list(L = c("L_1", "L_2"),
                                       C = c("C_1", "C_2")),
                     utility = c("U_1", "U_2", "U_3"))

  pl2 <- policy_learn(type = "ptl",
                      control = control_ptl(),
                      alpha = 0.45,
                      L = 2)
  expect_error(
    po2 <- pl2(pd2, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    NA
  )
  expect_error(
    pa2 <- get_policy(po2)(pd2),
    NA
  )
  expect_error(
    pe2 <- policy_eval(pd2,
                       policy_learn = pl2,
                       g_models = list(g_glm(), g_rf())),
    NA
  )

  expect_error(
    pf2_1 <- get_policy_functions(po2, stage = 1),
    NA
  )
  H1 <- get_history(pd2, stage = 1)$H
  expect_error(
    pfa2_1 <- pf2_1(H1),
    NA
  )
  expect_equal(
    pa2[stage == 1, ]$d,
    pfa2_1
  )

  expect_error(
    pf2_2 <- get_policy_functions(po2, stage = 2),
    NA
  )
  H1 <- get_history(pd2, stage = 2)$H
  expect_error(
    pfa2_2 <- pf2_2(H1),
    NA
  )
  expect_equal(
    pa2[stage == 2, ]$d,
    pfa2_2
  )

})
