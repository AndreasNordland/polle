test_that("get_policy.ql returns a policy", {
  d <- sim_single_stage(200, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  pl <- policy_learn()
  expect_error({
    p <- get_policy(pl(pd, q_models = q_glm()))
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

test_that("policy_learn with type = 'ql' works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  ql <- policy_learn(type = "ql",
                     alpha = 0,
                     L = 1)

  expect_error(
    ql(policy_data = pd, q_models = q_glm()),
    NA
  )
  expect_error(
    ql(policy_data = pd, q_models = list(q_glm(), q_glm())),
    NA
  )

  ql <- policy_learn(type = "ql",
                     alpha = 0.1,
                     L = 1)

  expect_error(
    ql(policy_data = pd, q_models = q_glm()),
    "Please provide g_models."
  )

  expect_error({
    po <- ql(policy_data = pd, q_models = q_glm(), g_glm())
  }, NA)

  expect_error({
    p <- get_policy(po)
  }, NA)

  expect_s3_class(p(pd), class = "data.table")
})

test_that("policy_learn with type ql handles varying action sets",{
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))
  pl <- policy_learn()

  expect_error(
    po <- pl(pd, q_models = q_glm()),
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

})
