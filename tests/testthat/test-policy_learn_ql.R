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

test_that("get_policy_functions.ql agrees with get_policy.ql", {
  d <- sim_two_stage(500, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  # not full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm())

  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = FALSE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = FALSE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )

  # full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm(), q_full_history = TRUE)
  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = TRUE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = TRUE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )
})

test_that("get_policy_functions.ql handles deterministic rewards", {
  d <- sim_two_stage(500, seed=1, deterministic_rewards = TRUE)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"),
                    deterministic_rewards = list(
                      U_A0 = c("U_1_A0", "U_2_A0"),
                      U_A1 = c("U_1_A1", "U_2_A1")
                    ))

  # not full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm())

  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = FALSE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = FALSE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )

  # full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm(), q_full_history = TRUE)
  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = TRUE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = TRUE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )
})

test_that("get_policy_functions.ql handles partially missing deterministic rewards", {
  d <- sim_two_stage(500, seed=1, deterministic_rewards = TRUE)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"),
                    deterministic_rewards = list(
                      U_A0 = c("U_1_A0", "U_2_A0"),
                      U_A1 = c("U_1_A1", "U_2_A1")
                    ))

  # not full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm())

  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = FALSE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = FALSE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )

  # full history:
  pl <- policy_learn()
  po <- pl(pd, q_models = q_glm(), q_full_history = TRUE)
  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = TRUE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = TRUE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )
})

test_that("get_policy_functions.ql handles realistic actions", {
  d <- sim_two_stage(500, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  # not full history:
  pl <- policy_learn(alpha = 0.1)
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = FALSE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = FALSE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )

  # full history:
  pl <- policy_learn(alpha = 0.1)
  po <- pl(pd, q_models = q_glm(), q_full_history = TRUE, g_models = g_glm(), g_full_history = FALSE)
  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = TRUE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  H1 <- cbind(H1, his_1$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  H1$L <- H1$L_1
  H1$C <- H1$C_1
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = TRUE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  H2 <- cbind(H2, his_2$U[, -c("id", "stage", "U_bar", "U_A0"), with = FALSE])
  H2$L <- H2$L_2
  H2$C <- H2$C_2
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
  )
})

test_that("get_policy_functions.ql handles varying stage action set", {
  d <- sim_two_stage_multi_actions(n=500, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  # not full history:
  pl <- policy_learn(alpha = 0.05)
  po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_empir()))

  pf_1 <- get_policy_functions(po, stage = 1)
  his_1 <- get_history(pd, stage = 1,
                       full_history = FALSE)
  H1 <- his_1$H[, -c("id", "stage"), with = FALSE]
  pa_1 <- pf_1(H1)

  pf_2 <- get_policy_functions(po, stage = 2)
  his_2 <- get_history(pd, stage = 2,
                       full_history = FALSE)
  H2 <- his_2$H[, -c("id", "stage"), with = FALSE]
  pa_2 <- pf_2(H2)

  ## benchmark
  pa <- get_policy(po)(pd)

  expect_equal(
    pa[stage ==1]$d,
    pa_1
  )
  expect_equal(
    pa[stage ==2]$d,
    pa_2
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
