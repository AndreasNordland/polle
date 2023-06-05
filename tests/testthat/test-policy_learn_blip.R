test_that("policy_learn with type blip works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))


  # default settings:
  pl <- policy_learn(type = "blip",
                     control = control_blip())
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )

    inherits(po, "policy_object")
  })

  # realistic policy:
  pl <- policy_learn(type = "blip",
                     control = control_blip(),
                     alpha = 0.2)
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )

    inherits(po, "policy_object")
  })

  # predictions from the blip model:
  expect_true({
    blip <-predict(po$blip_functions, pd)
    !any(is.na(blip$blip))
  })
  expect_true({
    all(is.numeric(blip$blip))
  })

  # folds passed to the blip model:
  pl <- policy_learn(type = "blip",
                     L = 2,
                     control = control_blip(
                       blip_models = q_sl()
                     ))
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )
    t1 <- inherits(po, "policy_object")
    t2 <- po$blip_functions$stage_1$blip_model$model$cvControl$V == 2
    t3 <- all.equal(po$folds, po$blip_functions$stage_1$blip_model$model$cvControl$validRows)

    all(c(t1, t2, t3))
  })
})

test_that("get_policy_functions.blip returns a list of policy functions", {
  d <- sim_two_stage(200, seed=1)
  d$A_2 <- paste(d$A_2, "test", sep = "")
  d$A_1 <- as.character(d$A_1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "blip",
                     control = control_blip())
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  his <- get_history(pd, stage = 1)
  H <- get_H(his)

  expect_true(
    all(pf1(H) %in% get_stage_action_sets(pd)[[1]])
  )

  # realistic action set at level alpha:
  pl <- policy_learn(type = "blip",
                     control = control_blip(),
                     alpha = 0.2)
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  his <- get_history(pd, stage = 1)
  H <- get_H(his)

  expect_true(
    all(pf1(H) %in% get_stage_action_sets(pd)[[1]])
  )

  expect_error(
    pf2 <- get_policy_functions(po, stage = 2),
    NA
  )
  his <- get_history(pd, stage = 2)
  H <- get_H(his)

  expect_true(
    all(pf2(H) %in% get_stage_action_sets(pd)[[2]])
  )

})

test_that("get_policy.blip returns a policy", {
  d <- sim_two_stage(200, seed=1)
  d$A_2 <- paste(d$A_2, "test", sep = "")
  d$A_1 <- as.character(d$A_1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "blip",
                     control = control_blip())
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_true({
    p <- get_policy(po)
    inherits(p, "policy")
  })

  expect_true(
    is.data.table(p(pd))
  )

  # realistic action set at level alpha:
  pl <- policy_learn(type = "blip",
                     control = control_blip(),
                     alpha = 0.2)
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_true({
    p <- get_policy(po)
    inherits(p, "policy")
  })

  pa <- p(pd)
  expect_true(is.data.table(pa))
  expect_true(all(pa$d %in% get_action_set(pd)))
  expect_true(all(key(pa) == c("id", "stage")))
  expect_true(inherits(pa$d, "character"))

  # comparing get_policy_function and get_policy:
  pf1 <- get_policy_functions(po, stage = 1)
  his1 <- get_history(pd, stage = 1)
  H1 <- get_H(his1)

  expect_equal(
    pf1(H1),
    pa[stage == 1,]$d
  )

  pf2 <- get_policy_functions(po, stage = 2)
  his2 <- get_history(pd, stage = 2)
  H2 <- get_H(his2)

  expect_equal(
    pf2(H2),
    pa[stage == 2,]$d
  )

})
