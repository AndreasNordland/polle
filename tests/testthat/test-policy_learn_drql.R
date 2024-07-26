
test_that("get_policy.drql returns a policy", {
  d <- sim_single_stage(200, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  pl <- policy_learn(type = "drql",
                     control = control_drql())
  expect_error({
    p <- get_policy(pl(pd, q_models = q_glm(), g_models = g_glm()))
  },
  NA
  )
  expect_true(
    inherits(p, what = "policy")
  )
  expect_true(
    is.data.table(p(pd))
  )

  pl <- policy_learn(type = "drql",
                     control = control_drql(
                       qv_models = q_sl()
                     ))
  expect_error({
    p <- get_policy(pl(pd, q_models = q_glm(), g_models = g_glm()))
  },
  NA
  )
  expect_true(
    inherits(p, what = "policy")
  )
  expect_true(
    is.data.table(p(pd))
  )

})

test_that("policy_learn with type = 'drql' checks input", {
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))
  qv <- policy_learn(type = "drql",
                     control = control_drql())

  gfun <- fit_g_functions(pd, g_models = g_glm(), full_history = FALSE)
  gfun2 <- fit_g_functions(pd, g_models = list(g_glm(), g_glm()), full_history = FALSE)

  expect_error(
    qv(policy_data = pd, q_models = q_glm()),
    "Please provide g_models."
  )
  expect_error(
    qv(policy_data = pd, q_models = q_glm(), g_functions = g_glm()),
    "g-functions must be of class 'g_functions'."
  )
  expect_error(
    qv(policy_data = pd, q_models = list(), g_models = g_glm()),
    "q_models must either be a list of length K or a single Q-model."
  )
  expect_error(
    qv(policy_data = pd,
       q_models = list(q_glm(), q_glm(),q_glm()),
       g_models = g_glm()),
    "q_models must either be a list of length K or a single Q-model."
  )

})

test_that("policy_learn with type drql works as intended", {
  d <- sim_two_stage(200, seed = 1)

  pd <- policy_data(d,
    action = c("A_1", "A_2"),
    baseline = c("BB", "B"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  qv <- policy_learn(
    type = "drql",
    control = control_drql()
  )
  gfun <- fit_g_functions(pd, g_models = g_glm(), full_history = FALSE)
  gfun2 <- fit_g_functions(
    pd,
    g_models = list(g_glm(), g_glm()),
    full_history = FALSE
  )

  expect_error(
    qv(policy_data = pd, g_models = g_glm(), q_models = q_glm()),
    NA
  )
  expect_error(
    qv(policy_data = pd, g_functions = gfun, q_models = q_glm()),
    NA
  )
  expect_error(
    qv(policy_data = pd, g_functions = gfun2, q_models = q_glm()),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv, g_functions = gfun),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv, g_functions = gfun2),
    NA
  )

  qv <- policy_learn(
    type = "drql",
    control = control_drql(qv_models = q_glm(formula = Y ~ .))
  )
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    NA
  )

  qv <- policy_learn(
    type = "drql",
    control = control_drql(qv_models = q_glm(formula = Y ~ BB))
  )

  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    NA
  )

  qv <- policy_learn(
    type = "drql",
    control = control_drql(qv_models = q_glm(formula = ~X))
  )

  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    "object 'X' not found when calling 'q_glm' with formula:
V_res ~ X"
  )

  qv <- policy_learn(
    type = "drql",
    control = control_drql(qv_models = q_glm(formula = Y ~ X))
  )
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    "object 'X' not found when calling 'q_glm' with formula:
V_res ~ X"
  )

  # q_glm formula default is A * (.), and A is not used when fitting the
  # QV-model.
  qv <- policy_learn(
    type = "drql",
    control = control_drql(qv_models = q_glm())
  )

  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    "object 'A' not found when calling 'q_glm' with formula:
V_res ~ A \\+ L \\+ C \\+ BB \\+ B \\+ A:L \\+ A:C \\+ A:BB \\+ A:B"
  )
})

test_that("policy_learn with type drql handles varying action sets", {
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "drql",
                     control = control_drql())
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    NA
  )
  expect_error(
    pa <- get_policy(po)(pd),
    NA
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  H1 <- get_history(pd, stage = 1, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa1 <- pf1(H1),
    NA
  )
  expect_equal(
    pa[stage == 1,]$d,
    pa1
  )

  expect_error(
    pf2 <- get_policy_functions(po, stage = 2),
    NA
  )
  H2 <- get_history(pd, stage = 2, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa2 <- pf2(H2),
    NA
  )
  expect_equal(
    pa[stage == 2,]$d,
    pa2
  )

  expect_error(
    pe <- policy_eval(pd, policy = get_policy(po),
                      g_models = list(g_glm(), g_rf())),
    NA
  )

  # realistic policy:
  pl <- policy_learn(type = "drql",
                     control = control_drql(),
                     alpha = 0.4)
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    "Cases with no realistic actions occur. Consider resetting the alpha level."
  )

  # realistic policy:
  pl <- policy_learn(type = "drql",
                     control = control_drql(),
                     alpha = 0.2)
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    NA
  )
  expect_error(
    pa <- get_policy(po)(pd),
    NA
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  H1 <- get_history(pd, stage = 1, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa1 <- pf1(H1),
    NA
  )
  expect_equal(
    pa[stage == 1,]$d,
    pa1
  )

  expect_error(
    pf2 <- get_policy_functions(po, stage = 2),
    NA
  )
  H2 <- get_history(pd, stage = 2, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa2 <- pf2(H2),
    NA
  )
  expect_equal(
    pa[stage == 2,]$d,
    pa2
  )

  expect_error(
    pe <- policy_eval(pd, policy = get_policy(po),
                      g_models = list(g_glm(), g_rf())),
    NA
  )

  # cross-fitting
  # realistic policy:
  pl <- policy_learn(type = "drql",
                     control = control_drql(),
                     alpha = 0.2,
                     L = 2)
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    NA
  )
  expect_error(
    pa <- get_policy(po)(pd),
    NA
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  H1 <- get_history(pd, stage = 1, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa1 <- pf1(H1),
    NA
  )
  expect_equal(
    pa[stage == 1,]$d,
    pa1
  )

  expect_error(
    pf2 <- get_policy_functions(po, stage = 2),
    NA
  )
  H2 <- get_history(pd, stage = 2, full_history = FALSE)$H[, -c("id", "stage"), with = FALSE]
  expect_error(
    pa2 <- pf2(H2),
    NA
  )
  expect_equal(
    pa[stage == 2,]$d,
    pa2
  )

  expect_error(
    pe <- policy_eval(pd, policy = get_policy(po),
                      g_models = list(g_glm(), g_rf()),
                      M = 2),
    NA
  )

})

test_that("policy_learn with type = 'drql' works with cross_fit_g_models.", {
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(
    data = d,
    action = c("A_1", "A_2"),
    baseline = c("B", "BB"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  pl <- policy_learn(
    type = "drql",
    cross_fit_g_models = FALSE,
    L = 2,
    alpha = 0.1,
    control = control_drql()
  )

  gfun <- fit_g_functions(pd,
    list(g_empir(), g_empir()),
    full_history = FALSE
  )

  po <- pl(pd,
    g_models = list(g_empir(), g_empir()),
    q_models = q_glm()
  )

  expect_equal(
    po$g_functions,
    gfun
  )
})



test_that("policy_learn with type = 'drql' saves cross-fitted models.", {
  d <- sim_two_stage(n = 1e2)
  pd <- policy_data(
    data = d,
    action = c("A_1", "A_2"),
    baseline = c("B", "BB"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  pl <- policy_learn(
    type = "drql",
    cross_fit_g_models = FALSE,
    L = 2,
    alpha = 0.1,
    save_cross_fit_models = FALSE,
    control = control_drql()
  )

  po <- pl(pd,
    g_models = list(g_empir(), g_empir()),
    q_models = q_glm()
  )

  expect_true(
    is.null(unlist(po$q_functions_cf))
  )

  pl <- policy_learn(
    type = "drql",
    cross_fit_g_models = FALSE,
    L = 2,
    alpha = 0.1,
    save_cross_fit_models = TRUE,
    control = control_drql()
  )

  po <- pl(pd,
    g_models = list(g_empir(), g_empir()),
    q_models = q_glm()
  )

  expect_true(
    !is.null(unlist(po$q_functions_cf))
  )
})


test_that("policy_learn with type drql handles multiple stages with varying stage action sets",{
  d <- sim_multi_stage(300, seed = 1)
  # constructing policy_data object:
  pd <- policy_data(data = d$stage_data,
                    baseline_data = d$baseline_data,
                    type = "long",
                    id = "id",
                    stage = "stage",
                    event = "event",
                    action = "A",
                    utility = "U")

  pd3 <- partial(pd, 3)

  pl <- policy_learn(type = "drql",
                     control = control_drql(
                       qv_models = q_sl(cvControl = SuperLearner.CV.control(V = 10L))
                     ),
                     alpha = 0.1,
                     L = 2)

  set.seed(1)
  suppressWarnings(
    pe <- policy_eval(policy_data = pd3,
                      policy_learn = pl,
                      g_models = list(g_glm(),g_glm(), g_glm()))
  )

  # renaming actions at each stage:
  d2_stage_data <- copy(d$stage_data)
  d2_stage_data[stage == 1, A := ifelse(A == 1, "t2", "t1")]
  d2_stage_data[stage == 2, A := ifelse(A == 1, "t4", "t3")]
  d2_stage_data[stage == 3, A := ifelse(A == 1, "t6", "t5")]
  d2_stage_data[, U_At1 := 0]
  d2_stage_data[stage == 1, U_At1 := U_A0]
  d2_stage_data[, U_At2 := 0]
  d2_stage_data[stage == 1, U_At2 := U_A1]
  d2_stage_data[, U_At3 := 0]
  d2_stage_data[stage == 2, U_At3 := U_A0]
  d2_stage_data[, U_At4 := 0]
  d2_stage_data[stage == 2, U_At4 := U_A1]
  d2_stage_data[, U_At5 := 0]
  d2_stage_data[stage == 3, U_At5 := U_A0]
  d2_stage_data[, U_At6 := 0]
  d2_stage_data[stage == 3, U_At6 := U_A1]
  d2_stage_data[, U_A0 := NULL]
  d2_stage_data[, U_A1 := NULL]

  pd3_2 <- policy_data(data = d2_stage_data,
                       baseline_data = d$baseline_data,
                       type = "long",
                       id = "id",
                       stage = "stage",
                       event = "event",
                       action = "A",
                       utility = "U")
  pd3_2 <- partial(pd3_2, K = 3)

  set.seed(1)
  suppressWarnings(
    pe2 <- policy_eval(policy_data = pd3_2,
                       policy_learn = pl,
                       g_models = list(g_glm(),g_glm(), g_glm()))
  )
  expect_equal(
    pe$value_estimate,
    pe2$value_estimate
  )
  expect_equal(
    pe$IC,
    pe2$IC
  )
})
