
# Single stage ------------------------------------------------------------


## RQL ---------------------------------------------------------------------

test_that("get_policy.RQL returns a policy", {
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

## RQVL --------------------------------------------------------------------

test_that("get_policy.RQVL returns a policy", {
  d <- sim_single_stage(200, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  pl <- policy_learn(type = "rqvl", control = control_rqvl())
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

## PTL ---------------------------------------------------------------------

test_that("get_policy.PTL returns a policy", {
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


## EARL --------------------------------------------------------------------

test_that("get_policy.EARL returns a policy", {
  library("DynTxRegime")

  d <- sim_single_stage(200, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  moPropen1 <- buildModelObj(model = ~B+Z+L,
                             solver.method = 'glm',
                             solver.args = list('family'='binomial'),
                             predict.method = 'predict.glm',
                             predict.args = list(type='response'))

  pl <- policy_learn(type = "earl",
                     control = control_earl(moPropen = moPropen1,
                                            regime = ~B+Z+L,))
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

test_that("the polle implementation of earl agrees with direct application of DynTxRegime::earl in the single stage case.",{
  library("DynTxRegime")

  d1 <- sim_single_stage(200, seed=1)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  # direct application:
  moPropen1 <- buildModelObj(model = ~B+Z+L,
                             solver.method = 'glm',
                             solver.args = list('family'='binomial'),
                             predict.method = 'predict.glm',
                             predict.args = list(type='response'))

  moMain1 <- buildModelObj(model = ~B+Z+L,
                           solver.method = 'lm')

  moCont1 <- buildModelObj(model = ~B+Z+L,
                           solver.method = 'lm')

  set.seed(1)
  dir <- DynTxRegime::earl(moPropen = moPropen1,
                           moMain = moMain1,
                           moCont = moCont1,
                           response = d1$U,
                           data = d1,
                           txName = "A",
                           lambdas = c(0.5, 1, 2),
                           regime = ~B+Z+L,
                           cvFolds = 3,
                           verbose = 0)

  # polle application:
  pl <- policy_learn(type = "earl",
                     control = control_earl(moPropen = moPropen1,
                                            moMain = moMain1,
                                            moCont = moCont1,
                                            regime = ~B+Z+L,
                                            verbose = 0,
                                            lambdas = c(0.5, 1, 2),
                                            cvFolds = 3))
  set.seed(1)
  po <- pl(policy_data = pd1)

  # comparison
  expect_equal(
    dir@analysis@optimal@estimatedValue,
    po$earl_object@analysis@optimal@estimatedValue
  )
})

test_that("the polle implementation is robust in respect to the action set.",{
  d1 <- sim_single_stage(200, seed=1)
  d2 <- d1

  d2$A[d1$A == 0] <- "B"
  d2$A[d1$A == 1] <- "A"

  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")
  pd2 <- policy_data(d2,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  # earl specification
  moPropen1 <- buildModelObj(model = ~B+Z+L,
                             solver.method = 'glm',
                             solver.args = list('family'='binomial'),
                             predict.method = 'predict.glm',
                             predict.args = list(type='response'))

  moMain1 <- buildModelObj(model = ~B+Z+L,
                           solver.method = 'lm')

  moCont1 <- buildModelObj(model = ~B+Z+L,
                           solver.method = 'lm')

  pl <- policy_learn(type = "earl",
                     control = control_earl(moPropen = moPropen1,
                                             moMain = moMain1,
                                             moCont = moCont1,
                                             regime = ~B+Z+L,
                                             verbose = 0,
                                             lambdas = c(0.5, 1, 2),
                                             cvFolds = 3))
  set.seed(1)
  po1 <- pl(policy_data = pd1)
  set.seed(1)
  po2 <- pl(policy_data = pd1)

  expect_equal(
    po1$earl_object@analysis@optimal@estimatedValue,
    po2$earl_object@analysis@optimal@estimatedValue
  )

  expect_equal(
    po1$earl_object@analysis@optimal@optimalTx,
    po2$earl_object@analysis@optimal@optimalTx
  )

})

test_that("earl handles missing arguments", {
  library("DynTxRegime")

  d1 <- sim_single_stage(200, seed=1)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  pl <- policy_learn(type = "earl",
                     control = control_earl(regime = ~B+Z+L,
                                            verbose = 0,
                                            lambdas = c(0.5, 1, 2),
                                            cvFolds = 3))
  set.seed(1)
  # moPropen is required:
  expect_error(pl(policy_data = pd1))
})


## RWL ---------------------------------------------------------------------

test_that("the polle implementation of rwl agrees with direct application of DynTxRegime::rwl in the single stage case.",{
  library("DynTxRegime")

  d1 <- sim_single_stage(200, seed=1)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  # direct application:
  moPropen1 <- buildModelObj(model = ~B+Z+L,
                             solver.method = 'glm',
                             solver.args = list('family'='binomial'),
                             predict.method = 'predict.glm',
                             predict.args = list(type='response'))

  moMain1 <- buildModelObj(model = ~B+Z+L,
                           solver.method = 'lm')

  set.seed(1)
  dir <- DynTxRegime::rwl(moPropen = moPropen1,
                           moMain = moMain1,
                           response = d1$U,
                           data = d1,
                           txName = "A",
                           lambdas = c(0.5, 1, 2),
                           regime = ~B+Z+L,
                           cvFolds = 3,
                           verbose = 0)

  # polle application:
  pl <- policy_learn(type = "rwl",
                     control = control_rwl(moPropen = moPropen1,
                                           moMain = moMain1,
                                           regime = ~B+Z+L,
                                           verbose = 0,
                                           lambdas = c(0.5, 1, 2),
                                           cvFolds = 3))
  set.seed(1)
  po <- pl(policy_data = pd1)

  # comparison
  expect_equal(
    dir@analysis@optimal@estimatedValue,
    po$rwl_object@analysis@optimal@estimatedValue
  )
})


## OWL --------------------------------------------------------------------

test_that("the implementation of owl agrees with direct application of DTRlearn2::owl in the single stage case.",{
  library("DTRlearn2")

  d1 <- sim_single_stage(200, seed=1)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  H <- scale(d1[, c("B", "Z", "L")])
  AA <- 2*d1[, "A"]-1
  RR <- d1[, "U"]

  pi_model <- glm(A~B+Z+L, data = d1, family = binomial())
  pi <- predict(pi_model, type = "response")
  pi <- pi * d1$A + (1-pi) * (1-d1$A)

  set.seed(1)
  owl1 <- owl(H = H, AA = AA, RR = RR, n = nrow(d1), K = 1, pi = pi)
  owl1_dd <- predict(owl1, H = H, K = 1)$treatment[[1]]
  owl1_d <- unname(as.character(unlist((owl1_dd + 1)/2)))

  pl <- policy_learn(type = "owl", control = control_owl())

  set.seed(1)
  owl2 <- pl(policy_data = pd1, g_models = g_glm())
  owl2_d <- unname(unlist(get_policy(owl2)(pd1)[, "d"]))
  expect_equal(owl1_d,owl2_d)
})

# Two stages --------------------------------------------------------------


## PTL ---------------------------------------------------------------------

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

## RQL --------------------------------------------------------------

test_that("policy_learn with type rql works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  ql <- policy_learn(type = "rql",
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

  ql <- policy_learn(type = "rql",
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

test_that("policy_learn with type rql handles varying action sets",{
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

## RQVL -------------------------------------------------------------

test_that("policy_learn with type = 'rqvl' checks input",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))
  qv <- policy_learn(type = "rqvl", control = control_rqvl())

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

test_that("policy_learn with type rqvl works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  qv <- policy_learn(type = "rqvl", control = control_rqvl())
  gfun <- fit_g_functions(pd, g_models = g_glm(), full_history = FALSE)
  gfun2 <- fit_g_functions(pd, g_models = list(g_glm(), g_glm()), full_history = FALSE)

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
    policy_eval(policy_data = pd,policy_learn = qv),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,policy_learn = qv, g_functions = gfun),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,policy_learn = qv,g_functions = gfun2),
    NA
  )

  qv <- policy_learn(type = "rqvl",
                     control = control_rqvl(qv_models = q_glm(formula = Y ~ .)))
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    NA
  )

  qv <- policy_learn(type = "rqvl",
                     control = control_rqvl(qv_models = q_glm(formula = Y ~ BB)))
  expect_error(
    policy_eval(policy_data = pd, policy_learn = qv),
    NA
  )

  qv <- policy_learn(type = "rqvl",
                     control = control_rqvl(qv_models = q_glm(formula = ~ X)))
  expect_error(
    policy_eval(policy_data = pd,policy_learn = qv),
    "The QV-model formula ~X is invalid."
  )

  qv <- policy_learn(type = "rqvl",
                     control = control_rqvl(qv_models = q_glm(formula = Y ~ X)))
  expect_error(
    policy_eval(policy_data = pd,policy_learn = qv),
    "The QV-model formula ~X is invalid."
  )

  # q_glm formula default is A * (.), and A is not used when fitting the
  # QV-model.
  qv <- policy_learn(type = "rqvl",
                     control = control_rqvl(qv_models = q_glm()))
  expect_error(policy_eval(policy_data = pd, policy_learn = qv))
})

test_that("policy_learn with type rqvl handles varying action sets",{
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "rqvl",
                     control = control_rqvl())
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
  pl <- policy_learn(type = "rqvl",
                     control = control_rqvl(),
                     alpha = 0.4)
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = list(g_glm(), g_rf())),
    "Cases with no realistic actions occur. Consider resetting the alpha level."
  )

  # realistic policy:
  pl <- policy_learn(type = "rqvl",
                     control = control_rqvl(),
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
  pl <- policy_learn(type = "rqvl",
                     control = control_rqvl(),
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

test_that("policy_learn with type = 'rqvl' works with cross_fit_g_models.",{
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "rqvl",
                     cross_fit_g_models = FALSE,
                     L = 2,
                     alpha = 0.1,
                     control = control_rqvl())

  gfun <- fit_g_functions(pd, list(g_empir(), g_empir()), full_history = FALSE)

  po <- pl(pd,
           g_models = list(g_empir(), g_empir()),
           q_models = q_glm())

  expect_equal(
    po$g_functions,
    gfun
  )

})


## OWL --------------------------------------------------------------------

test_that("the implementation of owl agrees with direct application of DTRlearn2::owl in the two stage case.",{
  d <- sim_two_stage(200, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  H1 <- d[, c("B", "L_1", "C_1")]
  H2 <- d[, c("B", "L_2", "C_2")]
  H <- list(H1, H2)
  H <- lapply(H, scale)
  AA <- list(2*d$A_1-1, 2*d$A_2-1)

  pi1_model <- glm(A_1~B+C_1+L_1, data = d, family = binomial())
  pi1 <- predict(pi1_model, type = "response")
  pi1 <- pi1 * d$A_1 + (1-pi1) * (1-d$A_1)
  pi2_model <- glm(A_2~B+C_2+L_2, data = d, family = binomial())
  pi2 <- predict(pi2_model, type = "response")
  pi2 <- pi2 * d$A_2 + (1-pi2) * (1-d$A_2)
  pi <- list(pi1, pi2)

  RR <- list(d$U_2, d$U_3)

  library("DTRlearn2")
  set.seed(1)
  owl1 <- owl(H = H, AA = AA, RR = RR, n = nrow(d), K = 2, pi = pi)
  owl_pred <- predict(owl1, H = H, K = 2)
  owl1_dd1 <- owl_pred$treatment[[1]]
  owl1_dd2 <- owl_pred$treatment[[2]]
  owl1_d1 <- unname(as.character(unlist((owl1_dd1 + 1)/2)))
  owl1_d2 <- unname(as.character(unlist((owl1_dd2 + 1)/2)))

  pl <- policy_learn(type = "owl",
                     control = control_owl(policy_vars = c("B", "L", "C"),
                                            reuse_scales = TRUE))
  set.seed(1)
  owl2 <- pl(policy_data = pd,
             g_models = list(g_glm(~B + L + C), g_glm(~B + L + C)))

  tmp <- get_policy(owl2)(pd)
  owl2_d1 <- unname(unlist(tmp[stage == 1, "d", with = FALSE]))
  owl2_d2 <- unname(unlist(tmp[stage == 2, "d", with = FALSE]))

  expect_equal(
    unname(owl2$owl_object$pi[[1]]),
    unname(pi[[1]]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(owl2$owl_object$pi[[2]]),
    unname(pi[[2]]),
    tolerance = 1e-10
  )
  expect_equal(owl1_d1,owl2_d1)
  expect_equal(owl1_d2,owl2_d2)

  # varying stage action sets:
  d2 <- copy(d)
  d2$A_1[d2$A_1 == 1] <- "t1"
  d2$A_1[d2$A_1 == 0] <- "t2"
  d2$A_2[d2$A_2 == 1] <- "t3"
  d2$A_2[d2$A_2 == 0] <- "t4"
  pd2 <- policy_data(d2,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  set.seed(1)
  owl3 <- pl(policy_data = pd2,
             g_models = list(g_glm(~B + L + C), g_glm(~B + L + C)))
  tmp3 <- get_policy(owl3)(pd2)
  owl3_d1 <- unname(unlist(tmp3[stage == 1, "d", with = FALSE]))
  owl3_d2 <- unname(unlist(tmp3[stage == 2, "d", with = FALSE]))

  owl1_d1 <- unname(owl1_dd1[,1])
  owl1_d1[owl1_d1 == -1] <- "t2"
  owl1_d1[owl1_d1 == 1] <- "t1"
  owl1_d2 <- unname(owl1_dd2[,1])
  owl1_d2[owl1_d2 == -1] <- "t4"
  owl1_d2[owl1_d2 == 1] <- "t3"

  expect_equal(owl1_d1,owl3_d1)
  expect_equal(owl1_d2,owl3_d2)
})

test_that("policy_learn with type owl runs as intended", {
  d <- sim_two_stage(200, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "ptl",
                     control = control_ptl(),
                     L = 2,
                     alpha = 0.3,
                     save_cross_fit_models = TRUE)

  po <- pl(pd,
           g_models = g_glm(),
           q_models = q_glm())

  expect_true(
    is.null(po$g_function)
  )
  expect_true(
    !is.null(po$g_functions_cf)
  )

  expect_true(
    is.data.table(get_policy(po)(pd))
  )
  expect_true(
    all(complete.cases(get_policy(po)(pd)))
  )


})

# Multiple stages ---------------------------------------------------------


## RQVL --------------------------------------------------------------------

test_that("policy_learn with type rqvl handles multiple stages with varying stage action sets",{
  d <- sim_multi_stage(200, seed = 1)
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

  pl <- policy_learn(type = "rqvl",
                     control = control_rqvl(),
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

## OWL ---------------------------------------------------------------------

test_that("input to policy_learn with type owl handles incorrect input.",{
  d <- sim_multi_stage(200, seed = 1)
  # constructing policy_data object:
  pd <- policy_data(data = d$stage_data,
                    baseline_data = d$baseline_data,
                    type = "long",
                    id = "id",
                    stage = "stage",
                    event = "event",
                    action = "A",
                    utility = "U")
  pd <- partial(pd, 3)

  pl <- policy_learn(type = "owl")

  expect_error(policy_eval(policy_data = pd,
                           policy_learn = pl), "owl is only implemented for a fixed number of stages.")
})
