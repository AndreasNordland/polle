
# Single stage ------------------------------------------------------------


## EARL --------------------------------------------------------------------

test_that("the polle implementation of earl agrees with direct application of DynTxRegime::earl in the single stage case.",{
  library("DynTxRegime")

  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(2e3, seed=1)
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
                     earl_args = list(moPropen = moPropen1,
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
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(1e3, seed=1)
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
                     earl_args = list(moPropen = moPropen1,
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


## RWL ---------------------------------------------------------------------

test_that("the polle implementation of rwl agrees with direct application of DynTxRegime::rwl in the single stage case.",{
  library("DynTxRegime")

  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(2e3, seed=1)
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
                     rwl_args = list(moPropen = moPropen1,
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


## BOWL --------------------------------------------------------------------

test_that("the implementation of bowl agrees with direct application of DTRlearn2::owl in the single stage case.",{
  library("DTRlearn2")

  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(5e2, seed=1)
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

  pl <- policy_learn(type = "bowl")

  set.seed(1)
  owl2 <- pl(policy_data = pd1, g_models = g_glm())
  owl2_d <- unname(unlist(get_policy(owl2)(pd1)[, "d"]))
  expect_equal(owl1_d,owl2_d)
})

# Two stages --------------------------------------------------------------


## PTL ---------------------------------------------------------------------

test_that("policy_learn with type ptl works as intended",{

  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(5e2, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "ptl",
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
                       ptl_args = list(policy_vars = c("L", "BB"),
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


## Q-learning --------------------------------------------------------------

test_that("policy_learn with type rql works as intended",{

  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(5e2, seed=1)

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

## QV-learning -------------------------------------------------------------

test_that("policy_learn with type rqvl works as intended",{
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(5e2, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm(formula = ~ .)))

  expect_error(
    qv(policy_data = pd, g_models = g_glm(), q_models = q_glm()),
    NA
  )

  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)

  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm(formula = Y ~ .)))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)

  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm(formula = Y ~ BB)))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), NA)

  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm(formula = ~ X)))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), "The QV-model formula ~X is invalid.")

  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm(formula = Y ~ X)))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv), "The QV-model formula ~X is invalid.")

  # q_glm formula default is A * (.), and A is not used when fitting the
  # QV-model.
  qv <- policy_learn(type = "rqvl",
                     rqvl_args = list(qv_models = q_glm()))
  expect_error(policy_eval(policy_data = pd,
                           policy_learn = qv))
})


## BOWL --------------------------------------------------------------------

test_that("the implementation of bowl agrees with direct application of DTRlearn2::owl in the two stage case.",{
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(5e2, seed=1)
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

  pl <- policy_learn(type = "bowl",
                     bowl_args = list(policy_vars = c("B", "L", "C"),
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

})

# Multiple stages ---------------------------------------------------------

# BOWL ---------------------------------------------------------------------

test_that("input to policy_learn with type bowl handles incorrect input.",{
  source(system.file("sim", "multi_stage.R", package="polle"))
  d <- sim_multi_stage(5e2, seed = 1)
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

  pl <- policy_learn(type = "bowl")

  expect_error(policy_eval(policy_data = pd,
                           policy_learn = pl), "bowl is only implemented for a fixed number of stages.")
})
