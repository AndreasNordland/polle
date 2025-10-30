test_that("get_policy.earl returns a policy", {
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
