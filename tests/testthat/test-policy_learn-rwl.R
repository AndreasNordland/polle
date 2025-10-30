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

