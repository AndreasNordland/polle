test_that("q_sl with discreteSL = TRUE picks the learner with the lowest cvrisk.",{
  library("polle")
  d <- sim_single_stage(200, seed=1)
  d$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L", "BB"),
                    utility="U")
  p <- policy_def(1)

  set.seed(1)
  qfun <- polle:::fit_Q_functions(pd,
                                  p(pd),
                                  q_sl(SL.library = c("SL.ranger", "SL.xgboost"), discreteSL = TRUE))

  expect_true(
    all(qfun$stage_1$q_model$model$coef %in% c(0,1)),
    sum(qfun$stage_1$q_model$model$coef) == 1
  )

})
