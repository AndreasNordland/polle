test_that("g_xgboost gives the same result as plain xgboost",{
  # data
  d <- sim_single_stage(1000, seed=1)
  xgboost_data = xgboost::xgb.DMatrix(data = as.matrix(d[,c("Z", "L", "B")]), label = d[["A"]])

  set.seed(1)
  bst <- xgboost::xgboost(data = xgboost_data,
                 max_depth = 2, eta = 1, nrounds = 2,
                 objective = "binary:logistic",
                 verbose = FALSE)

  pred <- predict(bst, xgboost_data)

  # policy data
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  set.seed(1)
  g_fun <- fit_g_functions(pd,
                           g_models = g_xgboost(max_depth = 2, eta = 1, nrounds = 2))

  pred_polle <- predict(g_fun, pd)

  expect_equal(
    pred,
    pred_polle[["g_1"]]
  )

})

test_that("g_xgboost gives the same result as SL.xgboost",{
  # policy data
  d <- sim_single_stage(1000, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  g_fun <- fit_g_functions(pd,
                           g_models = g_xgboost(max_depth = 2, eta = 1, nrounds = 1))
  pred_1 <- predict(g_fun, pd)

  xgboost_tune <- list(max_depth = 2,
                         shrinkage = 1, # eta
                         ntrees  = 1) # nrounds
  sl_learner <- create.Learner("SL.xgboost",tune = xgboost_tune)
  g_fun_sl <- fit_g_functions(pd,
                              g_models = g_sl(SL.library = sl_learner$names, env = environment()))
  pred_2 <- predict(g_fun_sl, pd)

  expect_equal(
    pred_1,
    pred_2
  )

})

test_that("g_xgboost tunes parameters",{
  # data
  d <- sim_single_stage(1000, seed=1)
  # policy data
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  set.seed(1)
  g_fun <- fit_g_functions(pd,
                           g_models = g_xgboost(max_depth = 2, eta = c(0.3,1), nrounds = c(2,4)))

  expect_equal(
    length(g_fun$all_stages$g_model$cv_res$cv),
    3 * 2 * 2 * 2
  )

})
