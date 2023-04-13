test_that("q_xgboost gives the same result as plain xgboost",{
  # data
  d <- sim_single_stage(1000, seed=1)
  xgboost_data = xgboost::xgb.DMatrix(data = as.matrix(d[,c("Z", "L", "B", "A")]), label = d[["U"]])

  set.seed(1)
  bst <- xgboost(data = xgboost_data,
                 max_depth = 2, eta = 1, nrounds = 2)

  pred <- predict(bst, xgboost_data)

  # policy data
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  set.seed(1)
  q_fun <- polle:::fit_Q_functions(pd,
                                   policy_actions = policy_def(1)(pd),
                                   q_models = q_xgboost(max_depth = 2, eta = 1, nrounds = 2))

  pred_polle <- predict(q_fun, pd)

  expect_equal(
    pred,
    pred_polle[["Q_1"]] * (d$A == "1") + pred_polle[["Q_0"]] * (d$A == "0")
  )

})
