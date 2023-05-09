test_that("policy_g_functions returns a policy which selects the most probable action.",{
  d <- sim_single_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")


  g_functions <- fit_g_functions(pd, g_models = g_glm())
  pol <- polle:::policy_g_functions(g_functions = g_functions)

  expect_true(
    inherits(pol, "policy")
  )

  tmp <- predict(g_functions, pd)
  tmp2 <- as.character(as.numeric((tmp[,4] > tmp[,3])))

  expect_equal(
    pol(pd)$d,
    tmp2
  )
})
