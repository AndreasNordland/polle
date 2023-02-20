
test_that("g_sl suppresses rank-deficient warnings",{
  d <- sim_single_stage(n = 1e2)
  d$Z2 <- d$Z*2
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "Z2", "B", "L"),
                    utility="U")

  expect_no_warning(
    pe <- policy_eval(
      type = "or",
      policy = policy_def(1),
      policy_data = pd,
      q_models = q_sl()
    )
  )
})
