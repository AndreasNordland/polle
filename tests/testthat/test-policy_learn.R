
test_that("policy_learn checks input", {

  expect_error(policy_learn(type = "test"),
               "Unknown type of policy learner. Use 'ql', 'drql', 'ptl', 'owl', 'earl' or 'rwl'.")

  expect_error(policy_learn(type = c("test", "test")),
               "type must be a character string.")


  d <- sim_single_stage(n=1e2)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  # alpha
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = "test"))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = c(0,1)))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = NULL))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = -0.1))
  )

  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = 0)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = 0.1)),
    NA
  )

  # full_history
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = TRUE)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = FALSE)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = "test")),
    "full_history must be TRUE or FALSE"
  )

})
