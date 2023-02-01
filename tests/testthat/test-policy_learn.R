
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

  # L: number of folds
  expect_error(
    policy_learn(type = "ql",
                 L = "test")
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 0)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = -1)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 1.1)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 1),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 2),
    NA
  )

  # cross_fit_g_models
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = TRUE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = FALSE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = "test"),
    "cross_fit_g_models must be TRUE or FALSE"
  )

  # cross_fit_g_models
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = TRUE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = FALSE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = "test"),
    "save_cross_fit_models must be TRUE or FALSE"
  )

  # future_args
  fa <- list(
    "test",
    NULL,
    1,
    TRUE
  )
  lapply(
    fa, function(fa){
      expect_error(
        policy_learn(type = "ql",
                     future_args =  fa)
      )
    })
  fa <- list(
    list(),
    list(future_seed = TRUE)
  )
  lapply(
    fa, function(fa){
      expect_error(
        policy_learn(type = "ql",
                     future_args =  fa),
        NA
      )
    })

  # name
  nn <- list(1, TRUE, "test")
  lapply(
    nn, function(name){
      expect_error(
        policy_learn(type = "ql",
                     name = name),
        NA
      )
    })

  nn <- list(c(1,1), NULL)
  lapply(
    nn, function(name){
      expect_error(
        policy_learn(type = "ql",
                     name = name)
      )
    })

})
