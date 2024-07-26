test_that("predict.q_glmnet return a vector", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
      action = "A",
      covariates = list("Z", "B", "L"),
      utility = "U"
      )

  his <- get_history(pd, stage = 1)
  Q <- get_utility(pd)$U

  qfun <- fit_Q_function(history = his, Q = Q, q_model = q_glmnet())

  qm <- qfun$q_model

  AH <- cbind(A = get_A(his), get_H(his))
  pred <- predict.q_glmnet(qm, new_AH = AH)

  expect_true(
      is.vector(pred)
  )
})

test_that("q_glmnet formats data correctly via the formula", {
    d1 <- sim_single_stage(200, seed = 1)
    d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
    pd1 <- policy_data(d1,
        action = "A",
        covariates = list("Z", "B", "L", "BB"),
        utility = "U"
    )

    expect_error(
        pe <- policy_eval(
            policy_data = pd1,
            policy_learn = policy_learn(type = "ql", alpha = 0.05),
            q_models = q_glmnet(formula = ~ A * .)
        ),
        NA
    )
})


test_that("q_glmnet handles missing covariates", {
    d <- sim_two_stage(2e3, seed = 1)
    d$C_1 <- NULL
    pd <- policy_data(d,
        action = c("A_1", "A_2"),
        baseline = c("BB", "B"),
        covariates = list(
            L = c("L_1", "L_2"),
            C = c(NA, "C_2")
        ), # C_1 is missing
        utility = c("U_1", "U_2", "U_3")
    )
    p <- policy_def(1, reuse = TRUE)

    expect_error(
        suppressWarnings({
            pe <- policy_eval(
                policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet()
            )
        })
    )


    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            type = "or",
            q_models = q_glmnet(~ L + A)
        ),
        NA
    )
    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            type = "or",
            q_models = list(q_glmnet(~ L + A), q_glmnet())
        ),
        NA
    )

    d <- sim_two_stage(2e3, seed = 1)
    d$C_1[1:10] <- NA
    pd <- policy_data(d,
        action = c("A_1", "A_2"),
        baseline = c("BB", "B"),
        covariates = list(
            L = c("L_1", "L_2"),
            C = c("C_1", "C_2")
        ),
        utility = c("U_1", "U_2", "U_3")
    )

    expect_error(
        expect_warning(
            policy_eval(
                policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet()
            ),
            "The regression variables C have missing NA values."
        )
    )
})
