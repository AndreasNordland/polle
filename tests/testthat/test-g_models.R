library("data.table")

test_that("predict.g_functions checks the action set",{
  d <- sim_two_stage_multi_actions(n = 1e2)

  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  expect_error(
    gfit <- fit_g_functions(pd, g_models = list(g_glm(), g_rf()), full_history = TRUE),
    NA
  )

  d2 <- copy(d)
  d2$A_1[d2$A_1 == "no"] <- "maybe"

  pd2 <- policy_data(data = d2,
                     action = c("A_1", "A_2"),
                     baseline = c("B", "BB"),
                     covariates = list(L = c("L_1", "L_2"),
                                       C = c("C_1", "C_2")),
                     utility = c("U_1", "U_2", "U_3"))

  expect_error(
    predict(gfit, pd2),
    "factor A_1 has new levels maybe"
  )

  expect_equal(
    {
      gval <- predict.g_function(gfit[[1]],
                                 get_history(pd2, stage = 1,
                                             full_history = TRUE))
      names(gval)
    },
    c("id", "stage", paste("g_", pd2$action_set, sep = ""))
  )

  d3 <- copy(d2)
  d3$A_2[d3$A_2 == "no"] <- "maybe"

  pd3 <- policy_data(data = d3,
                     action = c("A_1", "A_2"),
                     baseline = c("B", "BB"),
                     covariates = list(L = c("L_1", "L_2"),
                                       C = c("C_1", "C_2")),
                     utility = c("U_1", "U_2", "U_3"))

  expect_error(
    predict(gfit, pd3),
    "The fitted stage action set is not a subset of the new action set."
  )

})

test_that("fit_g_functions handles varying stage-action sets", {
  d <- sim_two_stage_multi_actions(n = 1e2)
  expect_error(
    pd <- policy_data(data = d,
                      action = c("A_1", "A_2"),
                      baseline = c("B", "BB"),
                      covariates = list(L = c("L_1", "L_2"),
                                        C = c("C_1", "C_2")),
                      utility = c("U_1", "U_2", "U_3")),
    NA
  )

  expect_error(
    gfit <- fit_g_functions(pd, g_models = list(g_glm(), g_rf()), full_history = TRUE),
    NA
  )
  expect_error(
    predict(gfit, pd),
    NA
  )

  expect_error(
    gfit <- fit_g_functions(pd, g_models = list(g_glm(), g_rf()), full_history = FALSE),
    NA
  )
  expect_error(
    predict(gfit, pd),
    NA
  )

  expect_error(
    gfit <- fit_g_functions(pd, g_models = g_glm(), full_history = FALSE),
    "g_glm requires exactly two levels."
  )

  set.seed(1)
  folds <- list(c(1:30), 31:get_n(pd))
  expect_error(
    gfit <- fit_g_functions_cf(folds,
                               policy_data = pd,
                               g_models = list(g_glm(), g_rf()),
                               full_history = FALSE,
                               save_cross_fit_models = TRUE),
    NA
  )
})

test_that("fit_g_function_cf saves the cross-fitted models",{
  d <- sim_two_stage_multi_actions(n = 1e2)
  expect_error(
    pd <- policy_data(data = d,
                      action = c("A_1", "A_2"),
                      baseline = c("B", "BB"),
                      covariates = list(L = c("L_1", "L_2"),
                                        C = c("C_1", "C_2")),
                      utility = c("U_1", "U_2", "U_3")),
    NA
  )

  set.seed(1)
  folds <- list(c(1:30), 31:get_n(pd))
  expect_error(
    gfit <- fit_g_functions_cf(folds,
                               policy_data = pd,
                               g_models = list(g_glm(), g_rf()),
                               full_history = FALSE,
                               save_cross_fit_models = TRUE),
    NA
  )

  expect_true(
    all(!unlist(lapply(gfit$functions, is.null)))
  )

  set.seed(1)
  folds <- list(c(1:30), 31:get_n(pd))
  expect_error(
    gfit <- fit_g_functions_cf(folds,
                               policy_data = pd,
                               g_models = list(g_glm(), g_rf()),
                               full_history = FALSE,
                               save_cross_fit_models = FALSE),
    NA
  )
  expect_true(
    all(unlist(lapply(gfit$functions, is.null)))
  )

})

test_that("g_models checks formula input", {
  d <- sim_two_stage(1e2, seed = 1)
  pd <- policy_data(d,
    action = c("A_1", "A_2"),
    baseline = c("BB", "B"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  p_dynamic <- policy_def(
    policy_functions = list(
      function(L_1) (L_1 > 0) * 1,
      function(C_2) (C_2 > 0) * 1
    ),
    reuse = FALSE
  )

  expect_error(policy_eval(
    policy_data = pd,
    policy = p_dynamic,
    g_models = g_glm(formula = A ~ X)
  ), "object 'X' not found when calling 'g_glm' with formula:
AA ~ X")
  expect_error(policy_eval(
    policy_data = pd,
    policy = p_dynamic,
    g_models = g_sl(formula = a ~ X)
  ), "object 'X' not found when calling model.frame with formula:
a ~ X")
  expect_error(policy_eval(
    policy_data = pd,
    policy = p_dynamic,
    g_models = g_rf(formula = ~X)
  ), "object 'X' not found when calling model.frame with formula:
~X")
  expect_error(policy_eval(
    policy_data = pd,
    policy = p_dynamic,
    g_models = g_glmnet(formula = ~X)
  ), "object 'X' not found when calling model.frame with formula:
AA ~ X")
  expect_error(policy_eval(
    policy_data = pd,
    policy = p_dynamic,
    g_models = g_empir(formula = ~X)
  ), "The g-model formula ~X is invalid.")
})


test_that("g_rf runs:", {
  d1 <- sim_single_stage(200, seed = 1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
    action = "A",
    covariates = list("Z", "B", "L", "BB"),
    utility = "U"
  )

  expect_no_error(
    pe <- policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "ql", alpha = 0.05),
      g_models = g_rf(formula = ~.),
      g_full_history = FALSE,
      type = "ipw"
    )
  )
})


test_that("g_sl formats data correctly via the formula", {
  d1 <- sim_single_stage(200, seed = 1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
    action = "A",
    covariates = list("Z", "B", "L", "BB"),
    utility = "U"
  )

  library("SuperLearner")
  env <- as.environment("package:SuperLearner")
  expect_error(
    policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "ql", alpha = 0.05),
      g_models = g_sl(formula = ~., env = env),
      g_full_history = FALSE,
      q_models = q_glm()
    ),
    NA
  )
})

test_that("g_sl can find user-defined learners", {
  d1 <- sim_single_stage(200, seed = 1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
    action = "A",
    covariates = list("Z", "B", "L", "BB"),
    utility = "U"
  )

  library("SuperLearner")
  env <- as.environment("package:SuperLearner")
  env <- new.env(parent = env)
  env$SL.test <- function(Y, X, newX, family, obsWeights, model = TRUE, ...) {
    if (is.matrix(X)) {
      X = as.data.frame(X)
    }
    fit.glm <- glm(Y ~ .,
      data = X, family = family, weights = obsWeights,
      model = model
    )
    if (is.matrix(newX)) {
      newX = as.data.frame(newX)
    }
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.test"
    out <- list(pred = pred, fit = fit)
    return(out)
  }
  env$predict.SL.test <- function(object, newdata, ...) {
    if (is.matrix(newdata)) {
      newdata = as.data.frame(newdata)
    }
    pred <- predict(
      object = object$object, newdata = newdata,
      type = "response"
    )
    pred
  }

  g_model <- g_sl(
    formula = ~.,
    SL.library = "SL.test",
    env = env
  )
  his <- get_history(pd1)

  expect_error(
    {
      g_model(
        H = polle:::get_H(his),
        A = polle:::get_A(his),
        action_set = pd1$action_set
      )
    },
    NA
  )
})

test_that("g_glmnet formats data correctly via the formula",{
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    pe <- policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "ql", alpha = 0.05),
      g_models = g_glmnet(formula = ~.),
      g_full_history = FALSE,
      q_models = q_glm()
    ),
    NA
  )
})


# missing values ----------------------------------------------------------

test_that("g_glm handles missing covariates", {
  d <- sim_two_stage(2e3, seed=1)
  d$C_1 <- NULL
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c(NA, "C_2")), # C_1 is missing
                    utility = c("U_1", "U_2", "U_3"))
  p <- policy_def(1, reuse = TRUE)

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw"),
    "NA/NaN/Inf in 'x'"
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = list(g_glm(), g_glm()),
                type = "ipw"),
    "NA/NaN/Inf in 'x'"
  )

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = g_glm(~L),
                q_models = q_glm(~L)),
    NA
  )
  expect_error(
    pe <- policy_eval(policy_data = pd,
                policy = p,
                g_models = list(g_glm(~L), g_glm()),
                q_models = list(q_glm(~L), q_glm())),
    NA
  )

  d <- sim_two_stage(2e3, seed=1)
  d$C_1[1:10] <- NA
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")), # C_1 is missing
                    utility = c("U_1", "U_2", "U_3"))
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw"),
    "NA/NaN/Inf in 'x'"
  )
})

test_that("g_glmnet handles missing covariates", {
  d <- sim_two_stage(2e3, seed=1)
  d$C_1 <- NULL
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c(NA, "C_2")), # C_1 is missing
                    utility = c("U_1", "U_2", "U_3"))
  p <- policy_def(1, reuse = TRUE)

  expect_error(
    suppressWarnings({
      pe <- policy_eval(policy_data = pd,
                        policy = p,
                        type = "ipw",
                        g_models = list(g_glmnet(), g_glmnet()))
    })
  )

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw",
                g_models = g_glmnet(~L + B)),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw",
                g_models = list(g_glmnet(~L + B), g_glmnet())),
    NA
  )
  d <- sim_two_stage(2e3, seed=1)
  d$C_1[1:10] <- NA
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")), # C_1 is missing
                    utility = c("U_1", "U_2", "U_3"))
  expect_error(
    expect_warning(
      policy_eval(policy_data = pd,
                  policy = p,
                  type = "ipw",
                  g_models = g_glmnet()),
      "The regression variables C have missing NA values."
    )
  )

})
