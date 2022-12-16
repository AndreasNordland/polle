
test_that("g_rf",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    pe <- policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "rql", alpha = 0.05),
      g_models = g_rf(formula = ~.),
      g_full_history = FALSE,
      q_models = q_glm()
    ),
    NA
  )

})

test_that("g_sl formats data correctly via the formula",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  library("SuperLearner")
  env <- as.environment("package:SuperLearner")
  expect_error(
    suppressWarnings({
      policy_eval(
        policy_data = pd1,
        policy_learn = policy_learn(type = "rql", alpha = 0.05),
        g_models = g_sl(formula = ~., env = env),
        g_full_history = FALSE,
        q_models = q_glm()
      ) }),
    NA
  )
})

test_that("g_sl can find user-defined learners",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  library("SuperLearner")
  env <- as.environment("package:SuperLearner")
  env <- new.env(parent = env)
  env$SL.test <- function (Y, X, newX, family, obsWeights, model = TRUE, ...)
  {
    if (is.matrix(X)) {
      X = as.data.frame(X)
    }
    fit.glm <- glm(Y ~ ., data = X, family = family, weights = obsWeights,
                   model = model)
    if (is.matrix(newX)) {
      newX = as.data.frame(newX)
    }
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.test"
    out <- list(pred = pred, fit = fit)
    return(out)
  }
  env$predict.SL.test <- function (object, newdata, ...)
  {
    if (is.matrix(newdata)) {
      newdata = as.data.frame(newdata)
    }
    pred <- predict(object = object$object, newdata = newdata,
                    type = "response")
    pred
  }

  g_model <- g_sl(formula = ~.,
                  SL.library = "SL.test",
                  env = env)
  his <- get_history(pd1)

  expect_error({

      g_model(H = polle:::get_H(his),
              A = polle:::get_A(his),
              action_set = pd1$action_set)

  },NA)
})

test_that("g_glmnet formats data correctly via the formula",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    pe <- policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "rql", alpha = 0.05),
      g_models = g_glmnet(formula = ~.),
      g_full_history = FALSE,
      q_models = q_glm()
    ),
    NA
  )
})


# missing values ----------------------------------------------------------

test_that("g_glm handles missing covariates", {
  source(system.file("sim", "two_stage.R", package="polle"))
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
  source(system.file("sim", "two_stage.R", package="polle"))
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
    }),
    NA # glmnet ignores all NA regressors!
  )
  expect_warning(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw",
                g_models = list(g_glmnet(), g_glmnet()))
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
  expect_warning(
    policy_eval(policy_data = pd,
                policy = p,
                type = "ipw",
                g_models = g_glmnet()),
    "The regression variables C have missing NA values."
  )
})
