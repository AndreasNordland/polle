
test_that("g_rf",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    policy_eval(
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

  expect_error(
    suppressWarnings({
      policy_eval(
        policy_data = pd1,
        policy_learn = policy_learn(type = "rql", alpha = 0.05),
        g_models = g_sl(formula = ~.),
        g_full_history = FALSE,
        q_models = q_glm()
      ) }),
    NA
  )
})

test_that("g_sl works if the env is deleted",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  SL.test <- function (Y, X, newX, family, obsWeights, model = TRUE, ...)
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
  predict.SL.test <- function (object, newdata, ...)
  {
    if (is.matrix(newdata)) {
      newdata = as.data.frame(newdata)
    }
    pred <- predict(object = object$object, newdata = newdata,
                    type = "response")
    pred
  }

  expect_error(
    suppressWarnings({
      pe <- policy_eval(
        policy_data = pd1,
        policy_learn = policy_learn(type = "rql", alpha = 0.05),
        g_models = g_sl(formula = ~., SL.library = "SL.test"),
        g_full_history = FALSE,
        q_models = q_glm()
      ) }),
    NA
  )
})
