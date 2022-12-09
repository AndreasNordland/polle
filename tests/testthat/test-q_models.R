
test_that("q_rf",{
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
      g_models = g_glm(),
      g_full_history = FALSE,
      q_models = q_rf()
    ),
    NA
  )

})

test_that("q_sl formats data correctly via the formula",{
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
        g_models = g_glm(),
        g_full_history = FALSE,
        q_models = q_sl(env = env)
      ) }),
    NA
  )
})

test_that("q_sl can find user-defined learners",{
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

  run_Q_function <- function(policy_data, q_model){
    his <- get_history(policy_data)
    AH <- cbind(A = polle:::get_A(his), polle:::get_H(his))
    rm(his)
    V_res <- get_utility(policy_data)$U
    out <- q_model(V_res = V_res, AH = AH)
    return(out)
  }

  expect_error(
    {
      q_fun <- run_Q_function(policy_data = pd1, q_model = q_sl(SL.library = "SL.test", env = env))
    },
    NA
  )
})
