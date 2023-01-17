test_that("fit_q_functions handles varying stage-action sets", {
  source(system.file("sim", "two_stage_multi_actions.R", package="polle"))
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
  p <- policy_def(
    c("yes", "no")
  )

  expect_error(
    qfit <- fit_Q_functions(pd, policy_actions = p(pd), q_models = list(q_glm(), q_glm()), full_history = FALSE),
    NA
  )
  expect_error(
    tmp <- predict(qfit, pd),
    NA
  )

  tmp2 <- predict(qfit$stage_1$q_model$model,
                  newdata = cbind(A = "yes", get_history(pd, stage = 1)$H[1,]))
  tmp2 <- tmp2 + get_history(pd, stage = 1)$U$U_bar[1]
  expect_equal(unname(tmp2), unname(unlist(tmp[1, "Q_yes"])))

  tmp2 <- predict(qfit$stage_1$q_model$model,
                  newdata = cbind(A = "no", get_history(pd, stage = 1)$H[1,]))
  tmp2 <- tmp2 + get_history(pd, stage = 1)$U$U_bar[1]
  expect_equal(unname(tmp2), unname(unlist(tmp[1, "Q_no"])))

})

test_that("q_models checks formula input", {
  source(system.file("sim", "two_stage.R", package="polle"))
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  p_dynamic <- policy_def(
    policy_functions = list(function(L_1) (L_1>0)*1,
                            function(C_2) (C_2>0)*1),
    reuse = FALSE
  )

  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glm(formula = Y~X)), "The Q-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_sl(formula = res~X)), "The Q-model formula ~X is invalid.")
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_rf(formula = V_res~X * (.))))
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glmnet(formula = Y~X)), "The Q-model formula ~X is invalid.")
})

test_that("q_rf formats data correctly via the formula",{
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

  expect_error(
    suppressWarnings({
      pe <- policy_eval(
        policy_data = pd1,
        policy_learn = policy_learn(type = "rql", alpha = 0.05),
        g_models = g_glm(),
        g_full_history = FALSE,
        q_models = q_sl()
      ) }),
    NA
  )

})

test_that("q_sl can find user-defined learners",{
  library("polle")
  source(system.file("sim", "single_stage.R", package="polle"))
  d <- sim_single_stage(200, seed=1)
  d$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L", "BB"),
                    utility="U")
  p <- policy_def(1)

  env <- as.environment("package:SuperLearner")
  env <- new.env(parent = env)
  with(env,{
    SL.test <- function (Y, X, newX, family, obsWeights, model = TRUE, ...){
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
      class(fit) <- "SL.glm" # SL.test
      out <- list(pred = pred, fit = fit)
      return(out)
    }
  })

  # with(env,{
  #   predict.SL.test <- function (object, newdata, ...){
  #     if (is.matrix(newdata)) {
  #       newdata = as.data.frame(newdata)
  #     }
  #     pred <- predict(object = object$object, newdata = newdata,
  #                     type = "response")
  #     pred
  #   }
  # })

  expect_error(
    qfun <- polle:::fit_Q_functions(pd,
                                    p(pd),
                                    q_sl(SL.library = "SL.test", env = env)),
    NA)


})

test_that("q_glm and q_sl(SL.library('SL.glm')) are (almost) equivalent",{
  library("SuperLearner")
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$A <- as.character(d1$A)
  d1$BB <- sample(c("group 1", "group_2", "G & 4"), size = nrow(d1), replace = TRUE)

  q1 <- q_glm(formula = ~.)
  q2 <- q_sl(formula = ~., SL.library = "SL.glm")

  q1 <- q1(AH = d1[,c("A", "B", "Z", "L", "BB")], V_res = d1$U)
  q2 <- q2(AH = d1[,c("A", "B", "Z", "L", "BB")], V_res = d1$U)

  # names are different
  expect_equal(
    unname(coef(q1$model)),
    unname(coef(q2$model$fitLibrary$SL.glm_All$object))
  )

  q1 <- q_glm(formula = ~.)
  q2 <- q_sl(formula = ~., SL.library = "SL.glm")
  d1$B <- as.character(d1$B)
  pd1 <- policy_data(d1, action = "A", covariates = c("Z", "L", "B", "BB"), utility = "U")
  pe1 <- policy_eval(policy_data = pd1,
              policy = policy_def(1),
              type = "or",
              q_models = q1)
  pe2 <- policy_eval(policy_data = pd1,
                     policy = policy_def(1),
                     type = "or",
                     q_models = q2)

  # names are different
  expect_equal(
    unname(coef(pe1$q_functions$stage_1$q_model$glm_model)),
    unname(coef( pe2$q_functions$stage_1$q_model$fit$fitLibrary$SL.glm_All$object))
  )
})

test_that("q_glmnet formats data correctly via the formula",{
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
      q_models = q_glmnet(formula = ~ A*.)
    ),
    NA
  )
})


# missing data ------------------------------------------------------------


test_that("q_glm handles missing covariates", {
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
                policy = p),
    "NA/NaN/Inf in 'x'"
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glm(~L)),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = list(q_glm(~L), q_glm())),
    NA
  )
})

test_that("q_glmnet handles missing covariates", {
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
                  type = "or",
                  q_models = q_glmnet())
    }),
    NA # glmnet ignores all NA regressors!
  )
  expect_warning(
    pe <- policy_eval(policy_data = pd,
                      policy = p,
                      type = "or",
                      q_models = q_glmnet())
  )

  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet(~L + A)),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = list(q_glmnet(~L + A), q_glmnet())),
    NA
  )

  d <- sim_two_stage(2e3, seed=1)
  d$C_1[1:10] <- NA
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))


  expect_warning(
    policy_eval(policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet()),
    "The regression variables C have missing NA values."
  )
})

