test_that("fit_q_functions handles varying stage-action sets", {

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
    qfit <- fit_Q_functions(pd, policy_actions = p(pd), q_models = list(q_glm(), q_glm()), full_history = FALSE, m_function = NULL),
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

  ## glm
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_glm(formula = Y~X)),
               "variable 'X' is not found in data when calling 'q_glm' with formula: ~X")
  ## ns() lives in the base 'splines' package. Previously attached
  ## transitively via SuperLearner -> gam; load it explicitly now that
  ## SuperLearner is no longer a hard dependency of polle.
  library("splines")
  expect_no_error(policy_eval(policy_data = pd,
                              policy = p_dynamic,
                              q_models = q_glm(formula = ~ns(C))))

  ## xgboost
  expect_error(policy_eval(policy_data = pd,
                           policy = p_dynamic,
                           q_models = q_xgboost(formula = Y~X, nrounds = 2)),
               "variable 'X' is not found in data when calling 'q_xgboost' with formula: ~X")

})

test_that("q_models checks formula input 2", {

  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
                    action = "A",
                    covariates = list("Z", "B", "L"),
                    utility = "U"
                    )

  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(formula = ~ A*.))
  )
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = g_glm()),
    "Error in blip_model: variable 'A' is not found in data when calling 'q_glm' with formula: ~A \\+ Z \\+ B \\+ L \\+ A:Z \\+ A:B \\+ A:L"
  )

  ## A in global environment (formula defined in the global environment):
  A <- d$A

  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(formula = ~ A*.))
  )
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = g_glm()),
    "Error in blip_model: variable 'A' is not found in data when calling 'q_glm' with formula: ~A \\+ Z \\+ B \\+ L \\+ A:Z \\+ A:B \\+ A:L"
  )

})

test_that("q_rf formats data correctly via the formula",{
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
      g_models = g_glm(),
      g_full_history = FALSE,
      q_models = q_rf()
    ),
    NA
  )
})

test_that("q_sl formats data correctly via the formula", {
  skip_if_not_installed("SuperLearner")
  d1 <- sim_single_stage(200, seed = 1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"),
                  size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action = "A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility = "U")

  expect_no_error(
    policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "ql", alpha = 0.05),
      g_models = g_glm(),
      g_full_history = FALSE,
      q_models = q_sl()
    )
  )
})

test_that("q_sl can find user-defined learners", {
  skip_if_not_installed("SuperLearner")
  d <- sim_single_stage(200, seed = 1)
  d$BB <- sample(c("group 1", "group & 2", "group & 3"),
                 size = 200, replace = TRUE)
  pd <- policy_data(d,
                    action = "A",
                    covariates = list("Z", "B", "L", "BB"),
                    utility = "U")
  p <- policy_def(1)

  env <- new.env(parent = globalenv())
  with(env, {
    SL.test <- function(Y, X, newX, family, obsWeights, model = TRUE, ...) {
      if (is.matrix(X)) X <- as.data.frame(X)
      fit.glm <- glm(Y ~ ., data = X, family = family,
                     weights = obsWeights, model = model)
      if (is.matrix(newX)) newX <- as.data.frame(newX)
      pred <- predict(fit.glm, newdata = newX, type = "response")
      fit <- list(object = fit.glm)
      class(fit) <- "SL.glm" # SL.test
      list(pred = pred, fit = fit)
    }
  })

  expect_no_error(
    polle:::fit_Q_functions(pd,
                            p(pd),
                            q_sl(SL.library = "SL.test", env = env),
                            m_function = NULL)
  )
})

test_that("q_glm and q_sl(SL.library('SL.glm')) are (almost) equivalent", {
  skip_if_not_installed("SuperLearner")
  d1 <- sim_single_stage(200, seed = 1)
  d1$A <- as.character(d1$A)
  d1$BB <- sample(c("group 1", "group_2", "G & 4"),
                  size = nrow(d1), replace = TRUE)

  q1 <- q_glm(formula = ~.)
  q2 <- q_sl(formula = ~., SL.library = "SL.glm")

  q1_fit <- q1(AH = d1[, c("A", "B", "Z", "L", "BB")], V_res = d1$U)
  q2_fit <- q2(AH = d1[, c("A", "B", "Z", "L", "BB")], V_res = d1$U)

  expect_equal(
    unname(coef(q1_fit$model)),
    unname(coef(q2_fit$model$fitLibrary$SL.glm_All$object))
  )
})


# missing data ------------------------------------------------------------


test_that("q_glm handles missing covariates", {
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
                g_models = g_glm(~L + B),
                q_models = q_glm(~L)),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = g_glm(~L + B),
                q_models = list(q_glm(~L), q_glm())),
    NA
  )
})

test_that("q_glm detects aliased terms with the rank tolerance of lm", {
  set.seed(1)
  n <- 2000
  AH <- data.frame(A = factor(rbinom(n, 1, 0.5), levels = 0:1), L = rnorm(n))
  # collinear with the intercept up to 1e-9: kept by the glm() default
  # tolerance (1e-11), but aliased for lm() (1e-7).
  AH$lag <- 1 + 1e-9 * rnorm(n)
  V_res <- rbinom(n, 1, 0.01)

  fit_glm <- function(...) {
    q_glm(~ A * ., ...)(AH = AH, V_res = V_res)$model
  }

  default <- fit_glm()
  expect_equal(default$rank, 4)
  expect_equal(sum(is.na(coef(default))), 2)
  expect_lt(max(abs(coef(default)), na.rm = TRUE), 1)
  expect_true(all(is.finite(predict(default, newdata = AH, type = "response"))))
  expect_equal(
    unname(coef(default)[!is.na(coef(default))]),
    unname(coef(lm(V_res ~ A * L, data = AH))),
    tolerance = 1e-8
  )

  # glm() default is recovered via control
  expect_equal(fit_glm(control = glm.control())$rank, 6)

  # a user supplied control is not overruled
  expect_equal(fit_glm(control = glm.control(epsilon = 1e-4))$rank, 4)
  expect_equal(fit_glm(control = glm.control(epsilon = 1e-4, maxit = 10))$control$maxit, 10)

  # family given as a character string or function
  expect_equal(fit_glm(family = "gaussian")$rank, 4)
  expect_equal(fit_glm(family = gaussian)$rank, 4)
})

test_that("q_glm only changes the glm control for gaussian identity models", {
  set.seed(1)
  n <- 500
  AH <- data.frame(A = factor(rbinom(n, 1, 0.5), levels = 0:1), L = rnorm(n))
  V_res <- rbinom(n, 1, plogis(AH$L))

  qb <- q_glm(~ A * L, family = binomial())(AH = AH, V_res = V_res)$model
  expect_equal(qb$control$epsilon, 1e-8)
  expect_equal(
    unname(coef(qb)),
    unname(coef(glm(V_res ~ A * L, data = AH, family = binomial())))
  )

  # full rank gaussian: unchanged estimates
  qg <- q_glm(~ A * L)(AH = AH, V_res = V_res)$model
  expect_equal(qg$control$epsilon, 1e-4)
  expect_equal(
    unname(coef(qg)),
    unname(coef(glm(V_res ~ A * L, data = AH))),
    tolerance = 1e-10
  )
})
