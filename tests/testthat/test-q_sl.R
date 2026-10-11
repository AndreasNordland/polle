test_that("sl_library_env() parents SuperLearner namespace and preserves user overrides", {
  skip_if_not_installed("SuperLearner")

  env <- new.env(parent = globalenv())
  env$SL.user <- function(...) stop("stub user learner")

  e <- polle:::sl_library_env(env, c("SL.user", "SL.glm", "SL.mean"))

  ## The parent chain must reach the SuperLearner namespace -- this is
  ## what makes All, SL.mean, SL.glm, screening algorithms etc. resolve
  ## via get(..., envir = e) under future::plan("multisession").
  expect_identical(environmentName(parent.env(e)), "SuperLearner")

  ## User-supplied override lives in e directly.
  expect_true(exists("SL.user", envir = e, inherits = FALSE))
  expect_identical(get("SL.user", envir = e, inherits = FALSE), env$SL.user)

  ## Names available only in SuperLearner's namespace are resolvable
  ## through e via the parent chain.
  for (nm in c("SL.glm", "SL.mean", "All")) {
    expect_identical(
      get(nm, envir = e),
      get(nm, envir = asNamespace("SuperLearner"))
    )
  }
})

test_that("check_superlearner() errors with a helpful hint when the package is missing", {
  ## Rebind requireNamespace in a copy of check_superlearner so that the
  ## guard fires without needing to actually un-install SuperLearner.
  f <- polle:::check_superlearner
  environment(f) <- new.env(parent = environment(polle:::check_superlearner))
  environment(f)$requireNamespace <- function(...) FALSE
  expect_error(f("q_sl"), "install\\.packages\\(\"SuperLearner\"\\)")
})

test_that("q_sl with discreteSL = TRUE picks the learner with the lowest cvrisk.", {
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("ranger")

  d <- sim_single_stage(200, seed = 1)
  d$BB <- sample(c("group 1", "group & 2", "group & 3"),
                 size = 200, replace = TRUE)
  pd <- policy_data(d,
                    action = "A",
                    covariates = list("Z", "B", "L", "BB"),
                    utility = "U")
  p <- policy_def(1)

  set.seed(1)
  qfun <- polle:::fit_Q_functions(pd,
                                  p(pd),
                                  q_sl(SL.library = c("SL.ranger", "SL.glm"),
                                       discreteSL = TRUE))

  expect_true(
    all(qfun$stage_1$q_model$model$coef %in% c(0, 1)),
    sum(qfun$stage_1$q_model$model$coef) == 1
  )
})

test_that("q_sl() works under future::plan('multisession') with SuperLearner only Suggested", {
  skip_on_cran()
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("future.apply")

  d <- sim_two_stage(200, seed = 1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  old <- future::plan("multisession", workers = 2)
  on.exit(future::plan(old), add = TRUE)

  expect_no_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = policy_def(1, reuse = TRUE),
      g_models = g_sl(SL.library = c("SL.mean", "SL.glm")),
      q_models = q_sl(SL.library = c("SL.mean", "SL.glm")),
      M = 2,
      future_args = list(future.seed = TRUE)
    )
  )
  expect_s3_class(pe, "policy_eval")
})
