# Place at tests/testthat/test-model-fitting-warnings.R in the polle source tree.
# Run: devtools::test(filter = "model-fitting-warnings", stop_on_failure = TRUE)
# These tests use the package's existing testthat runner and dependencies.

make_model_warning_data <- function() {
  # Exactly balanced outcomes, with 24/32 final outcomes observed.
  # Censoring must be present: otherwise the wrappers can bypass these models.
  people <- expand.grid(
    A = 0:1, Y = 0:1, replication = 1:8,
    KEEP.OUT.ATTRS = FALSE
  )
  people$id <- sprintf("w%02d", seq_len(nrow(people)))

  long <- do.call(rbind, lapply(seq_len(nrow(people)), function(i) {
    person <- people[i, ]
    observed <- person$replication <= 6L
    data.frame(
      id = person$id,
      stage = 1:2,
      event = c(0L, if (observed) 1L else 2L),
      A = c(person$A, NA_real_),
      X = 0,
      U = c(0, if (observed) person$Y else NA_real_)
    )
  }))

  polle::policy_data(
    long, type = "long", id = "id", stage = "stage",
    event = "event", action = "A", utility = "U"
  )
}

model_with_test_warning <- function(model, progress) {
  force(model)
  force(progress)
  wrapped <- function(...) {
    fitted <- model(...)
    warning("deliberate model-fit warning", call. = FALSE)
    # This line must execute after warning(), before the fitted object returns.
    progress$completed <- TRUE
    fitted
  }
  class(wrapped) <- class(model)
  wrapped
}

model_with_test_error <- function(model) {
  wrapped <- function(...) {
    stop("deliberate model-fit error", call. = FALSE)
  }
  class(wrapped) <- class(model)
  wrapped
}

testthat::test_that("fit_m_function retains a fitted model after a warning", {
  pd <- make_model_warning_data()
  fit_m <- getFromNamespace("fit_m_function", "polle")
  model <- polle::q_glm(~1, family = stats::gaussian())

  control <- fit_m(pd, m_model = model, full_history = TRUE)
  control_predictions <- as.data.frame(
    stats::predict(control, new_policy_data = pd)
  )
  testthat::expect_false(is.function(control$m_model))
  testthat::expect_equal(nrow(control_predictions), 32L)
  testthat::expect_equal(
    control_predictions$Q, rep(0.5, 32L), tolerance = 1e-8
  )

  progress <- new.env(parent = emptyenv())
  progress$completed <- FALSE
  warning_model <- model_with_test_warning(model, progress)

  # Capture outside the wrapper: an inner suppressWarnings() would hide the bug.
  # Match a substring so this accepts the old prefixed warning as well; the
  # completion, stored-object and prediction checks detect the actual defect.
  testthat::expect_warning(
    fitted <- fit_m(pd, m_model = warning_model, full_history = TRUE),
    "deliberate model-fit warning", fixed = TRUE
  )
  testthat::expect_true(progress$completed)
  testthat::expect_false(is.function(fitted$m_model))

  # Avoid a secondary prediction-dispatch error when running against old code.
  # The two expectations above already fail if the constructor was retained.
  if (!is.function(fitted$m_model)) {
    predictions <- as.data.frame(
      stats::predict(fitted, new_policy_data = pd)
    )
    testthat::expect_equal(
      predictions, control_predictions, tolerance = 1e-8
    )
  }
})

testthat::test_that("fit_c_function retains a fitted g-model after a warning", {
  pd <- make_model_warning_data()
  model <- polle::g_empir()
  control <- polle::fit_c_functions(
    policy_data = pd,
    c_models = list(polle::g_empir(), model), full_history = TRUE
  )
  control_predictions <- as.data.frame(
    stats::predict(control, new_policy_data = pd)
  )
  testthat::expect_false(is.function(control[[2L]]$c_model))
  testthat::expect_equal(nrow(control_predictions), 64L)
  testthat::expect_equal(
    control_predictions$surv_time2 / control_predictions$surv_time,
    ifelse(control_predictions$stage == 1L, 1, 0.75),
    tolerance = 1e-8
  )

  progress <- new.env(parent = emptyenv())
  progress$completed <- FALSE
  warning_model <- model_with_test_warning(model, progress)

  testthat::expect_warning(
    fitted <- polle::fit_c_functions(
      policy_data = pd,
      c_models = list(polle::g_empir(), warning_model), full_history = TRUE
    ),
    "deliberate model-fit warning", fixed = TRUE
  )
  testthat::expect_true(progress$completed)
  testthat::expect_false(is.function(fitted[[2L]]$c_model))

  if (!is.function(fitted[[2L]]$c_model)) {
    predictions <- as.data.frame(
      stats::predict(fitted, new_policy_data = pd)
    )
    testthat::expect_equal(
      predictions, control_predictions, tolerance = 1e-8
    )
  }
})

testthat::test_that("fit_m_function keeps its contextual fitting error", {
  pd <- make_model_warning_data()
  fit_m <- getFromNamespace("fit_m_function", "polle")
  broken_model <- model_with_test_error(
    polle::q_glm(~1, family = stats::gaussian())
  )

  testthat::expect_error(
    fit_m(pd, m_model = broken_model, full_history = TRUE),
    "Error fitting m_model: deliberate model-fit error", fixed = TRUE
  )
})

testthat::test_that("fit_c_function keeps its contextual g-model error", {
  pd <- make_model_warning_data()
  broken_model <- model_with_test_error(polle::g_empir())

  testthat::expect_error(
    polle::fit_c_functions(
      policy_data = pd,
      c_models = list(polle::g_empir(), broken_model), full_history = TRUE
    ),
    "Error fitting c_model: deliberate model-fit error", fixed = TRUE
  )
})
