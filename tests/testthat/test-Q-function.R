# devtools::test(filter = "test-Q-function", stop_on_failure = TRUE)


# The 64-person cohort is exactly balanced across treatment and outcome groups.
# Its always-treat risk is 0.50. When enabled, early outcomes terminate at
# stage 2 and final-stage observation has probability 0.75. All four
# combinations of early outcomes and final-stage censoring are tested.
# M = 1 and empirical/intercept-only models make this a deterministic check.
# The IPW target here is coef_ipw from a DR fit, not the type = "ipw" API.
# The package's existing testthat runner loads the package for these tests.
check_early_terminal_scenario <- function(early, censor) {

  cohort <- expand.grid(
    A1 = 0:1, A2 = 0:1,
    group = c("early", "late", "none1", "none2"), replication = 1:4,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  cohort$id <- sprintf("p%03d", seq_len(nrow(cohort)))
  cohort$Y <- as.numeric(cohort$group %in% c("early", "late"))

  early_person <- early & cohort$group == "early"
  final_observed <- !censor | cohort$replication <= 3L

  long <- do.call(rbind, lapply(seq_len(nrow(cohort)), function(i) {
    last <- if (early_person[i]) 2L else 3L
    observed <- early_person[i] || final_observed[i]
    data.frame(
      id = cohort$id[i], stage = seq_len(last),
      event = c(rep(0L, last - 1L), if (observed) 1L else 2L),
      A = c(cohort$A1[i], if (last == 3L) cohort$A2[i], NA_real_),
      U = c(rep(0, last - 1L), if (observed) cohort$Y[i] else NA_real_),
      X = 0
    )
  }))

  pd <- polle::policy_data(
                 long, type = "long", id = "id", stage = "stage",
                 event = "event", action = "A", utility = "U"
               )

  # Confirm that early outcomes are already known in the package input.
  if (early) {
    observed_utility <- polle::get_utility(pd)
    early_rows <- match(cohort$id[early_person], observed_utility$id)
    testthat::expect_length(early_rows, 16L)
    testthat::expect_false(anyNA(early_rows))
    testthat::expect_equal(
                unname(observed_utility$U[early_rows]), rep(1, 16L)
              )
  }

  args <- list(
    policy_data = pd, policy = polle::policy_def(1, reuse = TRUE),
    type = "dr", target = "value", M = 1L,
    g_models = list(polle::g_empir(), polle::g_empir()),
    q_models = list(polle::q_glm(~1, family = stats::gaussian()),
                    polle::q_glm(~1, family = stats::gaussian())),
    g_full_history = TRUE, q_full_history = TRUE,
    c_full_history = TRUE, m_full_history = TRUE
  )
  if (censor) {
    args$c_models <- list(polle::g_empir(), polle::g_empir(), polle::g_empir())
    args$m_model <- polle::q_glm(~1, family = stats::gaussian())
  }
  pe <- do.call(polle::policy_eval, args)

  # Check each coefficient separately so failures identify the affected field.
  # Equality to a finite scalar also rejects NULL, NA and non-scalar results.
  testthat::expect_equal(
              unname(pe[["coef_or", exact = TRUE]]), 0.5, tolerance = 1e-8
            )
}

testthat::test_that("final outcomes are retained without final-stage censoring", {
  check_early_terminal_scenario(early = FALSE, censor = FALSE)
})

testthat::test_that("early outcomes contribute without final-stage censoring", {
  check_early_terminal_scenario(early = TRUE, censor = FALSE)
})

testthat::test_that("final outcomes are weighted with final-stage censoring", {
  check_early_terminal_scenario(early = FALSE, censor = TRUE)
})

testthat::test_that("early outcomes contribute with final-stage censoring", {
  check_early_terminal_scenario(early = TRUE, censor = TRUE)
})
