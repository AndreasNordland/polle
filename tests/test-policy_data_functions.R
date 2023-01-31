test_that("get_history checks input", {

  d <- sim_single_stage(1e2, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  expect_error(
    get_history(pd,
                full_history = "test"),
    "full_history must be TRUE or FALSE"
  )

  expect_error(
    get_history(pd,
                full_history = logical(0)),
    "full_history must be TRUE or FALSE"
  )

  expect_error(
    get_history(pd,
                full_history = c(TRUE, TRUE)),
    "full_history must be TRUE or FALSE"
  )

})
