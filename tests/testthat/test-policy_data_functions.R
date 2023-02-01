test_that("get_history checks input", {
  d <- sim_single_stage(1e2, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  # stage
  ss <- list("test", logical(0), c(TRUE, TRUE), -1, 1.1, 0)
  lapply(ss, function(ss){
    expect_error(
      get_history(pd,
                  stage = ss),
      "stage must be an integer greater than 0."
    )

  })
  ss <- list(1)
  lapply(ss, function(ss){
    expect_error(
      get_history(pd,
                  stage = ss),
      NA
    )

  })
  expect_error(
    get_history(pd,
                stage = 2)
  )


  # full_history
  fh <- list("test", logical(0), c(TRUE, TRUE))
  lapply(fh, function(fh){
    expect_error(
      get_history(pd,
                  full_history = fh)
    )

  })
  fh <- list(TRUE, TRUE)
  lapply(fh, function(fh){
    expect_error(
      get_history(pd,
                  stage = 1,
                  full_history = fh),
      NA
    )

  })
  rm(fh)
})
