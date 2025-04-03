test_that("policy_data checks checks censored_covariates input.", {

  ## long data
  ## 3 cases: no right censoring, right censored before/at stage 1 action,
  ## right censored utility outcome (at stage 2)
  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    time = c(1, 2, 0.5, 1, 1.5),
    U = c(0, 10, NA, 0, NA),
    U_A0 = c(0,0,NA,0,NA),
    U_A1 = c(0,0,NA,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

                                        # baseline data:
  bd <- data.table(
    id = c(1,2,3),
    W = c("blue", "red", "blue")
  )
  setkey(bd, id)

  ## input checks:
  expect_error(
    pd <- policy_data(data = ld,
                      baseline_data = bd,
                      type = "long",
                      censoring_covariates = c("time1")),
    "invalid censoring_covariates."
  )

  expect_warning(
    policy_data(data = ld[stage == 1],
                action = "A",
                utility = "U",
                covariates = c("Z"),
                id = "id",
                type = "wide",
                censoring_covariates = c("time1")),
    "censoring_covariates is not used when type = 'wide'."
  )

})

test_that("policy_data formats right censored long data correctly in the single stage case.", {

  # long data
  ## 3 cases: no right censoring, right censored before/at stage 1 action,
  ## right censored utility outcome (at stage 2)
  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    time = c(1, 2, 0.5, 1, 1.5),
    U = c(0, 10, NA, 0, NA),
    U_A0 = c(0,0,NA,0,NA),
    U_A1 = c(0,0,NA,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  ## baseline data:
  bd <- data.table(
    id = c(1,2,3),
    W = c("blue", "red", "blue")
  )
  setkey(bd, id)


  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    type = "long",
                    censoring_covariates = c("time"))

  ## stage_data and baseline_data should not change:
  expect_equal(ld, pd$stage_data)
  expect_equal(bd, pd$baseline_data)

  ## censoring_names:
  expect_equal(
    pd$colnames$censoring_names,
    "time"
  )


  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    type = "long",
                    censoring_covariates = c("time", "Z"))
  expect_equal(
    pd$colnames$censoring_names,
    c("time", "Z")
  )


  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    type = "long",
                    censoring_covariates = list("time", "Z"))
  expect_equal(
    pd$colnames$censoring_names,
    c("time", "Z")
  )
})

test_that("a policy_data object prints as expected under right censoring occur.", {

  # long data
  ## 4 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2)
  ld <- data.table(
    id = c(1,1,2,3,3,4,4),
    stage = c(1,2,1,1,2,1,2),
    event = c(0,1,2,0,2,0,2),
    A = c("0", NA, NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "A", "B", NA, "B", NA),
    L = c(1, 2, 2, 1, 3, 3,4),
    time = c(1, 2, 0.5, 1, 1.5, 1, 1.2),
    U = c(0, 10, NA, 0, NA, 0, NA),
    U_A0 = c(0,0,NA,0,NA, 0, NA),
    U_A1 = c(0,0,NA,0,NA, 0, NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4),
    W = c("blue", "red", "blue", "red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld, baseline_data = bd, type = "long")

  invisible(
    capture.output(
      expect_error(
        print(pd),
        NA
      )
    )
  )

})

test_that("full_data returns the history associated with the censoring process when 'include_censoring = TRUE'", {

  # long data
  ## 4 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2)
  ld <- data.table(
    id = c(1,1,2,3,3,4,4),
    stage = c(1,2,1,1,2,1,2),
    event = c(0,1,2,0,2,0,2),
    A = c("0", NA, NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "A", "B", NA, "B", NA),
    L = c(1, 2, 2, 1, 3, 3,4),
    time = c(1, 2, 0.5, 1, 1.5, 1, 1.2),
    U = c(0, 10, NA, 0, NA, 0, NA),
    U_A0 = c(0,0,NA,0,NA, 0, NA),
    U_A1 = c(0,0,NA,0,NA, 0, NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4),
    W = c("blue", "red", "blue", "red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    censoring_covariates = c("time"),
                    type = "long")


  ## stage 1:
  fh <- full_history(pd, stage = 1, censoring = TRUE)

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    B_1 = c("gr1", "gr2", "gr3", "gr4"),
    Z_1 = c("A", "A", "B", "B"),
    L_1 = c(1,2,1,3),
    time_1 = c(1,0.5,1,1),
    W = c("blue", "red", "blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    event_1 = c(0,2,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  expect_equal(
    fh$event_name,
    "event_1"
  )

  ## stage 2
  fh <- full_history(pd, stage = 2, censoring = TRUE)

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    A_1 = c("0","1","0"),
    B_1 = c("gr1","gr3", "gr4"),
    B_2 = c("gr1","gr3", "gr4"),
    Z_1 = c("A", "B", "B"),
    Z_2 = as.character(c(NA, NA, NA)),
    L_1 = c(1,1,3),
    L_2 = c(2,3,4),
    time_1 = c(1,1,1),
    time_2 = c(2,1.5,1.2),
    W = c("blue", "blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    event_2 = c(1,2,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  expect_equal(
    fh$event_name,
    "event_2"
  )



})
