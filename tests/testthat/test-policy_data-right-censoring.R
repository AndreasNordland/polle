test_that("policy_data checks checks time and time2 input.", {

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

  ## baseline data:
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
                      time = "time1"),
    "'data' has a variable called \"time\", but 'time' = \"time1\". Please remove or rename 'time'."
  )

  expect_error(
    policy_data(data = ld[stage == 1],
                action = "A",
                utility = "U",
                covariates = c("Z", "time"),
                id = "id",
                type = "wide",
                time = NULL),
    "'covariates' can not have named elements \"event\", \"stage\", \"time\", or \"time2\"."
  )

  ## checking time and time2 input:
  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    timetest = c(1, 2, 0.5, 1, 1.5),
    U = c(0, 10, 0, 0, NA),
    U_A0 = c(0,0,0,0,NA),
    U_A1 = c(0,0,0,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  expect_error(
    pd <- policy_data(data = ld,
                      baseline_data = bd,
                      type = "long",
                      time = "timetest"),
    NA
  )
  expect_equal(
    pd$stage_data$time2,
    ld$timetest
  )
  expect_equal(
    pd$stage_data$time,
    c(0,1,0,0,1)
  )

  ## checking time and time2 input:
  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    time = c(1, 2, 0.5, NA, 1.5),
    U = c(0, 10,0, 0, NA),
    U_A0 = c(0,0,0,0,NA),
    U_A1 = c(0,0,0,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  expect_error(
    pd <- policy_data(data = ld,
                      baseline_data = bd,
                      type = "long",
                      time = "time"),
    "time has missing values"
  )

  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    time = c(1, 2, 0.5, 1, 1.5),
    time2 = c(2, 2.2, 1, NA, 2),
    U = c(0, 10, 0, 0, NA),
    U_A0 = c(0,0,0,0,NA),
    U_A1 = c(0,0,0,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  expect_error(
    pd <- policy_data(data = ld,
                      baseline_data = bd,
                      type = "long",
                      time = "time",
                      time2 = "time2"),
    "time2 has missing values"
  )

  ld <- data.table(
    id = c(1,1,2,3,3),
    stage = c(1,2,1,1,2),
    event = c(0,1,2,0,2),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    time = c(1, 2, 0.5, 1, 1.5),
    time2 = c(2, 2.2, 1, 0.8, 2),
    U = c(0, 10, 0, 0, NA),
    U_A0 = c(0,0,0,0,NA),
    U_A1 = c(0,0,0,0,NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  expect_error(
    pd <- policy_data(data = ld,
                      baseline_data = bd,
                      type = "long",
                      time = "time",
                      time2 = "time2"),
    "time2 must be greater than or equal to time for all pairwise elements."
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
    time = c(0, 1, 0, 0, 1),
    time2 = c(1, 2, 0.5, 1, 1.5),
    A = c("0", NA, NA, "1", NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3"),
    Z = c("A", NA, "A", "B", NA),
    L = c(1, 2, 2, 1, 3),
    U = c(0, 10, 0, 0, NA),
    U_A0 = c(0,0,0,0,NA),
    U_A1 = c(0,0,0,0,NA)
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
                    time = "time")

  ## stage_data and baseline_data should not change:
  expect_equal(ld, pd$stage_data)
  expect_equal(bd, pd$baseline_data)

})


test_that("policy_data formats right censored wide data correctly in the single stage case (single final outcome)", {

  ## stage 1 covariates and a single utility outcome
  ref_ld <- data.table(
    id = c(1,1,2,2,3,3),
    stage = c(1,2,1,2,1,2),
    event = c(0,1,0,2,0,2),
    time = c(0,0,0,0,0,0),
    time2 = c(0,2,0,1,0,1.5),
    A = c("0", NA, "1", NA, "1", NA),
    B = c("gr1", NA, "gr2", NA, "gr3", NA),
    Z = c("A", NA, "A", NA, "B", NA),
    L = c(1, NA, 2, NA, 3, NA),
    U = c(0, 10, 0, NA, 0, NA),
    U_A0 = c(0,NA,0,NA,0,NA),
    U_A1 = c(0,NA,0,NA,0,NA)
  )
  setkey(ref_ld, id,stage)
  setindex(ref_ld, event)

  wd <- data.table(
    id = c(1,2,3),
    A = c("0","1","1"),
    B = c("gr1","gr2","gr3"),
    Z = c("A","A","B"),
    L = c(1,2,3),
    U = c(10, NA, NA),
    time = c(2,1,1.5)
  )

  pd <- policy_data(data = wd,
                    type = "wide",
                    id = "id",
                    action = "A",
                    covariates = c("B", "Z", "L"),
                    utility = "U",
                    time = "time")

  expect_equal(
    pd$stage_data,
    ref_ld,
    check.attributes = FALSE
  )

  ## ## vector of covarites and utility
  ## ref_ld <- data.table(
  ##   id = c(1,1,2,2,3,3),
  ##   stage = c(1,2,1,2,1,2),
  ##   event = c(0,1,0,2,0,2),
  ##   time = c(0,0,0,0,0,0),
  ##   time2 = c(0,2,0,1,0,1.5),
  ##   A = c("0", NA, "1", NA, "1", NA),
  ##   B = c("gr1", NA, "gr2", NA, "gr3", NA),
  ##   Z = c("A", NA, "A", NA, "B", NA),
  ##   L = c(1, 4, 2, 5, 3, 6), # stage 2 values non-NA
  ##   U = c(5, 10, 7, NA, 3, NA), # stage 1 utility contributions
  ##   U_A0 = c(0,NA,0,NA,0,NA),
  ##   U_A1 = c(0,NA,0,NA,0,NA)
  ## )
  ## setkey(ref_ld, id,stage)
  ## setindex(ref_ld, event)

  ## wd <- data.table(
  ##   id = c(1,2,3),
  ##   A = c("0","1","1"),
  ##   B = c("gr1","gr2","gr3"),
  ##   Z = c("A","A","B"),
  ##   L = c(1,2,3),
  ##   L2 = c(3,5,6),
  ##   U = c(10, NA, NA),
  ##   time = c(2,1,1.5)
  ## )

  ## pd <- policy_data(data = wd,
  ##                   type = "wide",
  ##                   id = "id",
  ##                   action = "A",
  ##                   covariates = list(
  ##                     B = c("B", NA),
  ##                     Z = c("Z", NA),
  ##                     L = c("L", "L2")
  ##                   ),
  ##                   utility = "U",
  ##                   time = "time")

  ## expect_equal(
  ##   pd$stage_data,
  ##   ref_ld,
  ##   check.attributes = FALSE
  ## )

})
