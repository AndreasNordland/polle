test_that("get_history() checks input", {
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

test_that("full_data() returns the history associated with the censoring process when 'censoring = FALSE'", {

  # long data
  ld <- data.table(
    id = c(1,1,3,3,4,4),
    stage = c(1,2,1,2,1,2),
    event = c(0,1,0,1,0,1),
    A = c("0", NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "B", NA, "B", NA),
    L = c(1, 2, 1, 3, 3,4),
    U = c(0, 10, 0, 0, 0, 0),
    U_A0 = c(0,NA,0,NA, 0, NA),
    U_A1 = c(0,NA,0,NA, 0, NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,3,4),
    W = c("blue", "blue", "red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    type = "long")


  ## stage 1:
  fh <- full_history(pd, stage = 1, censoring = FALSE)

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    B_1 = c("gr1", "gr3", "gr4"),
    Z_1 = c("A", "B", "B"),
    L_1 = c(1,1,3),
    W = c("blue", "blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    A_1 = c("0","1","0")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$A,
    ref
  )

  expect_equal(
    fh$action_name,
    "A_1"
  )

  ## stage 2
  expect_error(
    fh <- full_history(pd, stage = 2, censoring = FALSE),
    "The stage number must be lower or equal to maximal number of stages observed."
  )
})

test_that("stage_state_history() returns the history associated with the censoring process when 'censoring = FALSE'", {

  # long data
  ld <- data.table(
    id = c(1,1,3,3,4,4),
    stage = c(1,2,1,2,1,2),
    event = c(0,1,0,1,0,1),
    A = c("0", NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "B", NA, "B", NA),
    L = c(1, 2, 1, 3, 3,4),
    time = c(1, 2, 1, 1.5, 1, 1.2),
    U = c(0, 10, 0, 0, 0, 0),
    U_A0 = c(0,0,0,NA, 0, NA),
    U_A1 = c(0,0,0,NA, 0, NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,3,4),
    W = c("blue", "blue", "red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    censoring_covariates = "time",
                    type = "long")


  ## stage 1:
  fh <- stage_state_history(pd, stage = 1, censoring = FALSE)

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    B = c("gr1", "gr3", "gr4"),
    Z = c("A", "B", "B"),
    L = c(1,1,3),
    W = c("blue", "blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    A = c("0","1","0")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$A,
    ref
  )

  expect_equal(
    fh$action_name,
    "A"
  )

  ## stage 2
  expect_error(
    fh <- stage_state_history(pd, stage = 2, censoring = FALSE),
    "The stage number must be lower or equal to maximal number of stages observed."
  )
})


test_that("state_history() returns the history associated with the censoring process when 'censoring = FALSE'", {

  # long data
  ld <- data.table(
    id = c(1,1,3,3,4,4),
    stage = c(1,2,1,2,1,2),
    event = c(0,1,0,1,0,1),
    A = c("0", NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "B", NA, "B", NA),
    L = c(1, 2, 1, 3, 3,4),
    time = c(1, 2, 1, 1.5, 1, 1.2),
    U = c(0, 10, 0, 0, 0, 0),
    U_A0 = c(0,0,0,NA, 0, NA),
    U_A1 = c(0,0,0,NA, 0, NA)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,3,4),
    W = c("blue", "blue", "red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    censoring_covariates = "time",
                    type = "long")


  ## stage 1:
  fh <- state_history(pd, censoring = FALSE)

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    B = c("gr1","gr3","gr4"),
    Z = c("A", "B", "B"),
    L = c(1, 1, 3),
    W = c("blue", "blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    A = c("0","1","0")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$A,
    ref
  )

  expect_equal(
    fh$action_name,
    "A"
  )
})

test_that("full_history() returns the history associated with the censoring process when 'censoring = TRUE'", {

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
    U = c(0, 0, NA, 0, NA, 0, NA),
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

test_that("stage_state_history() returns the history associated with the censoring process when 'censoring = TRUE'", {

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
    U = c(0, 0, NA, 0, NA, 0, NA),
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
  fh <- stage_state_history(pd, stage = 1, censoring = TRUE)

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    B = c("gr1", "gr2", "gr3", "gr4"),
    Z = c("A", "A", "B", "B"),
    L = c(1,2,1,3),
    time = c(1,0.5,1,1),
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
    event = c(0,2,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  expect_equal(
    fh$event_name,
    "event"
  )

  ## stage 2
  fh <- stage_state_history(pd, stage = 2, censoring = TRUE)

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    B = c("gr1","gr3", "gr4"),
    Z = as.character(c(NA, NA, NA)),
    L = c(2,3,4),
    time = c(2,1.5,1.2),
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
    event = c(1,2,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  expect_equal(
    fh$event_name,
    "event"
  )
})

test_that("state_history() returns the history associated with the censoring process when 'censoring = TRUE'", {

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
    U = c(0, 0, NA, 0, NA, 0, NA),
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


  fh <- state_history(pd, censoring = TRUE)

  ref <- data.table(
    id = c(1,1,2,3,3,4,4),
    stage = c(1,2,1,1,2,1,2),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "A", "B", NA, "B", NA),
    L = c(1, 2, 2, 1, 3, 3,4),
    time = c(1, 2, 0.5, 1, 1.5, 1, 1.2),
    W = c("blue", "blue", "red", "blue", "blue", "red", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,1,2,3,3,4,4),
    stage = c(1,2,1,1,2,1,2),
    event = c(0,1,2,0,2,0,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  expect_equal(
    fh$event_name,
    "event"
  )

})
