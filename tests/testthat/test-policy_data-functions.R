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

test_that("full_history() returns the history associated with the action process when event_set = c(0).", {

  # long data
  ld <- data.table(
    id = c(1,1,3,3,4,4),
    stage = c(1,2,1,2,1,2),
    event = c(0,1,0,1,0,1),
    A = c("0", NA, "1", NA,"0", NA),
    B = c("gr1","gr1", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", NA, "B", NA, "B", NA),
    L = c(1, 2, 1, 3, 3,4),
    U = c(0.1, 10, 0.2, 0.3, 0.4, 0.5),
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
  fh <- full_history(pd, stage = 1, event_set = c(0))

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

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    U_bar = c(0.1, 0.2, 0.4),
    U_A0 = c(0,0,0),
    U_A1 = c(0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

  ## stage 2
  expect_error(
    fh <- full_history(pd, stage = 2, event_set = c(0)),
    "empty history for the given stage and event_set."
  )

})

test_that("full_history() returns the history associated with the event process when event_set = c(0,2).", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")


  ## stage 1:
  fh <- full_history(pd, stage = 1, event_set = c(0,2))

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    B_1 = c("gr1", "gr2", "gr3", "gr4"),
    Z_1 = c("A", "A", "B", "B"),
    L_1 = c(1,2,1,3),
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

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    time = c(0,0,0,0),
    time2 = c(1,0.5,1,1)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    U_bar = c(0.1, 0.3, 0.4, 0.6),
    U_A0 = c(0,0,0,0),
    U_A1 = c(0,0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

  ## stage 2:
  fh <- full_history(pd, stage = 2, event_set = c(0,2))

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    A_1 = c("1","0"),
    B_1 = c("gr3", "gr4"),
    B_2 = c("gr3", "gr4"),
    Z_1 = c("B", "B"),
    Z_2 = as.character(c(NA, NA)),
    L_1 = c(1,3),
    L_2 = c(3,4),
    W = c("blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    event = c(2,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    time = c(1,1),
    time2 = c(1.5,1.2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    U_bar = as.numeric(c(NA, NA)),
    U_A0 = c(0,0),
    U_A1 = c(0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

})

test_that("full_history() returns the history associated with the event process when event_set = c(0,1,2)", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")


  ## stage 1:
  fh <- full_history(pd, stage = 1, event_set = c(0,1,2))

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    B_1 = c("gr1", "gr2", "gr3", "gr4","gr1"),
    Z_1 = c("A", "A", "B", "B","B"),
    L_1 = c(1,2,1,3,6),
    W = c("blue", "red", "blue", "red","red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    event = c(0,2,0,0,1)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    time = c(0,0,0,0,0),
    time2 = c(1,0.5,1,1,1.3)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    U_bar = c(0.1, 0.3, 0.4, 0.6,0.4),
    U_A0 = c(0,0,0,0,0),
    U_A1 = c(0,0,0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

  ## stage 2:
  fh <- full_history(pd, stage = 2, event_set = c(0,1,2))

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

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    time = c(1,1,1),
    time2 = c(2,1.5,1.2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    U_bar = c(0.3, NA, NA),
    U_A0 = c(0,0,0),
    U_A1 = c(0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

})


test_that("stage_state_history() returns the history associated with the action process when event_set = c(0)", {

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
    U = c(0.1, 10,0.2, 0.3, 0.4, 0.5),
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
                    time = "time",
                    type = "long")

  ## stage 1:
  fh <- stage_state_history(pd, stage = 1, event_set = c(0))

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

  ref <- data.table(
    id = c(1,3,4),
    stage = c(1,1,1),
    U_bar = c(0.1, 0.2, 0.4),
    U_A0 = c(0,0,0),
    U_A1 = c(0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

  ## stage 2
  expect_error(
    fh <- stage_state_history(pd, stage = 2, event_set = c(0)),
    "empty history for the given stage and event_set."
  )

})

test_that("stage_state_history() returns the history associated with the event process when event_set = c(0,2)", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")

  ## stage 1:
  fh <- stage_state_history(pd, stage = 1, event_set = c(0,2))

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    B = c("gr1", "gr2", "gr3", "gr4"),
    Z = c("A", "A", "B", "B"),
    L = c(1,2,1,3),
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

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    time = c(0,0,0,0),
    time2 = c(1,0.5,1,1)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4),
    stage = c(1,1,1,1),
    U_bar = c(0.1, 0.3, 0.4, 0.6),
    U_A0 = c(0,0,0,0),
    U_A1 = c(0,0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

  ## stage 2
  fh <- stage_state_history(pd, stage = 2, event_set = c(0,2))

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    B = c("gr3", "gr4"),
    Z = as.character(c(NA, NA)),
    L = c(3,4),
    W = c("blue", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    event = c(2,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    time = c(1,1),
    time2 = c(1.5,1.2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(3,4),
    stage = c(2,2),
    U_bar = as.numeric(c(NA, NA)),
    U_A0 = c(0,0),
    U_A1 = c(0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

})

test_that("stage_state_history() returns the history associated with the event process when event_set = c(0,1,2)", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")

  ## stage 1:
  fh <- stage_state_history(pd, stage = 1, event_set = c(0,1,2))

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    B = c("gr1", "gr2", "gr3", "gr4", "gr1"),
    Z = c("A", "A", "B", "B", "B"),
    L = c(1,2,1,3,6),
    W = c("blue", "red", "blue", "red","red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    event = c(0,2,0,0,1)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,4,5),
    stage = c(1,1,1,1,1),
    time = c(0,0,0,0,0),
    time2 = c(1,0.5,1,1,1.3)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ## stage 2
  fh <- stage_state_history(pd, stage = 2, event_set = c(0,1,2))

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    B = c("gr1","gr3", "gr4"),
    Z = as.character(c(NA,NA, NA)),
    L = c(2,3,4),
    W = c("blue","blue", "red")
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

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    time = c(1,1,1),
    time2 = c(2,1.5,1.2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  ref <- data.table(
    id = c(1,3,4),
    stage = c(2,2,2),
    U_bar = as.numeric(c(0.3,NA, NA)),
    U_A0 = c(0,0,0),
    U_A1 = c(0,0,0)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$U,
    ref
  )

})


test_that("state_history() returns the history associated with the action process when event_set = c(0)", {

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
    U = c(0.2, 10, 0.3, 0, 0.4, 0),
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
                    time = "time",
                    type = "long")


  ## stage 1:
  fh <- state_history(pd, event_set = c(0))

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

  expect_equal(
    fh$U,
    NULL
  )

})




test_that("state_history() returns the history associated with the event process when type = 'censoring'", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")


  fh <- state_history(pd, event_set = c(0,2))

  ref <- data.table(
    id = c(1,2,3,3,4,4),
    stage = c(1,1,1,2,1,2),
    B = c("gr1","gr2", "gr3", "gr3", "gr4", "gr4"),
    Z = c("A", "A", "B", NA, "B", NA),
    L = c(1, 2, 1, 3, 3,4),
    W = c("blue","red", "blue", "blue", "red", "red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,3,4,4),
    stage = c(1,1,1,2,1,2),
    event = c(0,2,0,2,0,2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(1,2,3,3,4,4),
    stage = c(1,1,1,2,1,2),
    time = c(0,0,0,1,0,1),
    time2 = c(1,0.5,1,1.5,1,1.2)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

})

test_that("state_history() returns the history associated with the event process when type = 'all'", {

  # long data
  ## 5 cases: no right censoring, right censored before/at stage 1 action,
  ## 2Xright censored utility outcome (at stage 2), terminal at stage 1
  ld <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1),
    A = c("0", NA, NA, "1", NA,"0", NA, NA),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4", "gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA, "B"),
    L = c(1,2,2,1,3,3,4,6),
    time = c(1, 2, 0.5,1,1.5,1,1.2,1.3),
    U = c(0.1, 0.2,0.3,0.4,NA,0.6,NA,0.4),
    U_A0 = c(0,0,0,0,0,0,0,0),
    U_A1 = c(0,0,0,0,0,0,0,0)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2,3,4,5),
    W = c("blue","red","blue","red","red")
  )
  setkey(bd, id)

  pd <- policy_data(data = ld,
                    baseline_data = bd,
                    time = c("time"),
                    type = "long")


  fh <- state_history(pd, event_set = c(0,1,2))

  ref <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    B = c("gr1","gr1", "gr2", "gr3", "gr3", "gr4", "gr4","gr1"),
    Z = c("A", NA, "A", "B", NA, "B", NA,"B"),
    L = c(1, 2, 2, 1, 3, 3,4,6),
    W = c("blue", "blue", "red", "blue", "blue", "red", "red","red")
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$H,
    ref
  )

  ref <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    event = c(0,1,2,0,2,0,2,1)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$event,
    ref
  )

  ref <- data.table(
    id = c(1,1,2,3,3,4,4,5),
    stage = c(1,2,1,1,2,1,2,1),
    time = c(0,1,0,0,1,0,1,0),
    time2 = c(1,2,0.5,1,1.5,1,1.2,1.3)
  )
  setkey(ref, id, stage)

  expect_equal(
    fh$time,
    ref
  )

  expect_equal(
    fh$U,
    NULL
  )

})

test_that("get_utility returns NA for right-censored observations", {
  sim_single_stage_right_cens <- function(n = 2e3, zeta = c(0.7, 0.2), type = "right"){

    d <- sim_single_stage(n = n)
    pd <- policy_data(data = d,
                      action = "A",
                      covariates = c("Z", "L", "B"),
                      utility = "U")

    ld <- pd$stage_data

    ld[stage == 1, time := 1]
    ld[stage == 2, time := 2]

    ld[stage == 2, Z := d$Z]
    ld[stage == 2, L := d$L]
    ld[stage == 2, B := d$B]

    ## simulating the right censoring time
    ## only depending on the baseline covariate Z:
    C <- c(rexp(n, 1) / exp((-1) * cbind(1, as.numeric(d$Z)) %*% zeta))

    ld[stage == 1, time_c := C]
    ld[stage == 2, time_c := C]

    ld[, delta := time_c >= time]

    ld[delta == FALSE , event := 2]
    ld[delta == FALSE, A := NA]
    ld[delta == FALSE & stage == 2, U := NA]
    ld[delta == FALSE, U_A0 := 0]
    ld[delta == FALSE, U_A1 := 0]

    ld[ , tmp := shift(delta, fill = TRUE), by = list(id)]
    ld <- ld[tmp == TRUE, ]
    ld[ , time := pmin(time, time_c)]
    ld[ , time_c := NULL]
    ld[ , tmp := NULL]
    ld[ , delta := NULL]

    if (type == "interval"){
      ld[, time2 := time]
      ld[, time := shift(time, fill = 0), by = list(id)]
    }

    return(ld)
  }

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  expect_equal(
    ld[ , list(na = any(event == 2)), by = id][["na"]],
    is.na(get_utility(pd)[["U"]])
  )

})
