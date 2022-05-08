
# check_data --------------------------------------------------------------


test_that("check_data fails if not given a data.table with unique variables.", {
  data <- c("A", "B")
  expect_error(check_data(data), "'data' must be a data.table.")
  baseline_data <- c("A", "B")
  expect_error(check_data(baseline_data), "'baseline_data' must be a data.table.")

  data <- data.table(id = 1:10, id = 10:1)
  expect_error(check_data(data), "'data' has duplicated variable names.")
})


# policy_data wide data ---------------------------------------------------


## two stage ---------------------------------------------------------------

test_that("policy_data melts wide data correctly in a two stage case.", {
  wide_data <- data.table(
    B = c("gr1", "gr2"),
    Z_1 = c("A", "B"),
    L_1 = c(1,2),
    Z_2 = c("C", "D"),
    L_2 = c(3,4),
    treat_1 = c(0,1),
    treat_2 = c(1,0),
    outcome = c(10, 5)
  )

  target_stage_data <- data.table(
    id = c(1,1,1,2,2,2),
    stage = c(1,2,3,1,2,3),
    event = c(0,0,1,0,0,1),
    A = as.character(c(0,1,NA,1,0,NA)),
    Z = c("A","C",NA,"B","D",NA),
    L = c(1,3,NA,2,4,NA),
    U = c(0,0,10,0,0,5),
    U_A0 = rep(0, 6),
    U_A1 = rep(0, 6)
  )
  setkey(target_stage_data, id, stage)
  setindex(target_stage_data, event)

  pd <- policy_data(
    data = wide_data,
    action = c("treat_1", "treat_2"),
    covariates = list(
      Z = c("Z_1", "Z_2"),
      L = c("L_1", "L_2")
    ),
    baseline = "B",
    utility = "outcome"
  )
  expect_equal(pd$stage_data, target_stage_data)

  # including deterministic rewards:
  wide_data <- data.table(
    B = c("gr1", "gr2"),
    Z_1 = c("A", "B"),
    L_1 = c(1,2),
    Z_2 = c("C", "D"),
    L_2 = c(3,4),
    treat_1 = c(0,1),
    treat_2 = c(1,0),
    outcome_1 = c(10, 5),
    outcome_2 = c(7, 3),
    outcome_3 = c(-2, 1),
    outcome_1_A0 = c(1.5, 2.5),
    outcome_2_A0 = c(3.5, 4.5)
  )

  target_stage_data <- data.table(
    id = c(1,1,1,2,2,2),
    stage = c(1,2,3,1,2,3),
    event = c(0,0,1,0,0,1),
    A = as.character(c(0,1,NA,1,0,NA)),
    Z = c("A","C",NA,"B","D",NA),
    L = c(1,3,NA,2,4,NA),
    U = c(10,7,-2,5,3,1),
    U_A0 = c(1.5,3.5,NA,2.5,4.5,NA),
    U_A1 = rep(0, 6)
  )
  setkey(target_stage_data, id, stage)
  setindex(target_stage_data, event)

  pd <- policy_data(
    data = wide_data,
    action = c("treat_1", "treat_2"),
    covariates = list(
      Z = c("Z_1", "Z_2"),
      L = c("L_1", "L_2")
    ),
    baseline = "B",
    utility = c("outcome_1", "outcome_2", "outcome_3"),
    deterministic_rewards = list(U_A0 = c("outcome_1_A0", "outcome_2_A0"))
  )
  expect_equal(pd$stage_data, target_stage_data)

  # invalid inputs:
  args <- list(
    data = wide_data,
    action = c("treat_1", "treat_2"),
    covariates = list(
      Z = c("Z_1", "Z_2"),
      L = c("L_1", "L_2")
    ),
    baseline = "B",
    utility = c("outcome_1", "outcome_2", "outcome_3"),
    deterministic_rewards = list(U_A0 = c("outcome_1_A0", "outcome_2_A0"))
  )

  args_copy <- args
  args_copy$covariates$Z <- c("Z_1", "Z_1")
  expect_error(do.call(what = "policy_data", args_copy), "Duplicated variables: \"Z_1\".")

  args_copy <- args
  args_copy$covariates$Z <- c("Z_1")
  expect_error(do.call(what = "policy_data", args_copy), "Each element in 'covariates' must have length 2.")


})


## single stage ------------------------------------------------------------

test_that("policy_data melts wide data correctly in a single stage case.", {

  wide_data <- data.table(
    B = c("gr1", "gr2"),
    Z = c("A", "B"),
    L = c(1, 2),
    treat = c(0,1),
    outcome = c(10, 5)
  )

  target_stage_data <- data.table(
    id = c(1,1,2,2),
    stage = c(1,2,1,2),
    event = c(0,1,0,1),
    A = c("0", NA, "1", NA),
    B = c("gr1", NA, "gr2", NA),
    Z = c("A", NA, "B", NA),
    L = c(1, NA, 2, NA),
    U = c(0, 10, 0, 5),
    U_A0 = rep(0, 4),
    U_A1 = rep(0, 4)
  )
  setkey(target_stage_data, id, stage)
  setindex(target_stage_data, event)

  # except equal:
  pd <- policy_data(data = wide_data, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome")
  expect_equal(pd$stage_data, target_stage_data)
  rm(pd)

  # duplicate variable names:
  expect_error(policy_data(data = wide_data, action = "treat", covariates = c("B", "Z", "Z"), utility = "outcome"), "Duplicated variables: \"Z\".")
  expect_error(policy_data(data = wide_data, action = "treat", covariates = c("B", "Z", "treat"), utility = "outcome"), "Duplicated variables: \"treat\".")
  expect_error(policy_data(data = wide_data, action = "treat", covariates = c("B", "Z", "L"), utility = "treat"), "Duplicated variables: \"treat\".")

  # invalid variable names:
  wide_data_copy <- copy(wide_data)
  wide_data_copy$stage <- 1
  wide_data_copy$event <- 0
  expect_error(policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "stage"), utility = "outcome"), "'covariates' can not have named elements \"event\" or \"stage\".")
  expect_error(policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "event"), utility = "outcome"), "'covariates' can not have named elements \"event\" or \"stage\".")

  # id input
  wide_data_copy <- copy(wide_data)
  wide_data_copy$id <- c(1,2)
  expect_error(
    policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome"),
    "'data' has a variable id, but 'id' = NULL. Please set 'id' = \"id\" or change the name of the id variable."
  )
  expect_equal(
    policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome", id = "id")$stage_data,
    target_stage_data
  )
  wide_data_copy$id <- NULL
  wide_data_copy$ID <- c(1,2)
  expect_equal(
    policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome", id = "ID")$stage_data,
    target_stage_data
  )

  #
  pd <- policy_data(data = wide_data, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome")
  expect_equal(
    pd$colnames$stage_data_names,
    c("B", "Z", "L")
  )

  # expected baseline
  target_baseline <- data.table(id = c(1,2), B = c("gr1", "gr2"))
  setkey(target_baseline, id)
  pd <- policy_data(data = wide_data, action = "treat", covariates = c("Z", "L"), utility = "outcome", baseline = "B")
  expect_equal(
    pd$baseline_data,
    pd$baseline_data
  )
  expect_equal(
    pd$colnames$baseline_data_names,
    c("B")
  )
  rm(pd)

  # deterministic rewards
  wide_data_copy <- copy(wide_data)
  wide_data_copy$reward_A0 <- 2
  wide_data_copy$reward_A1 <- 1
  expect_error(
    policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome", deterministic_rewards = list("reward_A0", "reward_A1")),
    "'deterministic_rewards' must be a named list with names in the set 'U_A0', 'U_A1'."
  )
  expect_error(
    policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome", deterministic_rewards = list(U_0 = "reward_A0", U_1 = "reward_A1")),
    "'deterministic_rewards' must be a named list with names in the set 'U_A0', 'U_A1'."
  )
  pd <- policy_data(data = wide_data_copy, action = "treat", covariates = c("B", "Z", "L"), utility = "outcome", deterministic_rewards = list(U_A0 = "reward_A0", U_A1 = "reward_A1"))
  expect_identical(
    pd$stage_data[, c("U_A0", "U_A1"), with = FALSE],
    data.table(U_A0 = c(2,NA,2,NA), U_A1 = c(1,NA,1,NA))
  )
})


# policy_data long data ---------------------------------------------------------------

## single stage ------------------------------------------------------------

test_that("policy_data formats long data correctly for a single stage case.", {

  # long data:
  ld <- data.table(
    id = c(1,1,2,2),
    stage = c(1,2,1,2),
    event = c(0,1,0,1),
    A = c("0", NA, "1", NA),
    B = c("gr1", NA, "gr2", NA),
    Z = c("A", NA, "B", NA),
    L = c(1, NA, 2, NA),
    U = c(0, 10, 0, 5),
    U_A0 = rep(0, 4),
    U_A1 = rep(0, 4)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2),
    W = c("blue", "red")
  )
  setkey(bd, id)

  # correct
  expect_error(
    policy_data(data = ld, baseline_data = bd, type = "long"),
    NA
  )
  pd <- policy_data(data = ld, baseline_data = bd, type = "long")
  expect_equal(ld, pd$stage_data)
  expect_equal(bd, pd$baseline_data)

  # invalid variable names
  expect_error(
    policy_data(data = ld, type = "long", id = 1),
    "'id' must be a character string."
  )
  expect_error(
    policy_data(data = ld, type = "long", id = "ID"),
    "'id' is invalid."
  )
  expect_error(
    policy_data(data = ld, type = "long", action = "AA"),
    "'action' is invalid."
  )

  # setting new namesÆ
  ld_copy <- copy(ld)
  setnames(ld_copy, "id", "ID")
  setnames(ld_copy, "stage", "k")
  setnames(ld_copy, "event", "evt")
  setnames(ld_copy, "A", "action")
  setnames(ld_copy, "U", "reward")

  bd_copy <- copy(bd)
  setnames(bd_copy, "id", "ID")

  # correct variable names:
  expect_error(
    policy_data(
      data = ld_copy,
      baseline_data = bd_copy,
      type = "long",
      action = "action",
      id = "ID",
      stage = "k",
      event = "evt",
      utility = "reward"
    ),
    NA
  )
  # comparison with original:
  expect_identical(
    policy_data(
      data = ld_copy,
      baseline_data = bd_copy,
      type = "long",
      action = "action",
      id = "ID",
      stage = "k",
      event = "evt",
      utility = "reward"
    ),
    pd
  )

  # preventing duplicated id variable:
  ld_copy$id <- c(3,3,4,4)
  expect_error(
    policy_data(data = ld_copy, type = "long", id = "ID"),
    "'data' has a variable called \"id\", but 'id' = \"ID\". Please remove or rename 'id'."
  )
  ld_copy$id <- NULL

  # preventing duplicated action variable:
  ld_copy$A <- c(0,NA,0,NA)
  expect_error(
    policy_data(
      data = ld_copy,
      type = "long",
      action = "action",
      id = "ID",
      stage = "k",
      event = "evt",
      utility = "U"
    ),
    "'data' has a variable called \"A\", but 'action' = \"action\". Please remove or rename 'action'."
  )
  ld_copy$A <- NULL

  # missing variable with non-matching default\
  expect_error(
    policy_data(
      data = ld_copy,
      type = "long",
      action = "action",
      id = "ID",
      stage = "k",
      event = "evt"
    ),
    "'utility' is invalid."
  )

})

# new_policy_data missing values ------------------------------------------

test_that("policy_data handles missing values.", {
  # long data:
  ld <- data.table(
    id = c(1,1,2,2),
    stage = c(1,2,1,2),
    event = c(0,1,0,1),
    A = c(0, NA, 1, NA),
    B = c("gr1", NA, "gr2", NA),
    Z = c("A", NA, "B", NA),
    L = c(1, NA, 2, NA),
    U = c(0, 10, 0, 5),
    U_A0 = rep(0, 4),
    U_A1 = rep(0, 4)
  )
  setkey(ld, id, stage)
  setindex(ld, event)

  # baseline data:
  bd <- data.table(
    id = c(1,2),
    W = c("blue", "red")
  )
  setkey(bd, id)

  # correct
  expect_error(
    policy_data(data = ld, baseline_data = bd, type = "long"),
    NA
  )

  # missing values:
  ld$A <- c(NA , NA, 1, NA)
  expect_error(policy_data(data = ld, baseline_data = bd, type = "long"))
  ld$A <- c(0, NA, 1, NA)

  ld$U <- c(NA, 10, 0, 5)
  expect_error(policy_data(data = ld, baseline_data = bd, type = "long"))
  ld$U <- c(0, 10, 0, 5)

  ld$Z <- c(NA, NA, "B", NA)
  expect_error(policy_data(data = ld, baseline_data = bd, type = "long"), NA) # allowing for missing covariates
})


