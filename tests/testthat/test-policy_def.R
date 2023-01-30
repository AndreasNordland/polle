
test_that("policy_def checks the action set",{
  d <- sim_single_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  p <- policy_def("test")

  expect_warning(
    p(pd),
    "The policy actions does not comply with the action set of the policy data object."
  )

  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U",
                    action_set = c("1", "0", "test"))

  expect_error(
    p(pd),
    NA
  )

})

# Single stage ------------------------------------------------------------

test_that("policy_def handles static policies (single stage).",{
  d <- sim_single_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  p <- policy_def(1)
  expect_equal(
    p(pd)[["d"]],
    rep("1", get_n(pd))
  )

  p <- policy_def("1")
  expect_equal(
    p(pd)[["d"]],
    rep("1", get_n(pd))
  )

  p <- policy_def(as.factor("1"))
  expect_equal(
    p(pd)[["d"]],
    rep("1", get_n(pd))
  )

  p <- policy_def(TRUE)
  expect_warning(
    p(pd)[["d"]],
    "The policy actions does not comply with the action set of the policy data object."
  )

  p <- policy_def(c(1))
  expect_equal(
    p(pd)[["d"]],
    rep("1", get_n(pd))
  )

  p <- policy_def(list(1))
  expect_equal(
    p(pd)[["d"]],
    rep("1", get_n(pd))
  )

  p <- policy_def(list(2))
  expect_warning(
    p(pd)[["d"]],
    "The policy actions does not comply with the action set of the policy data object."
  )

  p <- list(
    policy_def(c(0,1)),
    policy_def(c("0","1")),
    policy_def(as.factor(c("0","1"))),
    policy_def(list("0","1"))
  )
  lapply(p,function(p){
    expect_error(
      p(pd),
      "policy_functions must be a list of length K."
    )
  })

  ll <- list(
    c(0,1),
    c("0","1"),
    as.factor(c("0","1")),
    list("0","1")
  )
  lapply(ll,function(l){
    expect_error(
      policy_def(ll, reuse = TRUE),
      "When reuse is TRUE, policy_functions must be a single function or a constant."
    )
  })

})

test_that("policy_def handles dynamic policies (single stage).",{
  d <- sim_single_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  fun <- function(L) (L>0)*1
  res <- do.call(what = function(L,...) as.character(fun(L)), d)

  p <- policy_def(fun, name = "a=(L>0)")
  expect_equal(
    p(pd)[["d"]],
    res
  )
  expect_equal(
    attr(p, "name"),
    "a=(L>0)"
  )

  p <- policy_def(fun, reuse = TRUE)
  expect_equal(
    p(pd)[["d"]],
    res
  )

  expect_error(
    policy_def(fun, reuse = TRUE, full_history = TRUE),
    "full_history must be FALSE when reuse is TRUE."
  )

  p <- policy_def(list(fun), reuse = TRUE)
  expect_equal(
    p(pd)[["d"]],
    res
  )

  expect_error(
    policy_def(list(fun, fun), reuse = TRUE),
    "When reuse is TRUE, policy_functions must be a single function or a constant."
  )
})

# Two stages ------------------------------------------------------------

test_that("policy_def handles static policies (two stages).",{
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  a <- list("1",
            1,
            list(1),
            c(1))
  tmp <- lapply(
    a,
    function(a){
      p <- policy_def(a, reuse = TRUE)
      expect_equal(p(pd)[["d"]],
                   rep("1", get_n(pd) * get_K(pd)))
    }
  )

  a <- list(c("1", "1"),
            c(1,1),
            list(1,1),
            as.factor(c(1,1)))
  tmp <- lapply(
    a,
    function(a){
      p <- policy_def(a, reuse = FALSE)
      expect_equal(p(pd)[["d"]],
                   rep("1", get_n(pd) * get_K(pd)))
    }
  )

  p <- policy_def(c(0,1), reuse = FALSE)
  expect_equal(
    p(pd)[["d"]],
    as.character(rep(c(0,1), times = get_n(pd)))
  )

  p <- policy_def(2, reuse = TRUE)
  expect_warning(
    p(pd),
    "The policy actions does not comply with the action set of the policy data object."
  )

  p <- policy_def(c(1,2), reuse = FALSE)
  expect_warning(
    p(pd),
    "The policy actions does not comply with the action set of the policy data object."
  )

  p <- policy_def(c(1,1,1), reuse = FALSE)
  expect_error(
    p(pd),
    "policy_functions must be a list of length K."
  )

})

test_that("policy_def handles dynamic policies (two stages).",{
  d <- sim_two_stage(2e3, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  fun <- function(L) (L>0)*1
  res <- do.call(pd[["stage_data"]][event == 0, ], what = function(L,...) as.character(fun(L)))

  p <- policy_def(fun, name = "a=(L>0)")
  expect_error(
    p(d),
    'The policy input is not of inherited class policy_data.'
  )
  expect_error(
    p(pd),
    "policy_functions must be a list of length K."
  )

  p <- policy_def(fun, reuse = TRUE, name = "a=(L>0)")
  expect_equal(
    p(pd)[["d"]],
    res
  )
  expect_equal(
    attr(p, "name"),
    "a=(L>0)"
  )

  expect_error(
    policy_def(fun, reuse = TRUE, full_history = TRUE),
    "full_history must be FALSE when reuse is TRUE."
  )

  p <- policy_def(list(fun, fun), reuse = FALSE, name = "a=(L>0)")
  expect_equal(
    p(pd)[["d"]],
    res
  )

  fun <- list(function(L) (L>0)*1, function(C) (C>0)*1)
  res <- do.call(pd[["stage_data"]][event == 0, ], what = function(L, C, stage,...){
    (stage == 1) * as.numeric((L>0)) +
      (stage == 2) * as.numeric((C>0))
  })
  res <- as.character(res)

  p <- policy_def(fun, reuse = FALSE, name = "a=(L>0)")
  expect_equal(
    p(pd)[["d"]],
    res
  )

  fun <- list(function(L_1) (L_1>0)*1, function(C_2) (C_2>0)*1)
  p <- policy_def(fun, reuse = FALSE, name = "a=(L>0)", full_history = TRUE)
  expect_equal(
    p(pd)[["d"]],
    res
  )

  fun <- list(function(L_2) (L_2>0)*1, function(C_2) (C_2>0)*1)
  p <- policy_def(fun, reuse = FALSE, name = "a=(L>0)", full_history = TRUE)
  expect_error(
    p(pd)[["d"]],
    "argument \"L_2\" is missing, with no default"
  )

})

# Stochastic number of stages ---------------------------------------------

test_that("policy_def handles a stochastic number of stages", {
  d <- sim_multi_stage(1e3, seed = 1)
  # constructing policy_data object:
  pd <- policy_data(data = d$stage_data,
                    baseline_data = d$baseline_data,
                    type = "long",
                    id = "id",
                    stage = "stage",
                    event = "event",
                    action = "A",
                    utility = "U")

  fun <- function(X, ...) as.character((X>0)*1)
  res <- do.call(pd[["stage_data"]][event == 0,], what = fun)

  p <- policy_def(function(X) (X>0)*1, reuse = TRUE)
  expect_equal(
    p(pd)[["d"]],
    res
  )

  p <- policy_def(replicate(get_K(pd), function(X) (X>0)*1), reuse = FALSE)
  expect_equal(
    p(pd)[["d"]],
    res
  )

})
