test_that("policy_eval returns g and Q-function values.", {
  d <- sim_single_stage(1e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
  p <- policy_def(1)

  pe <- policy_eval(pd, policy = p)

  expect_true(is.data.table(pe$g_values))
  expect_true(nrow(pe$g_values) == 1e2)
  expect_true(all(names(pe$g_values) == c("id", "stage", "g_0", "g_1")))

  expect_true(is.data.table(pe$q_values))
  expect_true(nrow(pe$q_values) == 1e2)
  expect_true(all(names(pe$q_values) == c("id", "stage", "Q_0", "Q_1")))

  pe <- policy_eval(pd, policy = p, type = "ipw")
  expect_true(!is.null(pe$g_values))
  expect_true(is.null(pe$q_values))

  pe <- policy_eval(pd, policy = p, type = "or")
  expect_true(is.null(pe$g_values))
  expect_true(!is.null(pe$q_values))

  ##
  ## cross-fitting
  ##

  pe <- policy_eval(pd, policy = p, M = 2)
  expect_true(is.data.table(pe$g_values))
  expect_true(nrow(pe$g_values) == 1e2)
  expect_true(all(names(pe$g_values) == c("id", "stage", "g_0", "g_1")))
  expect_true(all(key(pe$g_values) == c("id", "stage")))

  expect_true(is.data.table(pe$q_values))
  expect_true(nrow(pe$q_values) == 1e2)
  expect_true(all(names(pe$q_values) == c("id", "stage", "Q_0", "Q_1")))
  expect_true(all(key(pe$q_values) == c("id", "stage")))

  pe <- policy_eval(pd, policy = p, type = "ipw", M = 2)
  expect_true(!is.null(pe$g_values))
  expect_true(is.null(pe$q_values))
})

test_that(
  "policy_eval do not save g and Q-functions when
save_g_functions = FALSE and save_q_functions = FALSE.",
  {
    d <- sim_single_stage(1e2, seed = 1)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(1)

    pe <- policy_eval(pd, policy = p)
    expect_true(!is.null(pe$g_functions))
    expect_true(!is.null(pe$q_functions))

    pe <- policy_eval(pd, policy = p, save_g_functions = FALSE)
    expect_true(is.null(pe$g_functions))
    expect_true(!is.null(pe$q_functions))

    pe <- policy_eval(pd, policy = p, save_q_functions = FALSE)
    expect_true(!is.null(pe$g_functions))
    expect_true(is.null(pe$q_functions))

    pe <- policy_eval(pd, policy = p, save_g_functions = FALSE, M = 2)
    expect_true(is.null(pe$cross_fits[[1]]$g_functions))

    pe <- policy_eval(pd, policy = p, save_q_functions = FALSE, M = 2)
    expect_true(is.null(pe$cross_fits[[1]]$q_functions))
  }
)

test_that("policy_eval checks inputs.", {
  d <- sim_single_stage(1e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")

  p <- policy_def(1)
  pl <- policy_learn()

  ## policy_data
  expect_error(
    policy_eval(
      policy_data = d,
      policy = p
    ),
    "policy_data must be of inherited class 'policy_data'."
  )

  ## policy
  expect_error(
    policy_eval(
      policy_data = pd
    ),
    "Provide either policy or policy_learn."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = function() 1
    ),
    "policy must be of inherited class 'policy'."
  )

  ## type
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      type = "test"
    ),
    "type must be either 'dr', 'ipw' or  'or'."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      type = NULL
    ),
    "type must be a character string."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      type = c("test", "test")
    ),
    "type must be a character string."
  )

  ## policy_learn
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = NULL,
      policy_learn = p
    ),
    "policy_learn must be of inherited class 'policy_learn'."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      policy_learn = p
    ),
    "Provide either policy or policy_learn."
  )

  ## g-models
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      g_model = g_glm()
    ),
    NA
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      g_models = list(g_glm())
    ),
    NA
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      g_model = function() 1
    ),
    "g_models must be a single g_model or a list of K g_models's."
  )

  gfun <- fit_g_functions(pd, g_glm(), FALSE)
  qfun <- fit_Q_functions(pd, q_models = q_glm(), policy_actions = p(pd))

  ## g-functions
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      g_functions = gfun
    ),
    NA
  )
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      g_functions = qfun
    ),
    "g_functions must be of class 'g_functions'."
  )

  ## q-functions
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      q_functions = qfun
    ),
    NA
  )
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_functions = gfun
    ),
    "q-functions must be of class 'q_functions'."
  )

  ## M: cross-fitting
  mm <- list("test", TRUE, c(1, 2), -1, 0, 1.2)
  lapply(mm, function(m) {
    expect_error(
      policy_eval(
        policy_data = pd,
        policy = p,
        M = m
      )
    )
  })
  mm <- list(1, 2)
  lapply(mm, function(m) {
    expect_error(
      policy_eval(
        policy_data = pd,
        policy = p,
        M = m
      ),
      NA
    )
  })
  rm(mm)

  ## names
  nn <- list("test", NULL, 1, TRUE)
  lapply(nn, function(n) {
    expect_error(
      policy_eval(
        policy_data = pd,
        policy = p,
        name = n
      ),
      NA
    )
  })

  nn <- list(c(1, 2), list())
  lapply(nn, function(n) {
    expect_error(
      policy_eval(
        policy_data = pd,
        policy = p,
        name = n
      )
    )
  })
})


test_that("policy_eval runs on a subset of the data with missing actions.", {
  d1 <- sim_single_stage(1e2, seed = 1)
  pd1 <- policy_data(d1, action = "A", covariates = c("Z"), utility = "U")

  pd2 <- subset_id(pd1, id = get_id(pd1)[d1$A == "0"])

  ## ipw
  expect_error(
    pe1_ipw <- policy_eval(pd1, policy = policy_def(1), type = "ipw"),
    NA
  )
  expect_error(
    pe2_ipw <- policy_eval(
      policy_data = pd2,
      g_functions = get_g_functions(pe1_ipw),
      policy = policy_def(1),
      type = "ipw"
    ),
    NA
  )
  expect_equal(
    pe2_ipw$value_estimate,
    0
  )
  expect_equal(
    pe2_ipw$IC,
    rep(0, get_n(pd2))
  )

  ## or
  expect_error(
    pe1_or <- policy_eval(pd1, policy = policy_def(1), type = "or"),
    NA
  )
  expect_error(
    pe2_or <- policy_eval(
      policy_data = pd2,
      q_functions = get_q_functions(pe1_or),
      policy = policy_def(1),
      type = "or"
    ),
    NA
  )
  expect_equal(
    pe2_or$IC,
    NULL
  )
})

test_that("policy_eval handles varying stage action sets.", {
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(
    data = d,
    action = c("A_1", "A_2"),
    baseline = c("B", "BB"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  p <- policy_def(
    c("yes", "no")
  )
  expect_error(
    pe <- policy_eval(pd,
      policy = p,
      type = "dr",
      g_models = list(g_glm(), g_rf())
    ),
    NA
  )

  p <- policy_def(
    c("default", "default")
  )
  expect_error(
    policy_eval(pd,
      policy = p,
      type = "dr",
      g_models = list(g_glm(), g_rf())
    ),
    "The policy actions does not comply with the stage action sets of the policy data object."
  )
})

test_that("policy_eval with target = 'value' runs when cross-fitting.", {
  d <- sim_single_stage(1e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
  p <- policy_def(1, name = 1)

  set.seed(1)
  expect_no_error(
    pe_pooled <- policy_eval(
      policy_data = pd,
      policy = p,
      M = 2,
      variance_type = "pooled"
    )
  )

  expect_true(
    is.numeric(pe_pooled$value_estimate)
  )

  set.seed(1)
  expect_no_error(
    pe_stacked <- policy_eval(
      policy_data = pd,
      policy = p,
      M = 2,
      variance_type = "stacked"
    )
  )

  expect_equal(
    pe_pooled$value_estimate,
    pe_stacked$value_estimate
  )

  expect_true(
    (pe_pooled |> estimate() |> vcov())
    != (pe_stacked |> estimate() |> vcov())
  )

  set.seed(1)
  expect_no_error(
    pe_pooled <- policy_eval(
      policy_data = pd,
      policy = p,
      M = 2,
      variance_type = "complete"
    )
  )
})

test_that("policy_eval with target = 'value' agrees with targeted::lava", {
  d <- sim_single_stage(1e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
  p <- policy_def(1)

  ## set.seed(1)
  ## ca_dml1 <- targeted::cate(
  ##   treatment = A ~ 1,
  ##   response_model = U ~ A * Z,
  ##   propensity_model = A ~ Z,
  ##   type = "dml1",
  ##   data = d,
  ##   nfolds = 2,
  ##   contrast = 1
  ##   )

  ## set.seed(1)
  ## pe_dml1 <- policy_eval(
  ##   policy_data = pd,
  ##   policy = p,
  ##   M = 2,
  ##   variance_type = "stacked",
  ##   )

  set.seed(1)
  ca_dml2 <- targeted::cate(
    treatment = A ~ 1,
    response_model = U ~ A * Z,
    propensity_model = A ~ Z,
    type = "dml2",
    data = d,
    nfolds = 2,
    contrast = 1
  )

  set.seed(1)
  pe_dml2 <- policy_eval(
    policy_data = pd,
    policy = p,
    M = 2,
    variance_type = "pooled",
  )

  expect_equal(
    coef(ca_dml2$estimate)["(Intercept)"] |> unname(),
    pe_dml2$value_estimate
  )

  expect_equal(
    vcov(ca_dml2$estimate)["(Intercept)", "(Intercept)"] |> unname(),
    vcov(estimate(pe_dml2))[1, 1] |> unname()
  )
})

test_that("policy_eval with target 'value' has the correct outputs.", {
  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  d <- data.table(z = z, a = a, y = y, p = p)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p"),
    utility = c("y")
  )

  ## his <- get_history(pd, stage = 1)
  ## qfun <- fit_Q_function(history = his, Q = d$y, q_degen(var = "z"))
  ## predict.Q_function(qfun, new_history = his)

  p <- policy_def(function(p) p)

  ref_pe <- mean((d$a == d$p) / 0.5 * (d$y - d$z) + d$z)
  ref_IC <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z - ref_pe

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = q_degen(var = "z"),
    g_models = g_glm(~1)
  )

  expect_equal(
    coef(pe),
    ref_pe
  )

  expect_equal(
    pe$IC,
    ref_IC
  )

  ##
  ## cross-fitting: default
  ##

  ## in each training, the empirical propensity is no longer 0.5
  ## instead a g_model is fitted on the complete data:
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = q_degen(var = "z"),
    g_functions = gf,
    M = 2,
  )

  expect_equal(
    coef(pe),
    ref_pe
  )

  expect_equal(
    pe$IC,
    ref_IC
  )

  ##
  ## cross-fitting: stacked variance (IC)
  ##

  set.seed(1)
  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = q_degen(var = "z"),
    g_functions = gf,
    M = 2,
    variance_type = "stacked"
  )

  set.seed(1)
  n <- nrow(d)
  M <- 2
  folds <- split(sample(1:n, n), rep(1:M, length.out = n))
  folds <- lapply(folds, sort)

  ref_IC <- numeric(n)
  for (f in folds) {
    d_fold <- d[f, ]
    ref_pe_fold <- mean((d_fold$a == d_fold$p) / 0.5 * (d_fold$y - d_fold$z) +
      d_fold$z)
    ref_IC[f] <- (d_fold$a == d_fold$p) / 0.5 * (d_fold$y - d_fold$z) +
      d_fold$z - ref_pe_fold
  }

  expect_equal(
    coef(pe),
    ref_pe
  )

  expect_equal(
    pe$IC,
    ref_IC
  )

  ##
  ## cross-fitting: complete variance
  ##

  set.seed(1)
  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = q_degen(var = "z"),
    g_functions = gf,
    M = 2,
    variance_type = "complete"
  )

  ref_pe <- mean((d$a == d$p) / 0.5 * (d$y - d$z) + d$z)
  ref_IC <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z - ref_pe

  expect_equal(
    coef(pe),
    ref_pe
  )

  expect_equal(
    pe$IC,
    ref_IC
  )
})

test_that("policy_eval using the policy_learn has the correct output", {
  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p <- (z > 75) + 1
  d <- data.table(z = z, a = a, y = y, p = p)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p"),
    utility = c("y")
  )

  ref_pe <- mean((d$a == d$p) / 0.5 * (d$y - d$z) + d$z)
  ref_IC <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z - ref_pe

  pl <- policy_learn(
    type = "blip",
    threshold = 75,
    control = control_blip(blip_models = q_degen(var = "z"))
  )

  pe <- policy_eval(
    policy_data = pd,
    policy_learn = pl,
    q_models = q_degen(var = "z"),
    g_models = g_glm(~1)
  )

  expect_equal(
    ref_pe,
    coef(pe)
  )
  expect_equal(
    pe$IC,
    ref_IC
  )
})
