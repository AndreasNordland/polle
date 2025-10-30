
test_that("policy_learn checks input", {

  expect_error(policy_learn(type = "test"),
               "Unknown type of policy learner. Use 'ql', 'drql', 'ptl', 'owl', 'earl' or 'rwl'.")

  expect_error(policy_learn(type = c("test", "test")),
               "type must be a character string.")


  d <- sim_single_stage(n=1e2)
  pd <- policy_data(d,
                    action="A",
                    covariates = list("Z", "B", "L"),
                    utility="U")

  # alpha
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = "test"))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = c(0,1)))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = NULL))
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = -0.1))
  )

  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = 0)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             alpha = 0.1)),
    NA
  )

  # full_history
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = TRUE)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = FALSE)),
    NA
  )
  expect_error(
    policy_eval(pd,
                policy_learn =  policy_learn(type = "ql",
                                             full_history = "test")),
    "full_history must be TRUE or FALSE"
  )

  # L: number of folds
  expect_error(
    policy_learn(type = "ql",
                 L = "test")
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 0)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = -1)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 1.1)
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 1),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 L = 2),
    NA
  )

  # cross_fit_g_models
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = TRUE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = FALSE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                                 cross_fit_g_models = "test"),
    "cross_fit_g_models must be TRUE or FALSE"
  )

  # cross_fit_g_models
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = TRUE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = FALSE),
    NA
  )
  expect_error(
    policy_learn(type = "ql",
                 save_cross_fit_models = "test"),
    "save_cross_fit_models must be TRUE or FALSE"
  )

  # future_args
  fa <- list(
    "test",
    NULL,
    1,
    TRUE
  )
  lapply(
    fa, function(fa){
      expect_error(
        policy_learn(type = "ql",
                     future_args =  fa)
      )
    })
  fa <- list(
    list(),
    list(future_seed = TRUE)
  )
  lapply(
    fa, function(fa){
      expect_error(
        policy_learn(type = "ql",
                     future_args =  fa),
        NA
      )
    })

  # name
  nn <- list(1, TRUE, "test", NULL)
  lapply(
    nn, function(name){
      expect_error(
        policy_learn(type = "ql",
                     name = name),
        NA
      )
    })

  nn <- list(c(1,1))
  lapply(
    nn, function(name){
      expect_error(
        policy_learn(type = "ql",
                     name = name)
      )
    })

})

test_that("policy_learn returns an error if type != 'blip' or type != 'ptl and threshold != 0.", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
  )

  expect_no_error(
    policy_learn(type = "ptl", control = control_ptl(), threshold = NULL)
  )

  expect_no_error(
    policy_learn(type = "ptl", control = control_ptl(), threshold = 1)
  )

  expect_no_error(
    policy_learn(type = "blip", control = control_blip(), threshold = 1)
  )

  expect_error(
    policy_learn(type = "drql", control = control_drql(), threshold = 1),
    ".only implemented for type 'blip' or 'ptl'"
  )
})

test_that("set_threshold", {
  expect_equal(
    set_threshold(selection = c(1, 2, 3)),
    c(1, 2, 3)
  )

  expect_equal(
    set_threshold(threshold = c(1, 3), selection = c(1, 2, 3)),
    c(1, 3)
  )

  expect_equal(
    set_threshold(threshold = c(3, 1), selection = c(1, 2, 3)),
    c(1, 3)
  )

  expect_equal(
    set_threshold(threshold = c(), selection = c(1, 2, 3)),
    c(1, 2, 3)
  )

  expect_error(
    set_threshold(threshold = c(1.5, 3), selection = c(1, 2, 3)),
    "Invalid threshold. Choose between 1, 2, 3."
  )

  expect_equal(
    set_threshold(threshold = c(3, 1.5), selection = c(1, 2, 3), overwrite = TRUE),
    c(1.5, 3)
  )
})

test_that("policy_learn() does not run with missing outcomes for drql, rql, ptl, earl, rwl, owl.", {

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

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "drql", control = control_drql()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'drql' not implemented under right-censoring/missing outcomes."
  )

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "rql", control = list()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'rql' not implemented under right-censoring/missing outcomes."
  )

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "ptl", control = control_ptl()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'ptl' not implemented under right-censoring/missing outcomes."
  )

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "earl", control = control_earl()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'earl' not implemented under right-censoring/missing outcomes."
  )

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "rwl", control = control_rwl()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'rwl' not implemented under right-censoring/missing outcomes."
  )

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(type = "owl", control = control_owl()),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning with type 'owl' not implemented under right-censoring/missing outcomes."
  )

})
