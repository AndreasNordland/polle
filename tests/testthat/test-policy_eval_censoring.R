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

sim_two_stage_right_cens <- function(n = 1e4,
                                     par = c(gamma = 0.5,  beta = 1, zeta = 1),
                                     seed = NULL,
                                     action_model_1 = function(C_1, beta, ...)
                                       stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1)),
                                     action_model_2 = function(C_2, beta, ...)
                                       stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_2)),
                                     deterministic_rewards = FALSE,
                                     cens_model = function(L, zeta, ...)
                                       stats::rbinom(n = NROW(L), size = 1, prob = lava::expit(zeta * abs(L)))) {

  d <- sim_two_stage(n = n)
  d$U <- d$U_1 + d$U_2 + d$U_3

  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = "U")

  ld <- pd$stage_data
  ld[is.na(L), L := d[["L_3"]]]

  ## simulating the discrete right-censoring:
  delta <- rbinom(n = nrow(ld), size = 1, prob = cens_model(ld$L, zeta = par["zeta"]))

  ## adapting the data to the right-censoring process:
  ld[ , delta := delta]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  return(ld)
}

test_that("policy_eval return the expected outcome for a two-stage discrete censoring model and a final utility outcome:", {

  set.seed(1)
  ld <- sim_two_stage_right_cens(n = 500)
  ld[ , absL := abs(L)]
  pd <- policy_data(data = ld, type = "long")

  pe <- policy_eval(
    policy_data = pd,
    policy = policy_def(1, reuse = TRUE),
    m_model = q_glm(~ absL),
    m_full_history = FALSE,
    c_models = g_glm(~ abs(L))
  )

})


test_that("policy_eval returns the expected output when using c-models.", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  ## single c-model and m-model:
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = policy_def(1),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2")
      ),
    NA
  )

  ref_model <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld)

  expect_equal(
    coef(ref_model),
    coef(fcf$all_stages$c_model$model)
  )

})

test_that("get_utility returns NA for right-censored observations", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  expect_equal(
    ld[ , list(na = any(event == 2)), by = id][["na"]],
    is.na(get_utility(pd)[["U"]])
  )

})

test_that("policy_learns does not run with missing outcomes.", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy_learn = policy_learn(),
      m_model = q_glm(~.),
      m_full_history = FALSE,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      ),
    "policy learning not implemented under right-censoring/missing outcomes."
  )

})
