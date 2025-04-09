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
  ld[delta == FALSE, U := NA]
  ld[delta == FALSE, U_A0 := NA]
  ld[delta == FALSE, U_A1 := NA]

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


test_that("c_cox returns the expected output", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  ## state history across all stages:
  his <- get_history(pd, type = "event")
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld)
  ref_cox$call <- NULL

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )

  ## state history for a given stage
  his <- get_history(pd, type = "event", stage = 2)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 2,])
  ref_cox$call <- NULL

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )

  his <- get_history(pd, type = "event", stage = 1)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 1,])
  ref_cox$call <- NULL

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )

  ## full history
  his <- get_history(pd, type = "event", stage = 2, full_history = TRUE)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z_1 * A_1)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z*tmp, data = ld[, tmp := shift(A), by = id][stage == 2,])
  ref_cox$call <- NULL

  expect_equal(
    coef(fcf$c_model$model) |> unname(),
    coef(ref_cox) |> unname()
  )
})
