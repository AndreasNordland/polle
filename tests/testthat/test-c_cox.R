sim_single_stage_right_cens <- function(n = 5e2, zeta = c(0.7, 0.2), type = "right"){

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
  ld[delta == FALSE & stage == 2, U_A0 := NA]
  ld[delta == FALSE & stage == 2, U_A1 := NA]

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

  library("mets")

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  ## state history across all stages:
  his <- get_history(pd, event_set = c(0,1,2))
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld)
  ref_cox$call <- NULL

  fcf2 <- fit_c_functions(pd, c_models = c_cox(formula = ~ Z))$all_stages$c_model$model

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )
  expect_equal(
    coef(fcf$c_model$model),
    coef(fcf2)
  )

  surv <- predict.c_function(fcf, new_history = his)

  ref_surv_time <- as.vector(predict(ref_cox, newdata = ld, times = ld$time, individual.time = TRUE)$surv)
  ref_surv_time2 <- as.vector(predict(ref_cox, newdata = ld, times = ld$time2, individual.time = TRUE)$surv)

  expect_equal(
    surv$surv_time,
    ref_surv_time
  )
  expect_equal(
    surv$surv_time2,
    ref_surv_time2
  )

  ## state history for a given stage
  his <- get_history(pd, event_set = c(0,1,2), stage = 2)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 2,])
  ref_cox$call <- NULL

  fcf2 <- fit_c_functions(pd, c_models = list(c_cox(formula = ~ Z),c_cox(formula = ~ Z)))$stage_2$c_model$model

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )
  expect_equal(
    coef(fcf$c_model$model),
    coef(fcf2)
  )

  surv <- predict.c_function(fcf, new_history = his)

  ref_surv_time <- as.vector(predict(ref_cox, newdata = ld[stage == 2,], times = ld[stage == 2,]$time, individual.time = TRUE)$surv)
  ref_surv_time2 <- as.vector(predict(ref_cox, newdata = ld[stage == 2,], times = ld[stage == 2,]$time2, individual.time = TRUE)$surv)

  expect_equal(
    surv$surv_time,
    ref_surv_time
  )
  expect_equal(
    surv$surv_time2,
    ref_surv_time2
  )

  his <- get_history(pd, event_set = c(0,1,2), stage = 1)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 1,])
  ref_cox$call <- NULL

  fcf2 <- fit_c_functions(pd, c_models = list(c_cox(formula = ~ Z),c_cox(formula = ~ Z)))$stage_1$c_model$model

  expect_equal(
    coef(fcf$c_model$model),
    coef(ref_cox)
  )
  expect_equal(
    coef(fcf$c_model$model),
    coef(fcf2)
  )

  surv <- predict.c_function(fcf, new_history = his)

  ref_surv_time <- as.vector(predict(ref_cox, newdata = ld[stage == 1,], times = ld[stage == 1,]$time, individual.time = TRUE)$surv)
  ref_surv_time2 <- as.vector(predict(ref_cox, newdata = ld[stage == 1,], times = ld[stage == 1,]$time2, individual.time = TRUE)$surv)

  expect_equal(
    surv$surv_time,
    ref_surv_time
  )
  expect_equal(
    surv$surv_time2,
    ref_surv_time2
  )

  ## full history
  his <- get_history(pd, event_set = c(0,1,2), stage = 2, full_history = TRUE)
  fcf <- fit_c_function(
    history = his,
    c_model = c_cox(formula = ~ Z_1 * A_1)
  )
  ref_cox <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z*tmp, data = ld[, tmp := shift(A), by = id][stage == 2,])
  ref_cox$call <- NULL

  fcf2 <- fit_c_functions(pd, full_history = TRUE, c_models = list(c_cox(formula = ~ Z_1),c_cox(formula = ~ Z_1 * A_1)))$stage_2$c_model$model

  expect_equal(
    unname(coef(fcf$c_model$model)),
    unname(coef(ref_cox))
  )
  expect_equal(
    unname(coef(fcf$c_model$model)),
    unname(coef(fcf2))
  )

  surv <- predict.c_function(fcf, new_history = his)

  ref_surv_time <- as.vector(predict(ref_cox, newdata = ld[, tmp := shift(A), by = id][stage == 2,], times = ld[stage == 2,]$time, individual.time = TRUE)$surv)
  ref_surv_time2 <- as.vector(predict(ref_cox, newdata = ld[, tmp := shift(A), by = id][stage == 2,], times = ld[stage == 2,]$time2, individual.time = TRUE)$surv)

  expect_equal(
    surv$surv_time,
    ref_surv_time
  )
  expect_equal(
    surv$surv_time2,
    ref_surv_time2
  )

})
