
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

## test_that("fit_c_function return the expected output", {

##   set.seed(1)
##   ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")

##   pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time_2 = "time_2")

##   his <- get_history(pd, type = "event")

##   fit_c_function(
##     history = his,
##     c_model = c_cox(formula = ~ Z, time = "time", time_2 = "time_2")
##   )

##   his <- get_history(pd, type = "event", stage = 2)

##   fit_c_function(
##     history = his,
##     c_model = c_cox(formula = ~ Z, time = "time", time_2 = "time_2")
##   )

## })
