
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

sim_two_stage_right_cens <- function(n = 1e4,
                                     par = c(gamma = 0.5,  beta = 1, zeta = 1),
                                     seed = NULL,
                                     action_model_1 = function(C_1, beta, ...)
                                       stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1)),
                                     action_model_2 = function(C_2, beta, ...)
                                       stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_2)),
                                     deterministic_rewards = FALSE,
                                     cens_model_1 = function(L, zeta, ...)
                                       stats::rbinom(n = NROW(L), size = 1, prob = lava::expit(zeta * abs(L))),
                                     cens_model_2 = function(L, zeta, ...)
                                       stats::rbinom(n = NROW(L), size = 1, prob = lava::expit(zeta * abs(L))),
                                     cens_model_3 = function(L, zeta, ...)
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
  delta_1 <- rbinom(n = nrow(ld[stage == 1]), size = 1, prob = cens_model_1(ld[stage == 1]$L, zeta = par["zeta"]))
  delta_2 <- rbinom(n = nrow(ld[stage == 2]), size = 1, prob = cens_model_2(ld[stage == 2]$L, zeta = par["zeta"]))
  delta_3 <- rbinom(n = nrow(ld[stage == 3]), size = 1, prob = cens_model_3(ld[stage == 3]$L, zeta = par["zeta"]))

  ## adapting the data to the right-censoring process:
  ld[stage == 1 , delta := delta_1]
  ld[stage == 2 , delta := delta_2]
  ld[stage == 3 , delta := delta_3]
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

test_that("fit_c_functions return the expected output", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  ## single c-model:

  expect_error(
    fcf <- fit_c_functions(
      policy_data = pd,
      c_models = c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
      full_history = FALSE
    ),
    NA
  )

  ref_model <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld)

  expect_equal(
    coef(ref_model),
    coef(fcf$all_stages$c_model$model)
  )

  ## c-model for each stage, state history:
  expect_error(
    fcf <- fit_c_functions(
      policy_data = pd,
      c_models = list(
        c_cox(formula = ~ Z, time = "time", time_2 = "time_2"),
        c_cox(formula = ~ Z, time = "time", time_2 = "time_2")
      ),
      full_history = FALSE
    ),
    NA
  )

  ref_model1 <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 1, ])
  ref_model2 <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 2, ])

  expect_equal(
    coef(ref_model1),
    coef(fcf$stage_1$c_model$model)
  )

  expect_equal(
    coef(ref_model2),
    coef(fcf$stage_2$c_model$model)
  )

  ## c-model for each stage, full history:
  expect_error(
    fcf <- fit_c_functions(
      policy_data = pd,
      c_models = list(
        c_cox(formula = ~ Z_1, time = "time", time_2 = "time_2"),
        c_cox(formula = ~ Z_2, time = "time", time_2 = "time_2")
      ),
      full_history = TRUE
    ),
    NA
  )

  ref_model1 <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 1, ])
  ref_model2 <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld[stage == 2, ])

  expect_equal(
    unname(coef(ref_model1)),
    unname(coef(fcf$stage_1$c_model$model))
  )

  expect_equal(
    unname(coef(ref_model2)),
    unname(coef(fcf$stage_2$c_model$model))
  )


})

test_that("fit_c_functions(): g_model's passed as the argument c_models are fitted and predicted correctly for discrete right-censoring:", {

  set.seed(1)
  ld <- sim_two_stage_right_cens(n = 500)
  ld[ , absL := abs(L)]
  pd <- policy_data(data = ld, type = "long")

  ## single c_model:
  ref_model <- glm(I(event != 2) ~ absL, data = ld, family = binomial())
  tmp <- fit_c_functions(policy_data = pd,
                         c_models = g_glm(~ absL),
                         full_history = FALSE)
  expect_equal(
    coef(ref_model),
    coef(tmp$all_stages$c_model$model)
  )

  ref_predict <- predict(ref_model, newdata = ld, type = "response")
  ref_predict <- cbind(1, ref_predict)
  colnames(ref_predict) <- c("surv_time", "surv_time2")
  tmp <- predict(tmp, new_policy_data = pd)[ , 3:4] |> as.matrix()

  expect_equal(
    ref_predict,
    tmp,
    check.attributes = FALSE
  )

  ## multiple g_model's:
  ref_model1 <- glm(I(event != 2) ~ absL, data = ld[stage == 1], family = binomial())
  ref_model2 <- glm(I(event != 2) ~ absL, data = ld[stage == 2], family = binomial())
  ref_model3 <- glm(I(event != 2) ~ absL, data = ld[stage == 3], family = binomial())

  tmp <- fit_c_functions(policy_data = pd,
                         c_models = list(g_glm(~ absL), g_glm(~ absL), g_glm(~ absL)),
                         full_history = FALSE)

  ref_model <- list(ref_model1, ref_model2, ref_model3)

  for (i in seq_along(ref_model)) {
    expect_equal(
      coef(ref_model[[i]]),
      coef(tmp[[i]]$c_model$model)
    )
  }

  for (i in seq_along(ref_model)) {
    ref_predict <- predict(ref_model[[i]], newdata = ld[stage == i], type = "response")
    ref_predict <- cbind(1, ref_predict)
    colnames(ref_predict) <- c("surv_time", "surv_time2")
    pred <- predict(tmp, new_policy_data = pd)[ stage == i, 3:4] |> as.matrix()
    expect_equal(
      ref_predict,
      pred,
      check.attributes = FALSE
    )
  }

})

test_that("fit_c_functions() related errors to fitting the c-model.", {

  set.seed(1)
  ld <- sim_two_stage_right_cens(n = 500)
  ld[ , absL := abs(L)]
  ld$absL[1:10] <- as.numeric(NA)
  pd <- policy_data(data = ld, type = "long")

  expect_error(
    fit_c_functions(policy_data = pd,
                    c_models = list(g_glm(~ absL), g_glm(~ absL), g_glm(~ absL)),
                    full_history = FALSE),
    "Error fitting c_model: NA/NaN/Inf in 'x' when calling 'g_glm' with formula:\n~absL"
  )

})

test_that("fit_c_functions(): returns default right-censoring model for stages where no right-censoring occur,", {

  set.seed(1)
  ld <- sim_two_stage_right_cens(n = 500, cens_model_2 = function(L, ...) rep(1, length(L))) # no right-censoring at stage 2
  ld[ , absL := abs(L)]
  pd <- policy_data(data = ld, type = "long")

  expect_no_error(
    tmp <- fit_c_functions(policy_data = pd,
                           c_models = list(g_glm(~ absL), g_glm(~ absL), g_glm(~ absL)),
                           full_history = FALSE)
  )

  pred <- predict(tmp, new_policy_data = pd)

  ref_model1 <- glm(I(event != 2) ~ absL, data = ld[stage == 1], family = binomial())
  ref_model3 <- glm(I(event != 2) ~ absL, data = ld[stage == 3], family = binomial())

  ref_predict1 <- predict(ref_model1, newdata = ld[stage == 1], type = "response")
  ref_predict3 <- predict(ref_model3, newdata = ld[stage == 3], type = "response")

  ref <- cbind(get_id_stage(pd, event_set = c(0,1,2)))
  ref[, surv_time := 1]
  ref[stage == 1, surv_time2 := ref_predict1]
  ref[stage == 2, surv_time2 := 1]
  ref[stage == 3, surv_time2 := ref_predict3]
  setkey(ref, id, stage)
  attr(ref, "index") <- NULL

  expect_equal(
    pred,
    ref
  )

})
