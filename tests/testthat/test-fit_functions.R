test_that("fit_functions handle multiple thresholds", {
    d <- sim_single_stage(200, seed = 1)
    pd <- policy_data(d,
        action = "A",
        covariates = list("Z", "B", "L"),
        utility = "U"
    )

    pl <- policy_learn(
        type = "blip",
        threshold = c(0, 1),
        control = control_blip()
    )

    expect_no_error(
      ff <- fit_functions(
        policy_data = pd,
        type = "dr",
        policy_learn = pl,
        g_models = g_glm(),
        g_full_history = FALSE,
        q_models = q_glm(),
        q_full_history = FALSE,
        c_full_history = FALSE,
        m_full_history = FALSE
      )
    )

    expect_is(ff$policy_object, class = "policy_object")
})

test_that("crossfit_functions has the expected outcome", {

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

  set.seed(1)
  ld <- sim_two_stage_right_cens(n = 1e2)
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")
  folds <- sample_folds(2, sample_size = 1e2)

  ## fit_g_functions:
  expect_no_error(
    gf <- crossfit_function(folds = folds,
                            policy_data = pd,
                            fun = fit_g_functions,
                            models = g_glm(~C),
                            full_history = FALSE,
                            save_cross_fit_models = TRUE)
  )

  sgf <- lapply(folds, function(f) {
    spd <- subset_id(pd, id = get_id(pd)[-f])
    sgf <- fit_g_functions(spd, g_models = g_glm(~C))
    return(sgf)
  })

  for (i in seq_along(sgf)) {
    tmp <- sgf[[i]]
    tmp2 <- gf$functions[[i]]

    expect_equal(tmp, tmp2)

    pred1 <- predict.nuisance_functions(tmp, new_policy_data =  subset_id(pd, id = folds[[i]]))
    pred2 <- gf$values[id %in% folds[[i]]]

    expect_equal(pred1, pred2)
  }
  rm(sgf, tmp, tmp2, pred1, pred2)


  ## fit_c_functions
  expect_no_error(
    cf <- crossfit_function(folds = folds,
                            policy_data = pd,
                            fun = fit_c_functions,
                            models = list(g_glm(~L), g_glm(~L), g_glm(~L)),
                            full_history = FALSE,
                            save_cross_fit_models = TRUE)
  )

  scf <- lapply(folds, function(f) {
    spd <- subset_id(pd, id = get_id(pd)[-f])
    scf <- fit_c_functions(spd, c_models = list(g_glm(~L), g_glm(~L), g_glm(~L)))
    return(scf)
  })

  for (i in seq_along(scf)) {
    tmp <- scf[[i]]
    tmp2 <- cf$functions[[i]]

    expect_equal(tmp, tmp2)

    pred1 <- predict.c_functions(tmp, new_policy_data =  subset_id(pd, id = folds[[i]]))
    pred2 <- cf$values[id %in% folds[[i]]]

    expect_equal(pred1, pred2)
  }
  rm(scf, cf, tmp, tmp2, pred1, pred2)

  ## fit_m_function
  expect_no_error(
    mf <- crossfit_function(folds = folds,
                            policy_data = pd,
                            fun = fit_m_function,
                            models = q_glm(~L),
                            full_history = FALSE,
                            save_cross_fit_models = TRUE)
  )

  smf <- lapply(folds, function(f) {
    spd <- subset_id(pd, id = get_id(pd)[-f])
    smf <- fit_m_function(spd, m_model = q_glm(~L))
    return(smf)
  })

  for (i in seq_along(smf)) {
    tmp <- smf[[i]]
    tmp2 <- mf$functions[[i]]

    expect_equal(tmp, tmp2)

    pred1 <- predict.m_function(tmp, new_policy_data =  subset_id(pd, id = folds[[i]]))
    pred2 <- mf$values[id %in% folds[[i]]]

    expect_equal(pred1, pred2)
  }


})
