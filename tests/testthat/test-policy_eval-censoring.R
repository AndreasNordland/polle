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

test_that("policy_eval checks input under right-censoring", {

  ## right-censoring at every stage:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## g-functions:
  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))

  ## m-function:
  mf <- fit_m_function(policy_data = pd,
                       m_model = q_glm(~x))

  ## missing c_models and c_functions input:
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      m_model = q_degen(var = "x"),
      g_functions = gf
    ),
    "Right-censoring events \\(event = 2\\) occur in the policy data\\. Please provide either c_functions or c_models\\.$"
  )

  ## incorrect c_functions input:
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      m_model = q_degen(var = "x"),
      c_functions = gf,
      g_functions = gf
    ),
    "c_functions must be of class 'c_functions'\\."
  )

  ## incorrect c_models input:
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      m_model = q_degen(var = "x"),
      c_models = q_glm(~1),
      g_functions = gf
    ),
    "c_models must be a single c_model \\(or g_model\\) or a list of K\\+1 c_model's \\(or g_model's\\)\\."
  )

  ## missing m_model and m_function input:
  expect_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      c_functions = cf,
      g_functions = gf
    ),
    "Right-censoring events \\(event = 2\\) occur at stage K\\+1 in the policy data. Please provide either m_function or m_model\\."
  )

})

test_that("policy_eval with target 'value' has the expected output for the fixed two-stage case under right-censoring and single final utility outcome.", {

  ## right-censoring at every stage:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                  g_models = list(g_glm(~1), g_glm(~1)))
  tmp <- predict(gf, pd)

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]
  expect_equal(
    c(g1, g2, g3, g4),
    apply(tmp[, 3:6, with = FALSE], 2, function(x) max(unique(x))) |> unname()
  )
  rm(tmp)

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))
  tmp <- predict(cf, pd)
  c1 <- 1-ld[, list(mean(event == 2))][[1]]
  expect_equal(
    unique(tmp$surv_time),
    1
  )
  expect_equal(
    tmp$surv_time2 |> unique(),
    c1
  )

  Z <- d$x1 +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - d$x1) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3)

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ref_pe_or <- mean(d$x1)

  ref_pe_ipw <- mean((d$a1 == d$p1) / 0.5 * (d$m == 0) * (d$a2 == d$p2) / 0.5 *  d$y)

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

  ##
  ## cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_functions = gf,
    c_functions = cf,
    M = 2
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

  ## right-censoring NOT occuring at stage K+1:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rep(1, times = 1e2) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                  g_models = list(g_glm(~1), g_glm(~1)))
  tmp <- predict(gf, pd)

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]
  expect_equal(
    c(g1, g2, g3, g4),
    apply(tmp[, 3:6, with = FALSE], 2, function(x) max(unique(x))) |> unname()
  )
  rm(tmp)

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))
  tmp <- predict(cf, pd)
  c1 <- 1-ld[, list(mean(event == 2))][[1]]
  expect_equal(
    unique(tmp$surv_time),
    1
  )
  expect_equal(
    tmp$surv_time2 |> unique(),
    c1
  )

  Z <- d$x1 +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - d$x1) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$y - d$x2)

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ref_pe_or <- mean(d$x1)

  ref_pe_ipw <- mean((d$a1 == d$p1) / 0.5 * (d$m == 0) * (d$a2 == d$p2) / 0.5 *  d$y)

  ##
  ## no cross-fitting
  ##

  mf <- fit_m_function(policy_data = pd, m_model = q_glm(~x))
  expect_null(mf)

  ## m-model input:
  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"), # m-values should be ignored
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

  ## no m_model input:
  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

  ## right-censoring ONLY occuring at stage K+1:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rep(1, times = 1e2) # stage 1 non-missing indicator
  delta2 <- rep(1, times = 1e2) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                  g_models = list(g_glm(~1), g_glm(~1)))
  tmp <- predict(gf, pd)

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]
  expect_equal(
    c(g1, g2, g3, g4),
    apply(tmp[, 3:6, with = FALSE], 2, function(x) max(unique(x))) |> unname()
  )
  rm(tmp)

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))
  tmp <- predict(cf, pd)
  c1 <- 1-ld[, list(mean(event == 2))][[1]]
  expect_equal(
    unique(tmp$surv_time),
    1
  )
  expect_equal(
    tmp$surv_time2 |> unique(),
    c1
  )

  Z <- d$x1 +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - d$x1) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3)

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ref_pe_or <- mean(d$x1)

  ref_pe_ipw <- mean((d$a1 == d$p1) / 0.5 * (d$m == 0) * (d$a2 == d$p2) / 0.5 *  d$y)

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

})

test_that("policy_eval returns the expected c_functions output when using c-models.", {

  set.seed(1)
  ld <- sim_single_stage_right_cens(n = 5e2, type = "interval")
  pd <- policy_data(data = ld, type = "long", action = "A", time = "time", time2 = "time2")

  ## single c-model and m-model:

  pe <- policy_eval(
    policy_data = pd,
    policy = policy_def(1),
    m_model = q_glm(~.),
    m_full_history = FALSE,
    c_models = c_cox(formula = ~ Z)
  )

  library(mets)
  ref_model <- phreg(Surv(time = time, time2 = time2, event = (event == 2)) ~ Z, data = ld)

  expect_equal(
    coef(ref_model),
    coef(pe$c_functions$all_stages$c_model$model)
  )

})


test_that("policy_eval with target 'subgroup' has the correct output under right-censoring.", {

  ## right-censoring at every stage:
  set.seed(1)
  z <- 1:1e2*1.0
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  z2 <- runif(n = 1e2, min = -1, max = 1)
  d <- data.table(z = z, a = a, y = y, p = p, delta1 = delta1, delta2 = delta2, z2 = z2)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p"),
    utility = c("y")
  )

  ld <- pd$stage_data
  ld[stage == 2, z := z2]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 2, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  p <- policy_def(function(p) p, name = "p")

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  c1 <- 1-ld[, list(mean(event == 2))][[1]]

  ref_Z <- cbind(
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 1) / g1 * (d$z2 - d$z) +
    (d$delta1 == 1) / c1 * (d$delta2 == 1) / c1 * (d$a == 1) / g1 *(d$y - d$z2),
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 2) / g2 * (d$z2 - d$z) +
    (d$delta1 == 1) / c1 * (d$delta2 == 1) / c1 * (d$a == 2) / g2 *(d$y - d$z2)
  )
  ref_blip <- ref_Z[, 2] - ref_Z[, 1]
  ref_sub <- mean(ref_blip[d$p == 2])
  ref_sub_comp <- mean(ref_blip[d$p == 1])
  ref_IC <- 2 * (d$p == 2) * (ref_blip - ref_sub)
  ref_IC_comp <- 2 * (d$p == 1) * (ref_blip - ref_sub_comp)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                        g_models = list(g_glm(~1)))

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))

  ##
  ## no cross-fitting
  ##

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    m_model = polle:::q_degen(var = "z"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub, ref_sub_comp)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC, ref_IC_comp) |> unname()
  )

  expect_equal(
    names(sub$coef),
    c("E[U(2)-U(1)|d=2]: d=p", "E[U(2)-U(1)|d=1]: d=p")
  )

  ##
  ## cross-fitting
  ##

  ## in each training, the empirical propensity is no longer 0.5
  ## instead a g_model is fitted on the complete data:
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    m_model = polle:::q_degen(var = "z"),
    g_functions = gf,
    c_functions = cf,
    M = 2
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub, ref_sub_comp)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC, ref_IC_comp) |> unname()
  )

  ## right-censoring NOT occuring at stage 2:
  set.seed(1)
  z <- 1:1e2*1.0
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rep(1, times = 1e2) # stage 2 non-missing indicator
  z2 <- runif(n = 1e2, min = -1, max = 1)
  d <- data.table(z = z, a = a, y = y, p = p, delta1 = delta1, delta2 = delta2, z2 = z2)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p"),
    utility = c("y")
  )

  ld <- pd$stage_data
  ld[stage == 2, z := z2]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 2, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  p <- policy_def(function(p) p, name = "p")

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  c1 <- 1-ld[, list(mean(event == 2))][[1]]

  ref_Z <- cbind(
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 1) / g1 * (d$y - d$z),
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 2) / g2 * (d$y - d$z)
  )
  ref_blip <- ref_Z[, 2] - ref_Z[, 1]
  ref_sub <- mean(ref_blip[d$p == 2])
  ref_sub_comp <- mean(ref_blip[d$p == 1])
  ref_IC <- 2 * (d$p == 2) * (ref_blip - ref_sub)
  ref_IC_comp <- 2 * (d$p == 1) * (ref_blip - ref_sub_comp)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                        g_models = list(g_glm(~1)))

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))

  ##
  ## no cross-fitting
  ##

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    m_model = polle:::q_degen(var = "z"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub, ref_sub_comp)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC, ref_IC_comp) |> unname()
  )

  expect_equal(
    names(sub$coef),
    c("E[U(2)-U(1)|d=2]: d=p", "E[U(2)-U(1)|d=1]: d=p")
  )

  ## no m_model input:
  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub, ref_sub_comp)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC, ref_IC_comp) |> unname()
  )

  expect_equal(
    names(sub$coef),
    c("E[U(2)-U(1)|d=2]: d=p", "E[U(2)-U(1)|d=1]: d=p")
  )

  ## right-censoring NOT occuring at stage 1:
  set.seed(1)
  z <- 1:1e2*1.0
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  delta1 <- rep(1, times = 1e2)  # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  z2 <- runif(n = 1e2, min = -1, max = 1)
  d <- data.table(z = z, a = a, y = y, p = p, delta1 = delta1, delta2 = delta2, z2 = z2)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p"),
    utility = c("y")
  )

  ld <- pd$stage_data
  ld[stage == 2, z := z2]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 2, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  p <- policy_def(function(p) p, name = "p")

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  c1 <- 1-ld[, list(mean(event == 2))][[1]]

  ref_Z <- cbind(
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 1) / g1 * (d$z2 - d$z) +
    (d$delta1 == 1) / c1 * (d$delta2 == 1) / c1 * (d$a == 1) / g1 *(d$y - d$z2),
    d$z +
    (d$delta1 == 1) / c1 * (d$a == 2) / g2 * (d$z2 - d$z) +
    (d$delta1 == 1) / c1 * (d$delta2 == 1) / c1 * (d$a == 2) / g2 *(d$y - d$z2)
  )
  ref_blip <- ref_Z[, 2] - ref_Z[, 1]
  ref_sub <- mean(ref_blip[d$p == 2])
  ref_sub_comp <- mean(ref_blip[d$p == 1])
  ref_IC <- 2 * (d$p == 2) * (ref_blip - ref_sub)
  ref_IC_comp <- 2 * (d$p == 1) * (ref_blip - ref_sub_comp)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                        g_models = list(g_glm(~1)))

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))

  ##
  ## no cross-fitting
  ##

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    m_model = polle:::q_degen(var = "z"),
    g_functions = gf,
    c_functions = cf
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub, ref_sub_comp)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC, ref_IC_comp) |> unname()
  )

  expect_equal(
    names(sub$coef),
    c("E[U(2)-U(1)|d=2]: d=p", "E[U(2)-U(1)|d=1]: d=p")
  )

})

test_that("policy_eval() handles sparse censoring.", {

  ## right-censoring for a single subject at stage 2.
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 1) # stage 1 non-missing indicator
  delta2 <- c(0, rep(1, 99)) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 1) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## cross-fitting
  expect_no_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      g_functions = gf,
      c_models = g_empir(),
      M = 2
    )
  )

  expect_equal(
    pe$c_values$surv_time2 |> unique() |> sort(),
    c(1-1/149, 1)
  )

})

test_that("policy_eval() handles abundant censoring.", {

  ## right-censoring for full but 1 subject at stage 3.
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 1) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 1) # stage 2 non-missing indicator
  delta3 <- c(1, rep(0, 99))  # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## no cross-fitting
  expect_no_error(
    pe <- policy_eval(
      policy_data = pd,
      policy = p,
      q_models = list(q_degen(var = "x"), q_degen(var = "x")),
      g_functions = gf,
      c_models = g_empir(),
      m_model = polle:::q_degen(var = "x"),
      M = 1
    )
  )


  ## cross-fitting
  ## plan(sequential, split = TRUE) # browser() will work in future.apply
  expect_error(
    suppressWarnings(
      pe <- policy_eval(
        policy_data = pd,
        policy = p,
        q_models = list(q_degen(var = "x"), q_degen(var = "x")),
        g_functions = gf,
        c_models = g_empir(),
        m_model = polle:::q_degen(var = "x"),
        M = 2
      )
    ),
    "Unable to fit m_model: all utility outcomes are missing"
  )


   ## right-censoring for full but 1 subject at stage 2.
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 1) # stage 1 non-missing indicator
  delta2 <- c(1, rep(0, 99)) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 1)  # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)


  fpd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

})

test_that("policy_eval with target 'value' has the expected output
for the two-stage case under right-censoring and terminal events.", {

  ## right-censoring at every stage, but no death:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  u1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  u2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  u3 <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  p1 = p1,
                  p2 = p2,
                  u1 = u1,
                  u2 = u2,
                  u3 = u3,
                  delta1 = delta1,
                  delta2 = delta2,
                  delta3 = delta3)

  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("u1", "u2", "u3")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta:= delta3]
  ld[ , delta := cumprod(delta), by = id]
  ld[ , tmp := cumsum(delta == 0), by = id]
  ld <- ld[tmp %in% c(0,1)]
  ld[ , tmp := NULL]
  ld[delta == 0, event := 2]
  ld[ , delta := NULL]
  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                        g_models = list(g_glm(~1), g_glm(~1)))
  tmp <- predict(gf, pd)

  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]
  expect_equal(
    c(g1, g2, g3, g4),
    apply(tmp[, 3:6, with = FALSE], 2, function(x) max(unique(x))) |> unname()
  )
  rm(tmp)

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))
  tmp <- predict(cf, pd)
  c1 <- 1-ld[, list(mean(event == 2))][[1]]
  expect_equal(
    unique(tmp$surv_time),
    1
  )
  expect_equal(
    tmp$surv_time2 |> unique(),
    c1
  )

  Z <- (d$x1 + d$u1) +
    (d$delta1 == 1) / c1 *
    (d$a1 == d$p1) / (g1 * (d$a1 == "1") +
                      g2 *
                      (d$a1 == "2")) * ((d$x2 + d$u2) - d$x1) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") +
                                             g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 *
    (d$a2 == d$p2) / (g3 * (d$a2 == "3") +
                      g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") +
                                             g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") +
                                             g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$u3 - d$x3)

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_functions = gf,
    c_functions = cf
  )

  ## stage 1 Q-values
  expect_true(
    all(pe$q_values[stage == 1]$Q_1 == (d$x1 + d$u1)),
    all(pe$q_values[stage == 1]$Q_2 == (d$x1 + d$u1))
  )

  ## stage 2 Q-values
  expect_true(
    all(pe$q_values[stage == 2]$Q_3 == (d$x2 + d$u1 + d$u2)[d$delta1 == 1]),
    all(pe$q_values[stage == 2]$Q_4 == (d$x2 + d$u1 + d$u2)[d$delta1 == 1])
  )

  ## stage 3 Q-values
  expect_true(
    all(pe$m_values$Q == (d$x3 + d$u1 + d$u2)[d$delta1 == 1 & d$delta2 == 1])
  )

  expect_equal(
    pe$name,
    names(coef(pe))
  )
  expect_equal(
    pe$name,
    c("E[U(d)]: d=test")
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
    IC(pe),
    matrix(ref_IC)
  )

  rm(list = ls())
  ## right-censoring at every stage, and death at stage 2:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  u1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  u3 <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  death2 <- rbinom(n = 1e2, size = 1, prob = 0.2) # stage 2 death indicator
  u2 <- runif(n = 1e2, min = -1, max = 1) - death2 * runif(n = 1e2, min = 0, max = 1)
  d <- data.table(
    x1 = x1,
    a1 = a1,
    x2 = x2,
    a2 = a2,
    x3 = x3,
    p1 = p1,
    p2 = p2,
    u1 = u1,
    u2 = u2,
    u3 = u3,
    delta1 = delta1,
    delta2 = delta2,
    delta3 = delta3,
    death2 = death2
  )

  pd <- policy_data(
      data = d,
      action = c("a1", "a2"),
      covariates = list(
          x = c("x1", "x2"),
          p = c("p1", "p2")
      ),
      utility = c("u1", "u2", "u3")
  )

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta := delta3]

  ld[, death := 0]
  ld[stage == 2, death := death2]

  ld[, delta := cumprod(delta), by = id]
  ld[, nondeath := cumprod(!death), by = id]

  ld[, tmp := cumsum(delta == 0), by = id]
  ld[, tmp2 := cumsum(nondeath == 0), by = id]

  ld <- ld[tmp %in% c(0, 1)]
  ld <- ld[tmp2 %in% c(0, 1)]

  ld[, tmp := NULL]
  ld[, tmp2 := NULL]

  ld[delta == 0, event := 2]
  ld[death == 1, event := 1] # death before censoring

  ld[, delta := NULL]
  ld[, death := NULL]
  ld[, nondeath := NULL]

  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]
  ld[event == 1, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## g-values
  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]

  ## c-values:
  c1 <- 1-ld[, list(mean(event == 2))][[1]]

  Z <- (
    ## Q1
    (d$x1 + d$u1) +
    ## stage 1 dr score
    (d$death2 == 0) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) /
    (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * ((d$x2 + d$u2) - d$x1) +
    (d$death2 == 1) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) /
    (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$u2 - d$x1) +
    ## stage 2 dr score
    (d$death2 == 0) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) /
    (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 *
    (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    ## stage 3 dr score
    (d$death2 == 0) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) / (g1 * (d$a1 == "1") +
                                                               g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d$p2) / (g3 * (d$a2 == "3") +
                                             g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$u3 - d$x3)
  )

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_models =  list(g_glm(~1), g_glm(~1)),
    c_models = g_glm(~1)
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
      IC(pe),
      matrix(ref_IC)
  )

  rm(list =ls())
  ## right-censoring at every stage, and death at stage 1 and 2:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  u1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  u3 <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  delta1 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 1 non-missing indicator
  delta2 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 2 non-missing indicator
  delta3 <- rbinom(n = 1e2, size = 1, prob = 0.8) # stage 3 non-missing indicator
  death1 <- rbinom(n = 1e2, size = 1, prob = 0.1) # stage 1 death indicator
  death2 <- rbinom(n = 1e2, size = 1, prob = 0.2) # stage 2 death indicator
  u2 <- runif(n = 1e2, min = -1, max = 1) - death2 * runif(n = 1e2, min = 0, max = 1)
  d <- data.table(
    x1 = x1,
    a1 = a1,
    x2 = x2,
    a2 = a2,
    x3 = x3,
    p1 = p1,
    p2 = p2,
    u1 = u1,
    u2 = u2,
    u3 = u3,
    delta1 = delta1,
    delta2 = delta2,
    delta3 = delta3,
    death1 = death1,
    death2 = death2
  )

  pd <- policy_data(
      data = d,
      action = c("a1", "a2"),
      covariates = list(
          x = c("x1", "x2"),
          p = c("p1", "p2")
      ),
      utility = c("u1", "u2", "u3")
  )

  ## policy:
  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ld <- pd$stage_data
  ld[stage ==3, x := x3]

  ld[stage == 1, delta:= delta1]
  ld[stage == 2, delta:= delta2]
  ld[stage == 3, delta := delta3]

  ld[, death := 0]
  ld[stage == 1, death := death1]
  ld[stage == 2, death := death2]

  ld[, delta := cumprod(delta), by = id]
  ld[, nondeath := cumprod(!death), by = id]

  ld[, tmp := cumsum(delta == 0), by = id]
  ld[, tmp2 := cumsum(nondeath == 0), by = id]

  ld <- ld[tmp %in% c(0, 1)]
  ld <- ld[tmp2 %in% c(0, 1)]

  ld[, tmp := NULL]
  ld[, tmp2 := NULL]

  ld[delta == 0, event := 2]
  ld[death == 1, event := 1] # death before censoring

  ld[, delta := NULL]
  ld[, death := NULL]
  ld[, nondeath := NULL]

  ld[event == 2 & stage == 3, U := NA]
  ld[event == 2, A := NA]
  ld[event == 1, A := NA]

  pd <- policy_data(data = ld, type = "long")

  ## g-values
  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]

  ## c-values:
  c1 <- 1-ld[, list(mean(event == 2))][[1]]

  ## Q1
  Z <- (d$death1 == 0) * (d$x1 + d$u1)
  Z <- Z + (d$death1 == 1) * (d$u1)
  ## stage 1 dr score
  Z <- Z + (d$death1 == 0) * (d$death2 == 0) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) /
    (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * ((d$x2 + d$u2) - d$x1)
  Z <- Z + (d$death1 == 0) * (d$death2 == 1) * (d$delta1 == 1) / c1 * (d$a1 == d$p1) /
    (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$u2 - d$x1)
  ## stage 2 dr score
  Z <- Z + (d$death1 == 0) * (d$death2 == 0) * (d$delta1 == 1) / c1 *
    (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 *
    (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2)
    ## stage 3 dr score
  Z <- Z + (d$death1 == 0) * (d$death2 == 0) *
    (d$delta1 == 1) / c1 *
    (d$a1 == d$p1) / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 *
    (d$a2 == d$p2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$u3 - d$x3)

  ref_pe <- mean(Z)
  ref_IC <-  Z - ref_pe

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    m_model = q_degen(var = "x"),
    g_models =  list(g_glm(~1), g_glm(~1)),
    c_models = g_glm(~1)
  )

  expect_equal(
    coef(pe) |> unname(),
    ref_pe
  )

  expect_equal(
      IC(pe),
      matrix(ref_IC)
  )
})
