test_that("fit_m_function has the expected output.", {

  ## right-censoring occur at stage 3:
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

  m <- fit_m_function(policy_data = pd, m_model = q_glm(~ x), full_history = FALSE)
  m_ref <- glm(y ~ x3, data = d[delta1 == 1 & delta2 == 1 &  delta3 == 1 ])

  expect_equal(
    unname(coef(m$m_model$model)),
    unname(coef(m_ref))
  )

  m <- fit_m_function(policy_data = pd, m_model = q_glm(~ x_1 + A_1 + x_2 + x_3), full_history = TRUE)
  m_ref <- glm(y ~ x1 + as.factor(a1) + x2 + x3, data = d[delta1 == 1 & delta2 == 1 &  delta3 == 1 ])

  expect_equal(
    unname(coef(m$m_model$model)),
    unname(coef(m_ref))
  )

  ## right-censoring does NOT occur at stage 3:
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

  m <- fit_m_function(policy_data = pd, m_model = q_glm(~ x), full_history = FALSE)

  expect_equal(
    m,
    NULL
  )

  m <- fit_m_function(policy_data = pd, m_model = q_glm(~ x), full_history = TRUE)

  expect_equal(
    m,
    NULL
  )

})

test_that("fit_m_function handles missing covariates (throws and error message that is related to the m-model)", {

  ## right-censoring occur at stage 3:
  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  x3[1:10] <- as.numeric(NA)
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

  expect_error(
    m <- fit_m_function(policy_data = pd, m_model = q_glm(~ x), full_history = FALSE),
    "Error fitting m_model: NA/NaN/Inf in 'x' when calling 'q_glm' with formula:\nV_res ~ x"
  )


})
