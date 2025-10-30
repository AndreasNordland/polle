test_that("policy_learn with type 'blip' has the expected output for the fixed two-stage case under right-censoring and single final utility outcome.", {

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

  ## policy_learn(type = "blip")):
  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~x))
  )

  ## missing c-models and m-model:
  expect_error(
    po <- pl(pd, q_models = q_glm(), g_models = g_glm())
  )

  ## g-functions:
  gf <- fit_g_functions(policy_data = pd,
                        g_models = list(g_glm(~1), g_glm(~1)))
  g1 <- ld[stage == 1 & event == 0, mean(A == "1")]
  g2 <- ld[stage == 1 & event == 0, mean(A == "2")]
  g3 <- ld[stage == 2 & event == 0, mean(A == "3")]
  g4 <- ld[stage == 2 & event == 0, mean(A == "4")]

  ## c-functions:
  cf <- fit_c_functions(policy_data = pd,
                        c_models = g_glm(~1))
  c1 <- unique(predict(cf, new_policy_data = pd)$surv_time2)

  ##
  ## no cross-fitting
  ##

  expect_no_error(
    po <- pl(pd, q_models = list(q_glm(~x), q_degen(var = "x")), g_functions = gf, c_functions = cf, m_model = q_degen(var = "x"))
  )

  ## stage 2

  d2 <- d[delta1 == 1]

  Z2 <- cbind(
    d2$x2 +
    (d2$delta2 == 1) / c1 * (d2$a2 == "3") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$x3 - d2$x2) +
    (d2$delta2 == 1) / c1 * (d2$delta3 == 1) / c1 * (d2$a2 == "3") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$y - d2$x3),
     d2$x2 +
    (d2$delta2 == 1) / c1 * (d2$a2 == "4") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$x3 - d2$x2) +
    (d2$delta2 == 1) / c1 * (d2$delta3 == 1) / c1 * (d2$a2 == "4") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$y - d2$x3)
  )

  blip2 <- Z2[,2] - Z2[,1]

  blip_model2 <- glm(blip2 ~ d2$x2)

  expect_equal(
    po$blip_functions$stage_2$blip_model$model |> coef() |> unname(),
    blip_model2 |> coef() |> unname()
  )

  dd2 <- ifelse(predict(blip_model2, newdata = data.frame(x2 = d2$x2)) > 0, "4", "3")

  expect_equal(
    unname(dd2),
    get_policy(po)(pd)[stage == 2]$d
  )

  pf2 <- get_policy_functions(po, stage = 2)
  tmp <- pf2(get_history(pd, stage = 2, event_set = c(0,2))$H)

  expect_equal(tmp, unname(dd2))

  tmp <- glm(x2 ~ x1, data = d2)
  q1 <- predict(tmp, newdata = d, type = "response")
  rm(tmp)

  ## stage 1

  d2 <- d$delta1
  d2[d2 == 1] <- dd2

  Z1 <- cbind(
    q1 +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3),
    q1 +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3)
  )

  blip1 <- Z1[,2] - Z1[,1]

  blip_model1 <- glm(blip1 ~ d$x1)

  expect_equal(
    po$blip_functions$stage_1$blip_model$model |> coef() |> unname(),
    blip_model1 |> coef() |> unname()
  )

  d1 <- ifelse(predict(blip_model1, newdata = data.frame(x1 = d$x1)) > 0, "2", "1")

  expect_equal(
    unname(d1),
    get_policy(po)(pd)[stage == 1]$d
  )

  ##
  ## cross-fitting
  ##

  ## policy_learn(type = "blip")):
  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~x)),
    L = 2,
    cross_fit_g_models = FALSE,
    cross_fit_c_models = FALSE,
    save_cross_fit_models = TRUE
  )

  expect_no_error(
    po <- pl(pd,
             q_models = list(q_glm(~x), q_degen(var = "x")),
             g_functions = gf,
             c_functions = cf,
             m_model = q_degen(var = "x"))
  )

  ## stage 2

  d2 <- d[delta1 == 1]

  Z2 <- cbind(
    d2$x2 +
    (d2$delta2 == 1) / c1 * (d2$a2 == "3") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$x3 - d2$x2) +
    (d2$delta2 == 1) / c1 * (d2$delta3 == 1) / c1 * (d2$a2 == "3") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$y - d2$x3),
     d2$x2 +
    (d2$delta2 == 1) / c1 * (d2$a2 == "4") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$x3 - d2$x2) +
    (d2$delta2 == 1) / c1 * (d2$delta3 == 1) / c1 * (d2$a2 == "4") / (g3 * (d2$a2 == "3") + g4 * (d2$a2 == "4")) * (d2$y - d2$x3)
  )

  blip2 <- Z2[,2] - Z2[,1]

  blip_model2 <- glm(blip2 ~ d2$x2)

  expect_equal(
    po$blip_functions$stage_2$blip_model$model |> coef() |> unname(),
    blip_model2 |> coef() |> unname()
  )

  dd2 <- ifelse(predict(blip_model2, newdata = data.frame(x2 = d2$x2)) > 0, "4", "3")

  expect_equal(
    unname(dd2),
    get_policy(po)(pd)[stage == 2]$d
  )

  pf2 <- get_policy_functions(po, stage = 2)
  tmp <- pf2(get_history(pd, stage = 2, event_set = c(0,2))$H)

  expect_equal(tmp, unname(dd2))

  ## cross-fitted stage 1 Q-function
  q1 <- rep(as.numeric(NA), 1e2)
  q1[po$folds[[1]]] <- predict(glm(x2 ~ x1, data = d[!po$folds[[1]],][delta1 == 1]), type = "response", newdata = d[po$folds[[1]],])
  q1[po$folds[[2]]] <- predict(glm(x2 ~ x1, data = d[-po$folds[[2]],][delta1 == 1]), type = "response", newdata = d[po$folds[[2]],])

  ## stage 1

  d2 <- d$delta1
  d2[d2 == 1] <- dd2

  Z1 <- cbind(
    q1 +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == "1") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3),
    q1 +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) * (d$x3 - d$x2) +
    (d$delta1 == 1) / c1 * (d$a1 == "2") / (g1 * (d$a1 == "1") + g2 * (d$a1 == "2")) *
    (d$delta2 == 1) / c1 * (d$a2 == d2) / (g3 * (d$a2 == "3") + g4 * (d$a2 == "4")) *
    (d$delta3 == 1) / c1 * (d$y - d$x3)
  )

  blip1 <- Z1[,2] - Z1[,1]

  blip_model1 <- glm(blip1 ~ d$x1)

  expect_equal(
    po$blip_functions$stage_1$blip_model$model |> coef() |> unname(),
    blip_model1 |> coef() |> unname()
  )

  d1 <- ifelse(predict(blip_model1, newdata = data.frame(x1 = d$x1)) > 0, "2", "1")

  expect_equal(
    unname(d1),
    get_policy(po)(pd)[stage == 1]$d
  )

  d1 <- ifelse(predict(blip_model1, newdata = data.frame(x1 = d$x1)) > 0, "2", "1")

  expect_equal(
    unname(d1),
    get_policy(po)(pd)[stage == 1]$d
  )
})
