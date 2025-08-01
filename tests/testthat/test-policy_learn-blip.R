test_that("policy_learn with type blip works as intended", {

  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))


  # default settings:
  pl <- policy_learn(type = "blip",
                     control = control_blip())
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )

    inherits(po, "policy_object")
  })

  # realistic policy:
  pl <- policy_learn(type = "blip",
                     control = control_blip(),
                     alpha = 0.2)
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )

    inherits(po, "policy_object")
  })

  # predictions from the blip model:
  expect_true({
    blip <-predict(po$blip_functions, pd)
    !any(is.na(blip$blip))
  })
  expect_true({
    all(is.numeric(blip$blip))
  })

  # folds passed to the blip model:
  pl <- policy_learn(type = "blip",
                     L = 2,
                     control = control_blip(
                       blip_models = q_sl()
                     ))
  expect_true({
    po <- pl(
      policy_data = pd,
      g_models = g_glm(),
      q_models = q_glm()
    )
    t1 <- inherits(po, "policy_object")
    t2 <- po$blip_functions$stage_1$blip_model$model$cvControl$V == 2
    t3 <- all.equal(po$folds, po$blip_functions$stage_1$blip_model$model$cvControl$validRows)

    all(c(t1, t2, t3))
  })

})

test_that("get_policy_functions.blip returns a list of policy functions", {
  d <- sim_two_stage(200, seed = 1)
  d$A_2 <- paste(d$A_2, "test", sep = "")
  d$A_1 <- as.character(d$A_1)
  pd <- policy_data(d,
    action = c("A_1", "A_2"),
    baseline = c("BB", "B"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  pl <- policy_learn(
    type = "blip",
    control = control_blip()
  )
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  his <- get_history(pd, stage = 1)
  H <- get_H(his)

  expect_true(
    all(pf1(H) %in% get_stage_action_sets(pd)[[1]])
  )

  # realistic action set at level alpha:
  pl <- policy_learn(
    type = "blip",
    control = control_blip(),
    alpha = 0.2
  )
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_error(
    pf1 <- get_policy_functions(po, stage = 1),
    NA
  )
  his <- get_history(pd, stage = 1)
  H <- get_H(his)

  expect_true(
    all(pf1(H) %in% get_stage_action_sets(pd)[[1]])
  )

  expect_error(
    pf2 <- get_policy_functions(po, stage = 2),
    NA
  )
  his <- get_history(pd, stage = 2)
  H <- get_H(his)

  expect_true(
    all(pf2(H) %in% get_stage_action_sets(pd)[[2]])
  )

  expect_error(
    get_policy_functions(po),
    "stage argument is missing"
  )
})


test_that("get_policy.blip returns a policy", {

  d <- sim_two_stage(200, seed = 1)
  d$A_2 <- paste(d$A_2, "test", sep = "")
  d$A_1 <- as.character(d$A_1)
  pd <- policy_data(d,
    action = c("A_1", "A_2"),
    baseline = c("BB", "B"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = c("U_1", "U_2", "U_3")
  )

  pl <- policy_learn(
    type = "blip",
    control = control_blip()
  )
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_true({
    p <- get_policy(po)
    inherits(p, "policy")
  })

  expect_true(
    is.data.table(p(pd))
  )

  # realistic action set at level alpha:
  pl <- policy_learn(
    type = "blip",
    control = control_blip(),
    alpha = 0.2
  )
  po <- pl(
    policy_data = pd,
    g_models = list(g_glm(), g_glm()),
    q_models = q_glm()
  )

  expect_true({
    p <- get_policy(po)
    inherits(p, "policy")
  })

  pa <- p(pd)
  expect_true(is.data.table(pa))
  expect_true(all(pa$d %in% get_action_set(pd)))
  expect_true(all(key(pa) == c("id", "stage")))
  expect_true(inherits(pa$d, "character"))

  # comparing get_policy_function and get_policy:
  pf1 <- get_policy_functions(po, stage = 1)
  his1 <- get_history(pd, stage = 1)
  H1 <- get_H(his1)

  expect_equal(
    pf1(H1),
    pa[stage == 1, ]$d
  )

  pf2 <- get_policy_functions(po, stage = 2)
  his2 <- get_history(pd, stage = 2)
  H2 <- get_H(his2)

  expect_equal(
    pf2(H2),
    pa[stage == 2, ]$d
  )
})

test_that("policy_learn with type blip is persistent in the single stage case.", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
  )

  pl <- policy_learn(
    type = "blip",
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  ## his <- get_history(pd)
  ## pred <- predict.blip_function(po$blip_functions$stage_1, new_history = his)
  ## hist(x = pred$blip, breaks = 100)

  d <- get_policy(po)(pd)$d

  fix <- c(
    1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
    0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1,
    0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0,
    1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0
  )
  fix <- as.character(fix)

  expect_equal(
    d,
    fix
  )
})

test_that("policy_learn with type blip passes the threshold argument in the single-stage case,", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
  )

  pl <- policy_learn(
    type = "blip",
    threshold = c(1, 0.5),
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  expect_equal(
    po$threshold,
    c(0.5, 1)
  )

  pl <- policy_learn(
    type = "blip",
    threshold = c(3),
    control = control_blip()
  )
  po2 <- pl(pd, q_models = q_glm(), g_models = g_glm())

  ## the choosen treshold(s) should not influence the blip-function:
  expect_equal(
    po$blip_functions,
    po2$blip_functions
  )
})

test_that("get_policy.blip uses the threshold argument", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
  )

  pl <- policy_learn(
    type = "blip",
    threshold = 1,
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  his <- get_history(pd)
  pred <- predict.blip_function(po$blip_functions$stage_1, new_history = his)
  ## hist(x = pred$blip, breaks = 100)

  d <- get_policy(po)(pd)$d

  dd <- c("0", "1")[(pred$blip > 1) + 1]
  dd0 <- c("0", "1")[(pred$blip > 0) + 1]

  expect_equal(
    d,
    dd
  )

  d <- get_policy(po, threshold = 1)(pd)$d

  expect_equal(
    d,
    dd
  )

  d <- get_policy(po, threshold = 0)(pd)$d

  expect_true(
    any(!(d == dd))
  )

  d <- get_policy(po, threshold = c(0, 1))[[2]](pd)$d

  expect_equal(
    d,
    dd
  )

  d <- get_policy(po, threshold = c(0, 1))[[1]](pd)$d

  expect_equal(
    d,
    dd0
  )
})

test_that("policy_learn with type blip uses the threshold argument in the two-stage case,", {
  d <- sim_two_stage(n = 1e2, seed = 1)
  d$U <- d$U_1 + d$U_2 + d$U_3 - 2 * d$A_2
  pd <- policy_data(d,
    action = c("A_1", "A_2"),
    covariates = list(
      L = c("L_1", "L_2"),
      C = c("C_1", "C_2")
    ),
    utility = "U"
  )

  pl <- policy_learn(
    type = "blip",
    threshold = 1,
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  ## first stage:
  his <- get_history(pd, stage = 1)
  pred <- predict.blip_function(po$blip_functions$stage_1, new_history = his)
  ## hist(x = pred$blip, breaks = 100)
  dd <- c("0", "1")[(pred$blip > 1) + 1] # use the threshold in the first stage
  d <- get_policy(po)(pd)[stage == 1]$d

  expect_equal(
    d,
    dd
  )

  ## second stage:
  his <- get_history(pd, stage = 2)
  pred <- predict.blip_function(po$blip_functions$stage_2, new_history = his)
  ## hist(x = pred$blip, breaks = 100)
  dd <- c("0", "1")[(pred$blip > 0) + 1] # usual optimal policy in the second stage
  d <- get_policy(po)(pd)[stage == 2]$d

  expect_equal(
    d,
    dd
  )
})

test_that("get_policy and get_policy_functions agree with type blip and a non-zero threshold argument,", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
  )

  pl <- policy_learn(
    type = "blip",
    threshold = 1,
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())


  ## pred <- predict.blip_function(po$blip_functions$stage_1, new_history = his)
  ## hist(x = pred$blip, breaks = 100)

  d <- get_policy(po)(pd)$d
  pf <- get_policy_functions(po, stage = 1)
  his <- get_history(pd)
  H <- get_H(his)
  dd <- pf(H)

  expect_equal(
    d,
    dd
  )

  d <- get_policy(po, threshold = 0)(pd)$d
  pf <- get_policy_functions(po, stage = 1, threshold = 0)
  his <- get_history(pd)
  H <- get_H(his)
  dd <- pf(H)

  expect_equal(
    d,
    dd
  )

  expect_error(
    get_policy_functions(po, threshold = c(0, 1)),
    "Set single threshold"
  )

})

test_that("get_policy.blip() returns multiple policies when given multiple thresholds.", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
                    action = "A",
                    covariates = list("Z", "B", "L"),
                    utility = "U"
                    )

  pl <- policy_learn(
    type = "blip",
    threshold = c(0, 1, 2),
    control = control_blip()
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())

  pol_list <- get_policy(po)

  expect_equal(
    lapply(pol_list, function(x) attr(x, "name")) |> unlist(),
    paste0("blip(eta=", c(0,1,2), ")")
  )

  expect_true(
    is.list(pol_list),
    length(pol_list) == 3,
    all(unlist(lapply(pol_list, function(x) inherits(x, what = "policy"))))
  )

  expect_no_error(
    pol_list[[1]](pd)
  )
  expect_no_error(
    pol_list[[2]](pd)
  )
  expect_no_error(
    pol_list[[3]](pd)
  )


  his <- get_history(pd)
  pred <- predict.blip_function(po$blip_functions$stage_1, new_history = his)
  ## hist(x = pred$blip, breaks = 100)

  d <- pol_list[[2]](pd)$d
  dd <- c("0", "1")[(pred$blip > 1) + 1]

  expect_equal(
    d,
    dd
  )
})

test_that("policy_learn with type blip uses the quantile_threshold argument.", {

  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
    action = "A",
    covariates = list("Z", "B", "L"),
    utility = "U"
    )

  ## policy value

  pl <- policy_learn(
    type = "blip",
    threshold = c(1),
    control = control_blip(quantile_prob_threshold = c(0.25, 0.5))
  )
  po <- pl(pd, q_models = q_glm(), g_models = g_glm())
  pe <- policy_eval(
    policy_data = pd,
    policy_learn = pl
  )

  pl_ref <- policy_learn(
    type = "blip",
    threshold = pe$policy_object$threshold,
    control = control_blip()
  )
  pe_ref <- policy_eval(
    policy_data = pd,
    policy_learn = pl_ref
  )

  expect_equal(
    estimate(pe),
    estimate(pe_ref)
  )

  pl <- policy_learn(
    type = "blip",
    threshold = c(1),
    control = control_blip(quantile_prob_threshold = c(0.25, 1.1))
  )

  expect_error(
    policy_eval(policy_data = pd, policy_learn = pl),
    "quantile_prob_threshold must be numeric vector with values in"
  )

})

test_that("policy_learn with type 'blip' has the expected output for a fixed two-stage case with a single final utility outcome.", {

  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  x3 <- runif(n = 1e2, min = -1, max = 1)
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  x3 = x3,
                  y = y,
                  p1 = p1,
                  p2 = p2)


  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ##
  ## no cross-fitting
  ##

  ## policy_learn(type = "blip")):
  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~x))
  )

  expect_no_error(
    po <- pl(pd,
             q_models = list(q_glm(~x), q_degen(var = "x")),
             g_functions = gf)
  )

  ## stage 2

  Z2 <- cbind(
    d$x2 +
    (d$a2 == "3") / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2),
    d$x2 +
    (d$a2 == "4") / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2)
  )

  blip2 <- Z2[,2] - Z2[,1]

  blip_model2 <- glm(blip2 ~ d$x2)

  expect_equal(
    po$blip_functions$stage_2$blip_model$model |> coef() |> unname(),
    blip_model2 |> coef() |> unname()
  )

  d2 <- ifelse(predict(blip_model2, newdata = data.frame(x2 = d$x2)) > 0, "4", "3")

  expect_equal(
    unname(d2),
    get_policy(po)(pd)[stage == 2]$d
  )

  q1 <- predict(glm(d$x2 ~ d$x1), type = "response")

  ## stage 1

  Z1 <- cbind(
    q1 +
    (d$a1 == "1") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$a1 == "1") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) *
    (d$a2 == d2) / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2),
    q1 +
    (d$a1 == "2") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$a1 == "2") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) *
    (d$a2 == d2) / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2)
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

  rm(list = ls()[!(ls() %in% c("pd", "d", "gf"))])

  ##
  ## cross-fitting
  ##

  ## policy_learn(type = "blip")):
  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~x)),
    L = 2,
    cross_fit_g_models = FALSE
  )

  expect_no_error(
    po <- pl(pd,
             q_models = list(q_glm(~x), q_degen(var = "x")),
             g_functions = gf)
  )

  ## stage 2

  Z2 <- cbind(
    d$x2 +
    (d$a2 == "3") / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2),
    d$x2 +
    (d$a2 == "4") / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2)
  )

  blip2 <- Z2[,2] - Z2[,1]

  blip_model2 <- glm(blip2 ~ d$x2)

  expect_equal(
    po$blip_functions$stage_2$blip_model$model |> coef() |> unname(),
    blip_model2 |> coef() |> unname()
  )

  d2 <- ifelse(predict(blip_model2, newdata = data.frame(x2 = d$x2)) > 0, "4", "3")

  expect_equal(
    unname(d2),
    get_policy(po)(pd)[stage == 2]$d
  )

  q1 <- rep(as.numeric(NA), 1e2)
  q1[po$folds[[1]]] <- predict(glm(x2 ~ x1, data = d[-po$folds[[1]],]), type = "response", newdata = d[po$folds[[1]],])
  q1[po$folds[[2]]] <- predict(glm(x2 ~ x1, data = d[-po$folds[[2]],]), type = "response", newdata = d[po$folds[[2]],])

  ## stage 1

  Z1 <- cbind(
    q1 +
    (d$a1 == "1") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$a1 == "1") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) *
    (d$a2 == d2) / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2),
    q1 +
    (d$a1 == "2") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) * (d$x2 - q1) +
    (d$a1 == "2") / (0.5 * (d$a1 == "1") + 0.5 * (d$a1 == "2")) *
    (d$a2 == d2) / (0.5 * (d$a2 == "3") + 0.5 * (d$a2 == "4")) * (d$y - d$x2)
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

})


test_that("policy_learn with type 'blip' has the correct outputs for the stochastic two-stage case and a single final utility outcome.", {

  set.seed(1)
  x1 <- runif(n = 1e2, min = -1, max = 1)
  a1 <- c(rep(1, 50), rep(2, 50))
  x2 <- runif(n = 1e2, min = -1, max = 1)
  a2 <- c(rep(3, 50), rep(4, 50))
  y <- a1 * x1 + a2 * x2 + runif(n = 1e2, min = -1, max = 1)
  p1 <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
  p2 <- c(rep(3, 25), rep(4, 25), rep(3, 25), rep(4, 25))
  m <- rbinom(n = 1e2, size = 1, prob = 0.5) # stage 2 missing indicator
  d <- data.table(x1 = x1,
                  a1 = a1,
                  x2 = x2,
                  a2 = a2,
                  y = y,
                  p1 = p1,
                  p2 = p2,
                  m = m)
  rm(x1, a1, x2, a2, y)

  pd <- policy_data(
    data = d,
    action = c("a1", "a2"),
    covariates = list(x = c("x1", "x2"),
                      p = c("p1", "p2")),
    utility = c("y")
  )

  gf <- fit_g_functions(pd, g_models = list(g_glm(~1), g_glm(~1)))

  ld <- pd$stage_data
  ld[stage == 2, m := m]
  ld[, m := nafill(m, fill = 0)]
  ld <- ld[m == 0, ]
  ld[ , stage := 1:.N, by = id]
  setkey(ld, id, stage)
  ld[ , m := NULL]

  pd <- policy_data(data = ld, type = "long")

  ##
  ## no cross-fitting
  ##

  ## policy_learn(type = "blip")):
  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~x))
  )

  expect_no_error(
    po <- pl(pd,
             q_models = list(q_glm(~x), q_degen(var = "x")),
             g_functions = gf)
  )

  ## stage2

  p <- policy_def(function(p) p, name = "test", reuse = TRUE)

  ref_pe <- mean(d$x1 +
                 (d$a1 == d$p1) / 0.5 * (((d$m == 0) * d$x2 + (d$m == 1) * d$y) - d$x1) +
                 (d$m == 0) * (d$a1 == d$p1) / 0.5 * (d$a2 == d$p2) / 0.5 * (d$y - d$x2))

  ref_IC <-  (d$x1 +
              (d$a1 == d$p1) / 0.5 * (((d$m == 0) * d$x2 + (d$m == 1) * d$y) - d$x1) +
              (d$m == 0) * (d$a1 == d$p1) / 0.5 * (d$a2 == d$p2) / 0.5 * (d$y - d$x2) -
              ref_pe)

  ref_pe_or <- mean(d$x1)

  ref_pe_ipw <- mean((d$a1 == d$p1) / 0.5 * (d$m == 0) * (d$a2 == d$p2) / 0.5 *  d$y)

  ##
  ## no cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    g_functions = gf
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

  expect_equal(
    get_element(pe, "coef_or"),
    ref_pe_or
  )

  expect_equal(
    get_element(pe, "coef_ipw"),
    ref_pe_ipw
  )

  ##
  ## cross-fitting
  ##

  pe <- policy_eval(
    policy_data = pd,
    policy = p,
    q_models = list(q_degen(var = "x"), q_degen(var = "x")),
    g_functions = gf,
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

  expect_equal(
    get_element(pe, "coef_or"),
    ref_pe_or
  )

  expect_equal(
    get_element(pe, "coef_ipw"),
    ref_pe_ipw
  )


})
