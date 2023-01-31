test_that("the implementation of owl agrees with direct application of DTRlearn2::owl in the single stage case.",{
  library("DTRlearn2")

  d1 <- sim_single_stage(200, seed=1)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L"),
                     utility="U")

  H <- scale(d1[, c("B", "Z", "L")])
  AA <- 2*d1[, "A"]-1
  RR <- d1[, "U"]

  pi_model <- glm(A~B+Z+L, data = d1, family = binomial())
  pi <- predict(pi_model, type = "response")
  pi <- pi * d1$A + (1-pi) * (1-d1$A)

  set.seed(1)
  owl1 <- owl(H = H, AA = AA, RR = RR, n = nrow(d1), K = 1, pi = pi)
  owl1_dd <- predict(owl1, H = H, K = 1)$treatment[[1]]
  owl1_d <- unname(as.character(unlist((owl1_dd + 1)/2)))

  pl <- policy_learn(type = "owl", control = control_owl())

  set.seed(1)
  owl2 <- pl(policy_data = pd1, g_models = g_glm())
  owl2_d <- unname(unlist(get_policy(owl2)(pd1)[, "d"]))
  expect_equal(owl1_d,owl2_d)
})

test_that("the implementation of owl agrees with direct application of DTRlearn2::owl in the two stage case.",{
  d <- sim_two_stage(200, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  H1 <- d[, c("B", "L_1", "C_1")]
  H2 <- d[, c("B", "L_2", "C_2")]
  H <- list(H1, H2)
  H <- lapply(H, scale)
  AA <- list(2*d$A_1-1, 2*d$A_2-1)

  pi1_model <- glm(A_1~B+C_1+L_1, data = d, family = binomial())
  pi1 <- predict(pi1_model, type = "response")
  pi1 <- pi1 * d$A_1 + (1-pi1) * (1-d$A_1)
  pi2_model <- glm(A_2~B+C_2+L_2, data = d, family = binomial())
  pi2 <- predict(pi2_model, type = "response")
  pi2 <- pi2 * d$A_2 + (1-pi2) * (1-d$A_2)
  pi <- list(pi1, pi2)

  RR <- list(d$U_2, d$U_3)

  library("DTRlearn2")
  set.seed(1)
  owl1 <- owl(H = H, AA = AA, RR = RR, n = nrow(d), K = 2, pi = pi)
  owl_pred <- predict(owl1, H = H, K = 2)
  owl1_dd1 <- owl_pred$treatment[[1]]
  owl1_dd2 <- owl_pred$treatment[[2]]
  owl1_d1 <- unname(as.character(unlist((owl1_dd1 + 1)/2)))
  owl1_d2 <- unname(as.character(unlist((owl1_dd2 + 1)/2)))

  pl <- policy_learn(type = "owl",
                     control = control_owl(policy_vars = c("B", "L", "C"),
                                           reuse_scales = TRUE))
  set.seed(1)
  owl2 <- pl(policy_data = pd,
             g_models = list(g_glm(~B + L + C), g_glm(~B + L + C)))

  tmp <- get_policy(owl2)(pd)
  owl2_d1 <- unname(unlist(tmp[stage == 1, "d", with = FALSE]))
  owl2_d2 <- unname(unlist(tmp[stage == 2, "d", with = FALSE]))

  expect_equal(
    unname(owl2$owl_object$pi[[1]]),
    unname(pi[[1]]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(owl2$owl_object$pi[[2]]),
    unname(pi[[2]]),
    tolerance = 1e-10
  )
  expect_equal(owl1_d1,owl2_d1)
  expect_equal(owl1_d2,owl2_d2)

  # varying stage action sets:
  d2 <- copy(d)
  d2$A_1[d2$A_1 == 1] <- "t1"
  d2$A_1[d2$A_1 == 0] <- "t2"
  d2$A_2[d2$A_2 == 1] <- "t3"
  d2$A_2[d2$A_2 == 0] <- "t4"
  pd2 <- policy_data(d2,
                     action = c("A_1", "A_2"),
                     baseline = c("BB", "B"),
                     covariates = list(L = c("L_1", "L_2"),
                                       C = c("C_1", "C_2")),
                     utility = c("U_1", "U_2", "U_3"))

  set.seed(1)
  owl3 <- pl(policy_data = pd2,
             g_models = list(g_glm(~B + L + C), g_glm(~B + L + C)))
  tmp3 <- get_policy(owl3)(pd2)
  owl3_d1 <- unname(unlist(tmp3[stage == 1, "d", with = FALSE]))
  owl3_d2 <- unname(unlist(tmp3[stage == 2, "d", with = FALSE]))

  owl1_d1 <- unname(owl1_dd1[,1])
  owl1_d1[owl1_d1 == -1] <- "t2"
  owl1_d1[owl1_d1 == 1] <- "t1"
  owl1_d2 <- unname(owl1_dd2[,1])
  owl1_d2[owl1_d2 == -1] <- "t4"
  owl1_d2[owl1_d2 == 1] <- "t3"

  expect_equal(owl1_d1,owl3_d1)
  expect_equal(owl1_d2,owl3_d2)
})

test_that("policy_learn with type owl runs as intended", {
  d <- sim_two_stage(200, seed=1)
  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "ptl",
                     control = control_ptl(),
                     L = 2,
                     alpha = 0.3,
                     save_cross_fit_models = TRUE)

  po <- pl(pd,
           g_models = g_glm(),
           q_models = q_glm())

  expect_true(
    is.null(po$g_function)
  )
  expect_true(
    !is.null(po$g_functions_cf)
  )

  expect_true(
    is.data.table(get_policy(po)(pd))
  )
  expect_true(
    all(complete.cases(get_policy(po)(pd)))
  )


})

test_that("input to policy_learn with type owl handles incorrect input in a multi-stage setup.",{
  d <- sim_multi_stage(200, seed = 1)
  # constructing policy_data object:
  pd <- policy_data(data = d$stage_data,
                    baseline_data = d$baseline_data,
                    type = "long",
                    id = "id",
                    stage = "stage",
                    event = "event",
                    action = "A",
                    utility = "U")
  pd <- partial(pd, 3)

  pl <- policy_learn(type = "owl")

  expect_error(policy_eval(policy_data = pd,
                           policy_learn = pl), "owl is only implemented for a fixed number of stages.")
})

