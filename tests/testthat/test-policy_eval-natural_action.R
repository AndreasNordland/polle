## simulates a two-stage problem with (optional) right-censoring before the
## second action. The outcome is observed at the final stage (K+1).
sim_natural_action <- function(n = 400, cens = FALSE, seed = 1) {
  set.seed(seed)
  L1 <- rnorm(n)
  A1 <- rbinom(n, 1, plogis(0.5 * L1))
  S2 <- rbinom(n, 1, plogis(-0.5 + 0.5 * A1 + 0.5 * L1))
  L2 <- 0.5 * L1 + 0.3 * A1 + rnorm(n)
  A2 <- rbinom(n, 1, plogis(0.5 * L2 + 0.5 * A1 - 0.3))
  Y <- 1 + L2 + 0.5 * A1 + A2 * (1 - 2 * S2) + 0.5 * S2 * L1 + rnorm(n)
  if (cens) {
    Tc <- rexp(n, rate = exp(-1.5 + 0.8 * L2 + 0.8 * S2))
  } else {
    Tc <- rep(Inf, n)
  }
  cs <- Tc < 1
  id <- seq_len(n)
  s1 <- data.table(
    id = id, stage = 1, event = 0, time = 0, time2 = 1,
    A = as.character(A1), L = L1, S = 0, U = 0
  )
  s2 <- data.table(
    id = id, stage = 2, event = ifelse(cs, 2, 0), time = 1,
    time2 = ifelse(cs, 1 + Tc, 2),
    A = ifelse(cs, NA_character_, as.character(A2)), L = L2, S = S2, U = 0
  )
  s3 <- data.table(
    id = id[!cs], stage = 3, event = 1, time = 2, time2 = 2,
    A = NA_character_, L = NA_real_, S = NA_real_, U = Y[!cs]
  )
  sd <- rbind(s1, s2, s3)
  setorder(sd, id, stage)
  policy_data(
    sd, type = "long", id = "id", stage = "stage", event = "event",
    action = "A", utility = "U", time = "time", time2 = "time2"
  )
}

g_nat <- list(g_glm(~ L_1), g_glm(~ L_2 + A_1))
q_nat <- list(q_glm(~ A * L_1), q_glm(~ L_2 + A_1 + A * S_2 + S_2:L_1))
c_nat <- list(c_no_censoring(), c_cox(~ L_2 + S_2), c_no_censoring())

## treat, unless S = 1 (e.g., cancer) in which case the natural action is followed:
treat_unless_s <- policy_def(
  list(1, function(A, S) ifelse(S == 1, A, "1")),
  natural_action = TRUE,
  name = "treat_unless_S"
)

test_that("policy_def with natural_action returns NA policy actions for right-censored events", {
  pd <- sim_natural_action(200, cens = TRUE)

  pa <- treat_unless_s(pd)
  stage_data <- get_stage_data(pd)
  cens <- stage_data[event == 2, c("id", "stage")]
  expect_true(all(is.na(merge(cens, pa)$d)))
  expect_false(anyNA(merge(stage_data[event == 0, c("id", "stage")], pa)$d))

  ## treats unless S = 1, in which case the natural action is followed:
  obs <- merge(get_actions(pd), pa)
  S2 <- stage_data[stage == 2 & event == 0, ]$S
  expect_equal(obs[stage == 2]$d, ifelse(S2 == 1, obs[stage == 2]$A, "1"))
  expect_equal(obs[stage == 1]$d, rep("1", get_n(pd)))
})

test_that("natural_policy_actions evaluates the policy at every hypothetical natural action", {
  pd <- sim_natural_action(200, cens = TRUE)
  pan <- natural_policy_actions(treat_unless_s, pd)

  expect_equal(key(pan), c("id", "stage"))
  expect_equal(names(pan), c("id", "stage", "d_0", "d_1"))
  ## defined for right-censored events:
  expect_equal(nrow(pan), nrow(treat_unless_s(pd)))
  expect_false(anyNA(pan))
  S <- get_history(pd, stage = 2, event_set = c(0, 2))$H$S
  expect_equal(pan[stage == 2]$d_0, ifelse(S == 1, "0", "1"))
  expect_equal(pan[stage == 2]$d_1, rep("1", length(S)))
  expect_equal(pan[stage == 1]$d_0, rep("1", get_n(pd)))
  expect_equal(pan[stage == 1]$d_1, rep("1", get_n(pd)))

  expect_error(
    natural_policy_actions(policy_def(1), pd),
    "does not depend on the natural action"
  )
})

test_that("a natural-value policy that follows the natural action estimates the mean outcome", {
  pd <- sim_natural_action(400)
  p <- policy_def(function(A) A, reuse = TRUE, natural_action = TRUE)

  pe <- policy_eval(pd, policy = p, g_models = g_nat, q_models = q_nat,
                    g_full_history = TRUE, q_full_history = TRUE)
  U <- get_utility(pd)$U
  expect_equal(unname(coef(pe)), mean(U))
  expect_equal(pe$coef_ipw, mean(U))
  expect_equal(as.numeric(pe$IC), U - mean(U))
})

test_that("a natural-value policy that ignores the natural action equals the corresponding policy", {
  for (cens in c(FALSE, TRUE)) {
    pd <- sim_natural_action(400, cens = cens)
    args <- list(
      policy_data = pd,
      g_models = g_nat, q_models = q_nat,
      g_full_history = TRUE, q_full_history = TRUE
    )
    if (cens) {
      args <- c(args, list(c_models = c_nat, c_full_history = TRUE))
    }
    p_static <- policy_def(function(L) ifelse(L > 0, "1", "0"), reuse = TRUE)
    p_nat <- policy_def(function(A, L) ifelse(L > 0, "1", "0"),
                        reuse = TRUE, natural_action = TRUE)
    pe_static <- do.call(policy_eval, c(args, list(policy = p_static)))
    pe_nat <- do.call(policy_eval, c(args, list(policy = p_nat)))

    expect_equal(unname(coef(pe_nat)), unname(coef(pe_static)))
    expect_equal(pe_nat$IC, pe_static$IC)
    expect_equal(pe_nat$coef_ipw, pe_static$coef_ipw)
    expect_equal(pe_nat$coef_or, pe_static$coef_or)
    expect_equal(pe_nat$q_functions[[1]]$q_model$model$coefficients,
                 pe_static$q_functions[[1]]$q_model$model$coefficients)
  }
})

test_that("single-stage natural-value policy equals the hand-calculated score", {
  set.seed(1)
  n <- 500
  d <- sim_single_stage(n, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z", "L"), utility = "U")
  ## treat, unless Z > 0 in which case the natural action is followed:
  p <- policy_def(function(A, Z) ifelse(Z > 0, A, "1"), natural_action = TRUE)

  pe <- policy_eval(pd, policy = p, g_models = g_glm(~ Z + L), q_models = q_glm(~ A * (Z + L)))

  g <- as.matrix(pe$g_values[, c("g_0", "g_1")])
  q <- as.matrix(pe$q_values[, c("Q_0", "Q_1")])
  A <- as.numeric(d$A)
  Z <- d$Z
  ## probability of the policy action
  gd <- cbind(
    ifelse(Z > 0, g[, 1], 0),
    ifelse(Z > 0, g[, 2], 1)
  )
  gA <- g[cbind(seq_len(n), A + 1)]
  gdA <- gd[cbind(seq_len(n), A + 1)]
  dA <- ifelse(Z > 0, A, 1)
  Q_d <- q[cbind(seq_len(n), dA + 1)]
  Q_A <- q[cbind(seq_len(n), A + 1)]
  Zd <- Q_d + gdA / gA * (d$U - Q_A)

  expect_equal(unname(coef(pe)), mean(Zd))
  expect_equal(as.numeric(pe$IC), Zd - mean(Zd))
  expect_equal(pe$coef_ipw, mean(gdA / gA * d$U))
  expect_equal(pe$coef_or, mean(Q_d))
})

test_that("policy_eval with a natural-value policy handles right-censoring before the action", {
  pd <- sim_natural_action(500, cens = TRUE)
  expect_true(any(get_stage_data(pd)$event == 2))

  pe <- policy_eval(
    pd, policy = treat_unless_s,
    g_models = g_nat, q_models = q_nat, c_models = c_nat,
    g_full_history = TRUE, q_full_history = TRUE, c_full_history = TRUE
  )
  expect_true(is.finite(coef(pe)))
  expect_true(all(is.finite(pe$IC)))
  expect_equal(nrow(pe$g_values[stage == 2]), nrow(get_stage_data(pd)[stage == 2 & event %in% c(0, 2)]))

  ## cross-fitting:
  set.seed(1)
  pe_cf <- policy_eval(
    pd, policy = treat_unless_s,
    g_models = g_nat, q_models = q_nat, c_models = c_nat,
    g_full_history = TRUE, q_full_history = TRUE, c_full_history = TRUE,
    M = 2
  )
  expect_true(is.finite(coef(pe_cf)))
})

test_that("policy_eval with a natural-value policy requires g-models under right-censoring", {
  pd <- sim_natural_action(200, cens = TRUE)
  expect_error(
    policy_eval(
      pd, policy = treat_unless_s,
      g_models = NULL, q_models = q_nat, c_models = c_nat,
      q_full_history = TRUE, c_full_history = TRUE
    ),
    "g_models"
  )
})

test_that("natural-value policies are only implemented for target = 'value'", {
  d <- sim_single_stage(100, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z", "L"), utility = "U")
  p <- policy_def(function(A, Z) ifelse(Z > 0, A, "1"), natural_action = TRUE)
  expect_error(
    policy_eval(pd, policy = p, g_models = g_glm(), q_models = q_glm(),
                target = "subgroup"),
    "only implemented for target = 'value'"
  )
})

test_that("policy functions must return an action for observed natural actions", {
  d <- sim_single_stage(100, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z", "L"), utility = "U")
  p <- policy_def(function(A, Z) ifelse(Z > 0, NA, A), natural_action = TRUE)
  expect_error(
    policy_eval(pd, policy = p, g_models = g_glm(), q_models = q_glm()),
    "missing actions for observed natural actions"
  )
})

## four-stage design where treatment can be started at every stage. The outcome
## is observed at the final stage (K+1 = 5).
sim_grace_period <- function(n = 400, seed = 1, intervene = FALSE, cens = FALSE) {
  set.seed(seed)
  L1 <- rnorm(n)
  A1 <- rbinom(n, 1, plogis(-0.5 + 0.5 * L1))
  L2 <- 0.5 * L1 + 0.3 * A1 + rnorm(n)
  A2 <- rbinom(n, 1, plogis(-0.5 + 0.5 * L2 + 1.5 * A1))
  L3 <- 0.5 * L2 + 0.3 * A2 + rnorm(n)
  A3 <- rbinom(n, 1, plogis(-0.5 + 0.5 * L3 + 1.5 * A2))
  never <- (A1 + A2 == 0)
  if (intervene) A3 <- ifelse(never, 1, A3)
  L4 <- 0.5 * L3 + 0.3 * A3 + rnorm(n)
  A4 <- rbinom(n, 1, plogis(-0.5 + 0.5 * L4 + 1.5 * A3))
  if (intervene) A4 <- ifelse(never, 1, A4)
  Y <- 1 + L4 + 0.5 * (A1 + A2 + A3 + A4) + 0.3 * A4 * L4 + rnorm(n)
  Tc <- if (cens) rexp(n, rate = exp(-1.5 + 0.8 * L4 + 0.5 * A3)) else rep(Inf, n)
  cs <- Tc < 1
  id <- seq_len(n)
  stage_row <- function(k, A, L, event = 0, time2 = k) {
    data.table(id = id, stage = k, event = event, time = k - 1, time2 = time2,
               A = as.character(A), L = L, U = 0)
  }
  s4 <- stage_row(4, ifelse(cs, NA, A4), L4, event = ifelse(cs, 2, 0),
                  time2 = ifelse(cs, 3 + Tc, 4))
  s5 <- data.table(id = id[!cs], stage = 5, event = 1, time = 4, time2 = 4,
                   A = NA_character_, L = NA_real_, U = Y[!cs])
  sd <- rbind(stage_row(1, A1, L1), stage_row(2, A2, L2), stage_row(3, A3, L3), s4, s5)
  setorder(sd, id, stage)
  pd <- policy_data(
    sd, type = "long", id = "id", stage = "stage", event = "event",
    action = "A", utility = "U", time = "time", time2 = "time2"
  )
  list(pd = pd, Y = Y)
}

## start treatment at stage 3 and treat through the rest of follow-up, unless
## treatment was started before stage 3, in which case the natural action is
## followed:
follow_natural <- function(A) A
start_at_stage_3 <- function(A, A_1, A_2) ifelse(A_1 == "0" & A_2 == "0", "1", A)
start_by_stage_3 <- policy_def(
  list(follow_natural, follow_natural, start_at_stage_3, start_at_stage_3),
  natural_action = TRUE,
  full_history = TRUE,
  name = "start_by_stage_3"
)

g_grace <- list(g_glm(~ L_1), g_glm(~ L_2 + A_1), g_glm(~ L_3 + A_2), g_glm(~ L_4 + A_3))
q_grace <- list(
  q_glm(~ A * L_1),
  q_glm(~ A * L_2 + A_1),
  q_glm(~ A * L_3 + A_1 + A_2),
  q_glm(~ A * L_4 + A_1 + A_2 + A_3)
)

test_that("a start-at-stage-3 strategy follows the natural action unless treatment is not started", {
  d <- sim_grace_period(300)
  pa <- start_by_stage_3(d$pd)
  A <- get_actions(d$pd)
  obs <- merge(A, pa)
  expect_equal(obs[stage < 3]$d, obs[stage < 3]$A)

  A_wide <- dcast(A, id ~ stage, value.var = "A")
  never_started <- A_wide[["1"]] == "0" & A_wide[["2"]] == "0"
  expect_true(any(never_started) && any(!never_started))
  for (k in 3:4) {
    expect_equal(obs[stage == k]$d, ifelse(never_started, "1", A_wide[[as.character(k)]]))
  }
})

test_that("a start-at-stage-3 strategy only reweights the subjects that have not started", {
  d <- sim_grace_period(500)
  n <- get_n(d$pd)
  pe <- policy_eval(
    d$pd, policy = start_by_stage_3,
    g_models = g_grace, q_models = q_grace,
    g_full_history = TRUE, q_full_history = TRUE
  )

  A <- dcast(get_actions(d$pd), id ~ stage, value.var = "A")
  A1 <- as.numeric(A[["1"]])
  never_started <- A[["1"]] == "0" & A[["2"]] == "0"
  ## r_k = g^d(A_k | H_k) / g(A_k | H_k), with g^d(1) = 1 if the subject has not started:
  r <- sapply(3:4, function(k) {
    Ak <- as.numeric(A[[as.character(k)]])
    g <- as.matrix(pe$g_values[stage == k, c("g_0", "g_1")])
    ifelse(never_started, (Ak == 1) / g[cbind(seq_len(n), Ak + 1)], 1)
  })

  U <- get_utility(d$pd)$U
  q1 <- as.matrix(pe$q_values[stage == 1, c("Q_0", "Q_1")])
  expect_equal(pe$coef_ipw, mean(apply(r, 1, prod) * U))
  expect_equal(pe$coef_or, mean(q1[cbind(seq_len(n), A1 + 1)]))
  expect_true(all(is.finite(pe$IC)))
})

test_that("a start-at-stage-3 strategy handles right-censoring before the last action", {
  d <- sim_grace_period(600, cens = TRUE)
  expect_true(any(get_stage_data(d$pd)$event == 2))
  c_grace <- list(c_no_censoring(), c_no_censoring(), c_no_censoring(),
                  c_cox(~ L_4 + A_3), c_no_censoring())
  pe <- policy_eval(
    d$pd, policy = start_by_stage_3,
    g_models = g_grace, q_models = q_grace, c_models = c_grace,
    g_full_history = TRUE, q_full_history = TRUE, c_full_history = TRUE
  )
  expect_true(is.finite(coef(pe)))
  expect_true(all(is.finite(pe$IC)))
})
