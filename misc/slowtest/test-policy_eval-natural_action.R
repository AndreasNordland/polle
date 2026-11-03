library(polle)
library(data.table)

## two-stage DGP with right-censoring before the second action:
sim_natural <- function(n, cens = TRUE, intervene = FALSE) {
  L1 <- rnorm(n)
  A1 <- if (intervene) rep(1, n) else rbinom(n, 1, plogis(0.5 * L1))
  S2 <- rbinom(n, 1, plogis(-0.5 + 0.5 * A1 + 0.5 * L1))
  L2 <- 0.5 * L1 + 0.3 * A1 + rnorm(n)
  A2 <- rbinom(n, 1, plogis(0.5 * L2 + 0.5 * A1 - 0.3))
  if (intervene) A2 <- ifelse(S2 == 1, A2, 1)
  Y <- 1 + L2 + 0.5 * A1 + A2 * (1 - 2 * S2) + 0.5 * S2 * L1 + rnorm(n)
  Tc <- if (cens) rexp(n, rate = exp(-1.5 + 0.8 * L2 + 0.8 * S2)) else rep(Inf, n)
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
  pd <- policy_data(
    sd, type = "long", id = "id", stage = "stage", event = "event",
    action = "A", utility = "U", time = "time", time2 = "time2"
  )
  list(pd = pd, Y = Y)
}

## treat, unless S = 1 (e.g., cancer) in which case the natural action is followed:
treat_unless_s <- policy_def(
  list(1, function(A, S) ifelse(S == 1, A, "1")),
  natural_action = TRUE,
  name = "treat_unless_S"
)

q_correct <- list(q_glm(~ A * L_1), q_glm(~ L_2 + A_1 + A * S_2 + S_2:L_1))
q_wrong <- list(q_glm(~ A * L_1), q_glm(~ L_2 + A_1 + A))
g_correct <- list(g_glm(~ L_1), g_glm(~ L_2 + A_1))
c_correct <- list(c_no_censoring(), c_cox(~ L_2 + S_2), c_no_censoring())
c_wrong <- list(c_no_censoring(), c_cox(~ 1), c_no_censoring())

estimate_treat_unless_s <- function(d, q_models, g_models, c_models) {
  pe <- policy_eval(
    d$pd,
    policy = treat_unless_s,
    q_models = q_models, g_models = g_models, c_models = c_models,
    g_full_history = TRUE, q_full_history = TRUE, c_full_history = TRUE
  )
  unname(coef(pe))
}

test_that("policy_eval with a natural-value policy is doubly robust under right-censoring", {
  set.seed(1)
  truth <- mean(sim_natural(1e6, cens = FALSE, intervene = TRUE)$Y)

  n <- 2000
  nsim <- 40
  scenarios <- list(
    all_correct = list(q_correct, g_correct, c_correct),
    q_wrong = list(q_wrong, g_correct, c_correct),
    c_wrong = list(q_correct, g_correct, c_wrong)
  )
  for (s in names(scenarios)) {
    est <- vapply(seq_len(nsim), function(i) {
      set.seed(100 + i)
      d <- sim_natural(n)
      estimate_treat_unless_s(d, scenarios[[s]][[1]], scenarios[[s]][[2]], scenarios[[s]][[3]])
    }, numeric(1))
    bias <- mean(est) - truth
    se <- sd(est) / sqrt(nsim)
    expect_lt(abs(bias), 4 * se + 0.02, label = paste0("|bias| in scenario ", s))
  }
})

test_that("policy_eval with a natural-value policy is close to lmtp without censoring", {
  skip_if_not_installed("lmtp")
  skip_if_not_installed("SuperLearner")

  set.seed(1)
  n <- 4000
  d <- sim_natural(n, cens = FALSE)
  pd <- d$pd
  sd <- get_stage_data(pd)
  wide <- data.frame(
    L1 = sd[stage == 1]$L, A1 = as.numeric(get_actions(pd)[stage == 1]$A),
    S2 = sd[stage == 2]$S, L2 = sd[stage == 2]$L,
    A2 = as.numeric(get_actions(pd)[stage == 2]$A),
    U = get_utility(pd)$U
  )

  pe <- policy_eval(
    pd, policy = treat_unless_s,
    q_models = q_correct, g_models = g_correct,
    g_full_history = TRUE, q_full_history = TRUE, M = 5
  )

  shift <- function(data, trt) {
    if (trt == "A2") ifelse(data$S2 == 1, data$A2, 1) else rep(1, nrow(data))
  }
  fit <- suppressWarnings(
    lmtp::lmtp_sdr(
      wide, trt = c("A1", "A2"), outcome = "U",
      time_vary = list(c("L1"), c("S2", "L2")), shift = shift,
      outcome_type = "continuous",
      learners_outcome = "SL.glm.interaction",
      learners_trt = "SL.glm.interaction", folds = 5
    )
  )
  expect_equal(unname(coef(pe)), fit$estimate@x, tolerance = 0.1)
})

## four-stage design where treatment can be started at every stage. The outcome
## is observed at the final stage (K+1 = 5).
sim_grace <- function(n = 400, seed = 1, intervene = FALSE, cens = FALSE) {
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

q_grace_wrong <- list(q_glm(~ 1), q_glm(~ 1), q_glm(~ 1), q_glm(~ 1))
c_grace <- list(c_no_censoring(), c_no_censoring(), c_no_censoring(),
                c_cox(~ L_4 + A_3), c_no_censoring())

estimate_grace <- function(d, q_models, cens) {
  args <- list(
    d$pd, policy = start_by_stage_3,
    g_models = g_grace, q_models = q_models,
    g_full_history = TRUE, q_full_history = TRUE
  )
  if (cens) args <- c(args, list(c_models = c_grace, c_full_history = TRUE))
  unname(coef(do.call(policy_eval, args)))
}

test_that("policy_eval is doubly robust for a start-at-stage-3 strategy", {
  set.seed(1)
  truth <- mean(sim_grace(1e6, intervene = TRUE)$Y)

  n <- 2000
  nsim <- 40
  scenarios <- list(
    q_correct = list(q_grace, FALSE),
    q_wrong = list(q_grace_wrong, FALSE),
    q_correct_censored = list(q_grace, TRUE),
    q_wrong_censored = list(q_grace_wrong, TRUE)
  )
  for (s in names(scenarios)) {
    est <- vapply(seq_len(nsim), function(i) {
      d <- sim_grace(n, seed = 100 + i, cens = scenarios[[s]][[2]])
      estimate_grace(d, scenarios[[s]][[1]], scenarios[[s]][[2]])
    }, numeric(1))
    bias <- mean(est) - truth
    se <- sd(est) / sqrt(nsim)
    expect_lt(abs(bias), 4 * se + 0.03, label = paste0("|bias| in scenario ", s))
  }
})
