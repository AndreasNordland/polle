both <- function(p) c(g = p, c = p)

weight_truncation_matrices <- function(n = 500, K = 2, seed = 1,
                                       g1_min = 0.005) {
  set.seed(seed)
  II <- matrix(rbinom(n * (K + 1), 1, 0.7), nrow = n)
  II[, K + 1] <- 1
  G <- matrix(runif(n * (K + 1), 0.2, 1), nrow = n)
  G[, 1] <- runif(n, g1_min, 1)
  G[, K + 1] <- 1
  D <- matrix(1, nrow = n, ncol = K + 1)
  C <- matrix(1, nrow = n, ncol = K + 1)
  list(D = D, C = C, II = II, G = G, K = K)
}

test_that("cumulative_weights without truncation is the cumulated product of D/C * II/G", {
  m <- weight_truncation_matrices()
  cw <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K))
  expect_null(cw$truncation)
  expect_equal(dim(cw$W), c(500, 3))
  expect_equal(cw$W[, 1], m$II[, 1] / m$G[, 1])
  expect_equal(cw$W[, 3], apply(m$II / m$G, 1, prod))

  ## truncating at the maximum is a no-op:
  cw1 <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K, probs = both(1)))
  expect_equal(cw1$W, cw$W)
  expect_true(all(cw1$truncation$n_truncated == 0))
  expect_equal(unique(cw1$truncation$weight), c("g", "c"))
})

test_that("cumulative_weights truncates the stage-wise factors at the stage-wise percentile", {
  m <- weight_truncation_matrices()
  cw <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K, probs = both(0.9)))
  tr <- cw$truncation[weight == "g"]
  expect_equal(tr$stage, 1:3)

  ## stage 1: active observations follow the policy at stage 1
  w1 <- m$II[, 1] / m$G[, 1]
  cap1 <- unname(quantile(w1[w1 > 0], 0.9, type = 1))
  expect_equal(tr$cap[1], cap1)
  expect_equal(cw$W[, 1], pmin(w1, cap1))
  expect_equal(tr$n_active[1], sum(w1 > 0))
  expect_equal(tr$n_truncated[1], sum(w1 > cap1))
  expect_true(tr$n_truncated[1] > 0)

  ## stage 2: active observations follow the policy at stages 1 and 2
  w2 <- m$II[, 2] / m$G[, 2]
  active2 <- (cw$W[, 1] > 0) & (w2 > 0)
  cap2 <- unname(quantile(w2[active2], 0.9, type = 1))
  expect_equal(tr$cap[2], cap2)
  expect_equal(cw$W[, 2], cw$W[, 1] * pmin(w2, cap2))

  ## every stage-wise factor is bounded by its cap:
  W <- cbind(1, cw$W)
  factors <- W[, -1] / ifelse(W[, -ncol(W)] == 0, NA, W[, -ncol(W)])
  expect_true(all(sweep(factors, 2, tr$cap + 1e-10, "<=") | is.na(factors)))
})

test_that("an extreme stage does not change the truncation of later stages", {
  m_mild <- weight_truncation_matrices(g1_min = 0.2)
  m_extreme <- weight_truncation_matrices(g1_min = 0.001)
  ## same policy indicators and stage 2 and 3 probabilities:
  expect_equal(m_mild$II, m_extreme$II)
  expect_equal(m_mild$G[, -1], m_extreme$G[, -1])

  tr_mild <- with(m_mild, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, probs = both(0.95)
  ))$truncation[weight == "g"]
  tr_extreme <- with(m_extreme, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, probs = both(0.95)
  ))$truncation[weight == "g"]

  expect_false(tr_mild$cap[1] == tr_extreme$cap[1])
  expect_equal(tr_mild$cap[2:3], tr_extreme$cap[2:3])
  expect_equal(tr_mild$n_truncated[2:3], tr_extreme$n_truncated[2:3])
})

test_that("cumulative_weights ignores observations that are no longer at risk", {
  m <- weight_truncation_matrices()
  ## observations with a terminal event at stage 1 have no later stages:
  stopped <- seq_len(100)
  m$D[stopped, 2:3] <- NA
  m$C[stopped, 2:3] <- NA
  m$II[stopped, 2:3] <- NA
  m$G[stopped, 2:3] <- NA
  cw <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K, probs = both(0.9)))
  expect_equal(cw$truncation[weight == "g" & stage == 2]$n_active,
               sum(m$II[-stopped, 1] > 0 & m$II[-stopped, 2] > 0))
})

test_that("the action and censoring weights are truncated independently", {
  m <- weight_truncation_matrices(n = 1000)
  set.seed(2)
  n <- nrow(m$D)
  m$D[, 1:2] <- rbinom(n * 2, 1, 0.9)
  m$C[, 1:2] <- runif(n * 2, 0.1, 1)

  cw_none <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K))
  cw_g <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, probs = c(g = 0.9, c = NA)
  ))
  cw_c <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, probs = c(g = NA, c = 0.9)
  ))
  cw_gc <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, probs = c(g = 0.9, c = 0.9)
  ))

  expect_equal(unique(cw_g$truncation$weight), "g")
  expect_equal(unique(cw_c$truncation$weight), "c")
  expect_true(all(cw_g$truncation$n_truncated[1:2] > 0))
  expect_true(all(cw_c$truncation$n_truncated[1:2] > 0))

  ## the caps of one weight do not depend on the truncation of the other:
  expect_equal(cw_gc$truncation[weight == "g"]$cap, cw_g$truncation$cap)
  expect_equal(cw_gc$truncation[weight == "c"]$cap[1:2], cw_c$truncation$cap[1:2])

  ## stage 1: the truncated weight is the product of the truncated factors
  w_g <- m$II[, 1] / m$G[, 1]
  w_c <- m$D[, 1] / m$C[, 1]
  expect_equal(
    cw_gc$W[, 1],
    pmin(w_g, cw_gc$truncation[weight == "g" & stage == 1]$cap) *
      pmin(w_c, cw_gc$truncation[weight == "c" & stage == 1]$cap)
  )
  ## only truncating g leaves the censoring factor untouched:
  expect_equal(cw_g$W[, 1], pmin(w_g, cw_g$truncation$cap[1]) * w_c)
  expect_equal(cw_c$W[, 1], w_g * pmin(w_c, cw_c$truncation$cap[1]))
  expect_true(all(cw_gc$W <= cw_none$W + 1e-10))
})

test_that("policy_eval with weight_truncation = NULL is unchanged", {
  d <- sim_two_stage(2e2, seed = 1)
  pd <- policy_data(
    d,
    action = c("A_1", "A_2"),
    covariates = list(L = c("L_1", "L_2"), C = c("C_1", "C_2")),
    utility = c("U_1", "U_2", "U_3")
  )
  p <- policy_def(1, reuse = TRUE)

  pe <- policy_eval(pd, policy = p)
  pe_null <- policy_eval(pd, policy = p, weight_truncation = NULL)
  expect_equal(pe_null$coef, pe$coef)
  expect_equal(pe_null$IC, pe$IC)
  expect_equal(pe_null$coef_ipw, pe$coef_ipw)
  expect_null(pe_null$weight_truncation)

  ## truncation at the maximum gives the same estimates:
  pe_1 <- policy_eval(pd, policy = p, weight_truncation = 1)
  expect_equal(pe_1$coef, pe$coef)
  expect_equal(pe_1$IC, pe$IC)
  expect_equal(pe_1$coef_ipw, pe$coef_ipw)
  expect_equal(pe_1$coef_or, pe$coef_or)

  set.seed(1)
  pe_cf <- policy_eval(pd, policy = p, M = 2)
  set.seed(1)
  pe_cf_1 <- policy_eval(pd, policy = p, M = 2, weight_truncation = 1)
  expect_equal(pe_cf_1$coef, pe_cf$coef)
  expect_equal(pe_cf_1$IC, pe_cf$IC)
})

test_that("policy_eval reports the weight truncation", {
  d <- sim_two_stage(5e2, seed = 1)
  pd <- policy_data(
    d,
    action = c("A_1", "A_2"),
    covariates = list(L = c("L_1", "L_2"), C = c("C_1", "C_2")),
    utility = c("U_1", "U_2", "U_3")
  )
  p <- policy_def(1, reuse = TRUE, name = "A=1")

  pe <- policy_eval(pd, policy = p, weight_truncation = 0.9)
  tab <- pe$weight_truncation
  expect_true(is.data.table(tab))
  expect_equal(
    names(tab),
    c("name", "stage", "weight", "cap", "n_active", "n_truncated")
  )
  expect_equal(tab$stage, rep(1:3, each = 2))
  expect_equal(tab$weight, rep(c("g", "c"), 3))
  expect_true(all(tab$n_truncated <= tab$n_active))
  tab_g <- tab[weight == "g"]
  expect_true(all(tab_g$n_truncated[1:2] > 0))
  ## no actions at stage K + 1:
  expect_equal(tab_g$n_truncated[3], 0)
  ## no censoring in the data:
  expect_true(all(tab[weight == "c"]$n_truncated == 0))

  ## truncating only the action weights:
  pe_g <- policy_eval(pd, policy = p, weight_truncation = c(g = 0.9))
  expect_equal(unique(pe_g$weight_truncation$weight), "g")
  expect_equal(pe_g$coef, pe$coef)

  ## one table per fold when cross-fitting:
  pe_cf <- policy_eval(pd, policy = p, M = 2, weight_truncation = 0.9)
  tab_cf <- pe_cf$weight_truncation
  expect_equal(unique(tab_cf$fold), c("fold_1", "fold_2"))
  expect_equal(nrow(tab_cf), 2 * 3 * 2)
  ## the caps are calculated within each fold:
  expect_false(tab_cf[fold == "fold_1" & weight == "g"]$cap[1] ==
               tab_cf[fold == "fold_2" & weight == "g"]$cap[1])
})

test_that("policy_eval truncation reduces the standard error with extreme weights", {
  set.seed(1)
  n <- 3e3
  L_1 <- rnorm(n)
  A_1 <- rbinom(n, 1, plogis(3 * L_1))
  L_2 <- 0.5 * L_1 + 0.5 * A_1 + rnorm(n)
  A_2 <- rbinom(n, 1, plogis(3 * L_2))
  U_3 <- exp(0.5 * (L_2 + A_2) + 0.5 * rnorm(n))
  d <- data.frame(L_1, A_1, L_2, A_2, U_1 = 0, U_2 = 0, U_3)
  pd <- policy_data(
    d,
    action = c("A_1", "A_2"),
    covariates = list(L = c("L_1", "L_2")),
    utility = c("U_1", "U_2", "U_3")
  )
  p <- policy_def(1, reuse = TRUE)

  args <- list(
    policy_data = pd,
    policy = p,
    g_models = g_glm(~L),
    q_models = q_glm(~ A + L)
  )
  pe <- do.call(policy_eval, args)
  pe_tr <- do.call(policy_eval, c(args, list(weight_truncation = 0.99)))

  expect_true(max(pe$g_values$g_1) > 0.99 && min(pe$g_values$g_1) < 0.01)
  se <- function(x) sqrt(var(x$IC) / length(x$IC))
  expect_lt(se(pe_tr), se(pe))
  expect_true(pe_tr$weight_truncation[weight == "g"][1, n_truncated] > 0)
})

test_that("weight_truncation input checks", {
  d <- sim_single_stage(1e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
  p <- policy_def(1)

  mes <- "weight_truncation must be NULL"
  expect_error(policy_eval(pd, policy = p, weight_truncation = 0), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = 1.1), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = c(0.9, 0.99)), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = "0.99"), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = c(h = 0.99)), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = c(g = NA)), mes)
  expect_error(policy_eval(pd, policy = p, weight_truncation = c(g = 0.9, g = 0.9)), mes)
  expect_error(
    policy_eval(pd, policy = p, target = "subgroup", weight_truncation = 0.99),
    "only implemented for target = 'value'"
  )

  expect_error(
    policy_eval(pd, policy = p, target = "subgroup", weight_bound = 0.01),
    "only implemented for target = 'value'"
  )
  mes_b <- "weight_bound must be NULL"
  expect_error(policy_eval(pd, policy = p, weight_bound = 0), mes_b)
  expect_error(policy_eval(pd, policy = p, weight_bound = 1), mes_b)
  expect_error(policy_eval(pd, policy = p, weight_bound = c(0.1, 0.2)), mes_b)
  expect_error(policy_eval(pd, policy = p, weight_bound = c(h = 0.1)), mes_b)
  expect_equal(check_weight_bound(0.1), c(g = 0.1, c = 0.1))
  expect_equal(check_weight_bound(c(c = 0.1)), c(g = NA_real_, c = 0.1))

  expect_equal(check_weight_truncation(0.9), c(g = 0.9, c = 0.9))
  expect_equal(check_weight_truncation(c(c = 0.9)), c(g = NA_real_, c = 0.9))
  expect_equal(check_weight_truncation(c(c = 0.8, g = 0.9)), c(g = 0.9, c = 0.8))
})

test_that("cumulative_weights bounds the probabilities at a fixed level", {
  m <- weight_truncation_matrices(n = 1000)
  none <- c(g = NA_real_, c = NA_real_)
  cw_none <- with(m, cumulative_weights(D = D, C = C, II = II, G = G, K = K))

  ## a bound of (almost) zero is a no-op:
  cw0 <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, bounds = both(1e-8)
  ))
  expect_equal(cw0$W, cw_none$W)
  expect_true(all(cw0$truncation$n_truncated == 0))

  ## bounding g is the same as replacing g by max(g, bound):
  b <- 0.3
  cw_b <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, bounds = c(g = b, c = NA)
  ))
  G_b <- pmax(m$G, b)
  expect_equal(cw_b$W[, 2], apply((m$II / G_b)[, 1:2], 1, prod))
  tr <- cw_b$truncation
  expect_equal(unique(tr$weight), "g")
  expect_equal(tr$cap, rep(1 / b, 3))
  expect_equal(tr$n_truncated[1], sum(m$II[, 1] == 1 & m$G[, 1] < b))
  expect_true(tr$n_truncated[1] > 0)

  ## the bound does not depend on the other stages:
  m_extreme <- weight_truncation_matrices(n = 1000, g1_min = 0.001)
  cw_e <- with(m_extreme, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, bounds = c(g = b, c = NA)
  ))
  expect_equal(cw_e$truncation$cap, tr$cap)
  expect_equal(cw_e$truncation$n_truncated[2:3], tr$n_truncated[2:3])

  ## a bound on c caps the censoring factor only:
  set.seed(3)
  m$D[, 1:2] <- rbinom(2000, 1, 0.9)
  m$C[, 1:2] <- runif(2000, 0.1, 1)
  cw_c <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K, bounds = c(g = NA, c = 0.5)
  ))
  expect_equal(unique(cw_c$truncation$weight), "c")
  expect_equal(
    cw_c$W[, 1],
    m$II[, 1] / m$G[, 1] * pmin(m$D[, 1] / m$C[, 1], 2)
  )

  ## bound and percentile: the bound is applied first
  cw_bp <- with(m, cumulative_weights(
    D = D, C = C, II = II, G = G, K = K,
    probs = c(g = 0.9, c = NA), bounds = c(g = b, c = NA)
  ))
  tr_bp <- cw_bp$truncation
  w1 <- pmin(m$II[, 1] / m$G[, 1], 1 / b)
  cap1 <- unname(quantile(w1[w1 > 0], 0.9, type = 1))
  expect_equal(tr_bp$cap[1], cap1)
  expect_true(all(tr_bp$cap <= 1 / b))
  expect_equal(cw_bp$W[, 1], pmin(w1, cap1) * m$D[, 1] / m$C[, 1])
})

test_that("policy_eval with weight_bound", {
  d <- sim_two_stage(5e2, seed = 1)
  pd <- policy_data(
    d,
    action = c("A_1", "A_2"),
    covariates = list(L = c("L_1", "L_2"), C = c("C_1", "C_2")),
    utility = c("U_1", "U_2", "U_3")
  )
  p <- policy_def(1, reuse = TRUE, name = "A=1")

  pe <- policy_eval(pd, policy = p)
  ## NULL and a vanishing bound give the same estimate:
  pe_0 <- policy_eval(pd, policy = p, weight_bound = 1e-8)
  expect_equal(pe_0$coef, pe$coef)
  expect_equal(pe_0$IC, pe$IC)
  expect_equal(pe_0$coef_ipw, pe$coef_ipw)

  pe_b <- policy_eval(pd, policy = p, weight_bound = c(g = 0.4))
  tab <- pe_b$weight_truncation
  expect_equal(unique(tab$weight), "g")
  expect_equal(tab$cap[1:2], rep(2.5, 2))
  expect_true(all(tab$n_truncated <= tab$n_active))
  expect_true(tab$n_truncated[1] > 0)
  expect_false(isTRUE(all.equal(pe_b$coef_ipw, pe$coef_ipw)))

  ## bound and percentile together, with cross-fitting:
  pe_cf <- policy_eval(
    pd, policy = p, M = 2, weight_bound = 0.4, weight_truncation = 0.95
  )
  expect_equal(unique(pe_cf$weight_truncation$fold), c("fold_1", "fold_2"))
  expect_true(all(pe_cf$weight_truncation[weight == "g"]$cap <= 2.5))
})
