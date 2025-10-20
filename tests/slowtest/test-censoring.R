future::plan("multicore")

library(mets)

# Check single-stage policy estimation under right-
sim1 <- function(n=5e3, teff=log(0.5), tau=1, ...) {
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  time0 <- rexp(n) / exp(teff*a)
  cens <- rexp(n, .25)
  time <- pmin(time0, cens)
  status <- (time0 < cens) * 1
  y <- (time < tau) * 1
  d <- data.frame(y, time, status, x, a, id = seq_along(y), stage = 1, time0)
  return(d)
}

polle_riskreg <- function(data, tau, ...) {
  # Putting data on long-format
  pd <- polle::policy_data(data = data,
                           id = "id",
                           action = "a",
                           covariates = c("x", "status"),
                           utility = "y")
  dat <- pd$stage_data
  dat[stage == 1, time := 0] # No censoring prior to intervention
  dat[stage == 2, time := pmin(data$time, tau)]
  dat[stage == 2, status := data$status]
  dat[stage == 2 & (status == 0 & time < tau), event := 2]
  dat[stage == 2 & (status == 0 & time < tau), U := NA]
  dat[, status := NULL]

  pd <- polle::policy_data(
      data = dat, action = "A",
      type = "long",
      utility = "U",
      stage = "stage",
      time = "time",
      event = "event",
      id = "id"
      )

  pe1 <- polle::policy_eval(
      policy_data = pd,
      policy = polle::policy_def(1),
      g_models = polle::g_glm(~1),
      q_models = polle::q_glm(~ 1 + A),
      m_model = polle::q_glm(~ 1 + A_1),
      m_full_history = TRUE,
      c_models = list(
          polle::c_no_censoring(),
          polle::c_cox(formula = ~ strata(A_1))
      ),
      c_full_history = TRUE
      )
  pe0 <- polle::policy_eval(
      policy_data = pd,
      policy = polle::policy_def(0),
      g_models = polle::g_glm(~1),
      q_models = polle::q_glm(~ 1 + A),
      m_model = polle::q_glm(~ 1 + A_1),
      m_full_history = TRUE,
      c_models = list(
          polle::c_no_censoring(),
          polle::c_cox(formula = ~ strata(A_1))
      ),
      c_full_history = TRUE
      )
  merge(pe0, pe1)
}


onerun <- function(..., tau = 1, teff = log(0.5)) {
    d <- sim1(..., teff = teff, tau = tau)
    coxs <- mets::phreg(mets::Event(time, status) ~ mets::strata(a), data = d)
    pr <- predict(coxs, # stratified cox P(T>1 | A=a)
        time = tau,
        newdata = data.frame(a = 0:1), se = TRUE
    )
    est <- polle_riskreg(data = d, tau = tau) # polle P(T<=1 | A=a)
    # ipw
    p0 <- mets::phreg(
        mets::Event(time, status == 0) ~ mets::strata(a),
        data = d
    )
    pr0 <- predict(p0, # censoring weights
        time = pmin(d$time, tau),
        individual.time = TRUE,
        newdata = data.frame(a = rep(1, nrow(d))), se = FALSE
        )


    est_ipw <- with(d, c(
        mean((y * status / pr0$surv)[a == 0]),
        mean((y * status / pr0$surv)[a == 1])
        ))

    true <- c(1 - exp(-tau), 1 - exp(-exp(teff) * tau))
    # latent time
    # with(d, c(mean(time0[a == 0] < tau), mean(time0[a == 1] < tau)))
    res <- c(
        true,
        est_ipw,
        coef(est),
        1 - pr$surv,
        diag(vcov(est))  ** 0.5,
        pr$se.surv[, 1]
    )
    names(res) <-
        c(
            "true.0", "true.1",
            "ipw.0", "ipw.1",
            "polle.0", "polle.1",
            "cox.0", "cox.1",
            "se.polle.0", "se.polle.1",
            "se.cox.0", "se.cox.1"
        )
    return(res)
}


test_that("policy_eval with right-censoring, asymptotics", {
    nsim <- 500
    res1 <- lava::sim(onerun, nsim, args = list(n = 2000))
    s0 <- summary(res1,
        est = c("polle.0", "cox.0", "ipw.0"),
        se = c("se.polle.0", "se.cox.0", NA),
        true = res1[1, 1]
    )
    s1 <- summary(res1,
        est = c("polle.1", "cox.1", "ipw.1"),
        se = c("se.polle.1", "se.cox.1", NA),
        true = res1[1, 2]
    )

    bias.delta <- 1e-2
    expect_true(all(s0["Bias", ] < bias.delta))
    expect_true(all(s1["Bias", ] < bias.delta))

    norm.delta <- 0.25
    r0 <- abs(1 - s0["SE", 1:2] / s0["SD", 1:2])
    r1 <- abs(1 - s1["SE", 1:2] / s1["SD", 1:2])
    expect_true(all(r0 < norm.delta))
    expect_true(all(r1 < norm.delta))

    # under the null
    res0 <- lava::sim(onerun, nsim, args = list(n = 2000, teff = 0))
    s0 <- summary(res0,
        est = c("polle.0", "cox.0", "ipw.0"),
        se = c("se.polle.0", "se.cox.0", NA),
        true = res0[1, 1]
    )
    s1 <- summary(res0,
        est = c("polle.1", "cox.1", "ipw.1"),
        se = c("se.polle.1", "se.cox.1", NA),
        true = res0[1, 2]
    )

    bias.delta <- 1e-3
    expect_true(all(s0["Bias", ] < bias.delta))
    expect_true(all(s1["Bias", ] < bias.delta))

    norm.delta <- 0.25
    r0 <- abs(1 - s0["SE", 1:2] / s0["SD", 1:2])
    r1 <- abs(1 - s1["SE", 1:2] / s1["SD", 1:2])
    expect_true(all(r0 < norm.delta))
    expect_true(all(r1 < norm.delta))
})
