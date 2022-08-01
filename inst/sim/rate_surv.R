library(future.apply)
library(lava)
library(survival)
library(polle)
library(mets)

sim_surv <- function(n.grp, beta, zeta, kappa){
  n <- 2 * n.grp

  # id
  id <- 1:n

  # covariate W
  W <- runif(n, min = 1, max = 3)

  # treatment
  A <- c(rep(0, n.grp), rep(1, n.grp))

  # observed exposure
  pd <- lava::expit(kappa[1] + kappa[2] * W)
  D1 <- rbinom(n,1,pd)
  D0 <- rep(0, n)

  D <- D1 * A + D0 * (1-A)

  # simulate T
  TT1 <- c(unlist(rexp(n, 1) / exp(matrix(c(W, D1, W * D1), ncol = 3) %*% beta)))
  TT0 <- c(unlist(rexp(n, 1) / exp(matrix(c(W, D0, W * D0), ncol = 3) %*% beta)))
  TT <- TT1 * A + TT0 * (1-A)

  # simulate C
  C <- c(unlist(rexp(n, 1) / exp(matrix(c(W, A, D, W * A), ncol = 4) %*% zeta)))

  time <- apply(cbind(TT, C), 1, min)
  event <- TT < C

  d <- data.frame(
    id = id,
    W = W,
    A = A,
    D1 = D1,
    D0 = D0,
    D = D,
    TT1 = TT1,
    TT0 = TT0,
    TT = TT,
    C = C,
    time = time,
    event = event
  )
  d <- d[order(time), ]

  return(d)
}

par0 <- list(
  beta = c(0.6, -0.5, -0.3),
  zeta = c(0.2, -0.4, -0.8, 0.6),
  kappa = c(2, -0.5),
  tau = 0.5
)

# true values ----------------------------------------------------------

set.seed(1)
d0 <- sim_surv(n.grp = 5e6, beta = par0$beta, zeta = par0$zeta, kappa = par0$kappa)
Psi0_A1 <- mean(((d0$TT1 <= par0$tau)))
Psi0_A0 <- mean((d0$TT0 <= par0$tau))
Psi0_D1 <- mean(d0$D[d0$A == 1])
Psi0 <- mean(((d0$TT1 <= par0$tau) - (d0$TT0 <= par0$tau))[d0$D1 == 1])
rm(d0)

## Cox ---------------------------------------------------------------------

onerun_cox <- function(n.grp){
  dt <- sim_surv(
    n = n.grp,
    beta = par0$beta,
    zeta = par0$zeta,
    kappa = par0$kappa
  )

  require("mets")
  est <- polle:::RATE.surv(
    treatment = A ~ 1,
    post.treatment = D ~ A * W,
    SL.args.post.treatment = list(family = binomial(),
                                  SL.library = c("SL.glm")),
    response = Surv(time, event) ~ W*D,
    call.response = "phreg",
    censoring = Surv(time, event == 0) ~ W*A + D,
    call.censoring = "phreg",
    tau = par0$tau,
    data = dt,
    M = 2
  )

  out <- c(
    est$coef,
    setNames(diag(est$vcov)^(0.5), paste(names(est$coef), ".se", sep = ""))
  )
  return(out)
}
# set.seed(1)
# onerun_cox(1e3)

# future::plan("multicore")
# progressr::handlers(global = TRUE)
# progressr::handlers("progress")
# sim.res.cox <- sim(onerun_cox, R = 1e3, args = list(n.grp = 1e3), seed = 1)
# future::plan("sequential")
# summary(sim.res.cox, estimate = 1:4, se = 5:8, true = c(Psi0_A1, Psi0_A0, Psi0_D1, Psi0))

## Super Learner & Ranger -----------------------------------------

onerun_ranger <- function(n.grp){
  dt <- sim_surv(
    n = n.grp,
    beta = par0$beta,
    zeta = par0$zeta,
    kappa = par0$kappa
  )

  require("ranger")
  est <- polle:::RATE.surv(
    treatment = A ~ 1,
    post.treatment = D ~ A * W,
    SL.args.post.treatment = list(family = binomial(),
                                  SL.library = c("SL.glm")),
    response = Surv(time, event) ~ W + D,
    args.response = list(num.threads = NULL),
    call.response = "ranger",
    censoring = Surv(time, event == 0) ~ W + A + D,
    args.censoring = list(num.threads = NULL),
    call.censoring = "ranger",
    tau = par0$tau,
    data = dt,
    M = 2
  )

  out <- c(
    est$coef,
    setNames(diag(est$vcov)^(0.5), paste(names(est$coef), ".se", sep = ""))
  )
  return(out)
}

onerun_rfsrc <- function(n.grp){
  dt <- sim_surv(
    n = n.grp,
    beta = par0$beta,
    zeta = par0$zeta,
    kappa = par0$kappa
  )

  require("randomForestSRC")
  est <- polle:::RATE.surv(
    treatment = A ~ 1,
    post.treatment = D ~ A * W,
    SL.args.post.treatment = list(family = binomial(),
                                  SL.library = c("SL.glm")),
    response = Surv(time, event) ~ W + D,
    call.response = "rfsrc",
    args.response = list(save.memory = TRUE, ntime = NULL),
    censoring = Surv(time, event == 0) ~ W + A + D,
    args.censoring = list(save.memory = TRUE, ntime = NULL),
    call.censoring = "rfsrc",
    tau = par0$tau,
    data = dt,
    M = 2
  )

  out <- c(
    est$coef,
    setNames(diag(est$vcov)^(0.5), paste(names(est$coef), ".se", sep = ""))
  )
  return(out)
}
# set.seed(1)
# onerun_rfsrc(1e3)

# rfsrc
# future::plan("sequential")
# progressr::handlers(global = TRUE)
# progressr::handlers("progress")
# sim.res.rfsrc <- sim(onerun_rfsrc, R = 10, args = list(n.grp = 1e3), seed = 1)
# summary(sim.res.rfsrc, estimate = 1:4, se = 5:8, true = c(Psi0_A1, Psi0_A0, Psi0_D1, Psi0))

# ranger
# future::plan(list(tweak("multisession", workers = 2)))
# future::plan("sequential")
# progressr::handlers(global = TRUE)
# progressr::handlers("progress")
# sim.res.ranger <- sim(onerun_ranger, R = 500, args = list(n.grp = 1e3), seed = 2)
# summary(sim.res.ranger, estimate = 1:4, se = 5:8, true = c(Psi0_A1, Psi0_A0, Psi0_D1, Psi0))

### Doubly Robist ----

onerun_ranger_response <- function(n.grp){
  dt <- sim_surv(
    n = n.grp,
    beta = par0$beta,
    zeta = par0$zeta,
    kappa = par0$kappa
  )

  require("ranger")
  est <- polle:::RATE.surv(
    treatment = A ~ 1,
    post.treatment = D ~ A * W,
    SL.args.post.treatment = list(family = binomial(),
                                  SL.library = c("SL.glm")),
    response = Surv(time, event) ~ W*D,
    call.response = "phreg",
    censoring = Surv(time, event == 0) ~ W + A + D,
    args.censoring = list(num.threads = NULL),
    call.censoring = "ranger",
    tau = par0$tau,
    data = dt,
    M = 2
  )

  out <- c(
    est$coef,
    setNames(diag(est$vcov)^(0.5), paste(names(est$coef), ".se", sep = ""))
  )
  return(out)
}

onerun_ranger_censoring <- function(n.grp){
  dt <- sim_surv(
    n = n.grp,
    beta = par0$beta,
    zeta = par0$zeta,
    kappa = par0$kappa
  )

  require("ranger")
  est <- polle:::RATE.surv(
    treatment = A ~ 1,
    post.treatment = D ~ A * W,
    SL.args.post.treatment = list(family = binomial(),
                                  SL.library = c("SL.glm")),
    response = Surv(time, event) ~ W + D,
    args.response = list(num.threads = NULL),
    call.response = "ranger",
    censoring = Surv(time, event == 0) ~ W*A + D,
    call.censoring = "phreg",
    tau = par0$tau,
    data = dt,
    M = 2
  )

  out <- c(
    est$coef,
    setNames(diag(est$vcov)^(0.5), paste(names(est$coef), ".se", sep = ""))
  )
  return(out)
}

future::plan("sequential")
progressr::handlers(global = TRUE)
progressr::handlers("progress")
sim.res.ranger.response <- sim(onerun_ranger_response, R = 500, args = list(n.grp = 1e3), seed = 2)
progressr::handlers(global = TRUE)
progressr::handlers("progress")
sim.res.ranger.censoring <- sim(onerun_ranger_censoring, R = 500, args = list(n.grp = 1e3), seed = 2)

# save --------------------------------------------------------------------

save.image(file = "rate_surv_dr.RData")
