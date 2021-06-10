# TODO: bug if history object contains 1 observation:

smpd <- function(d, tau, lambda, alpha, sigma, beta, gamma, psi, rho, ...){
  stage_vec <- vector("numeric")
  entry_vec <- vector("numeric")
  exit_vec <- vector("numeric")
  event_vec <- vector("numeric")
  a_vec <- vector("numeric")

  # t_inc_vec <- vector("numeric")

  # baseline covariates
  z <- rbinom(n = 1, size = 1, prob = 0.3)
  z <- ifelse(z == 1, "a", "b")

  # stage specific covariates
  x_vec <- vector("numeric")
  x_lead_vec <- vector("numeric")

  stage <- 1
  entry_vec <- c(entry_vec, 0)
  # rate <- lambda
  # t_increment <- rexp(n = 1, rate = rate)
  # t <- t_increment
  t <- 0
  x_lead <- 0

  while (t<tau){
    x <- rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead) + (alpha[5] * (z == "a")), sd = sigma)
    a <- d(stage = stage, t = t, x = x, z = z, beta = beta)

    exit_vec <- c(exit_vec, t)
    stage_vec <- c(stage_vec, stage)
    event_vec <- c(event_vec, 0)
    x_vec <- c(x_vec, x)
    a_vec <- c(a_vec, a)
    x_lead_vec <- c(x_lead_vec, x_lead)

    if (a == 1){
      entry_vec <- c(entry_vec, t)
    }
    # the time increment comes from an exponential distribution with mean exp(lambda + rho * x)
    # remember that mean(t_increment  = 1 / rate)
    t_increment <- if(a == 1) rexp(n = 1, 1) / exp(lambda + rho * x)  else Inf

    #t_inc_vec <- c(t_inc_vec, t_increment)

    t <- t + t_increment + psi # minimum increment psi
    stage <- stage + 1
    x_lead <- x
  }

  if (a == 0){
    stage_vec <- c(stage_vec, stage)
    entry_vec <- c(entry_vec, last(exit_vec))
    exit_vec <- c(exit_vec, last(exit_vec))
    event_vec <- c(event_vec, 1)
    a_vec <- c(a_vec, NA)
    x_vec <- c(x_vec, NA)
    x_lead_vec <- c(x_lead_vec, NA)
    #t_inc_vec <- c(t_inc_vec, NA)

  }

  if (a == 1){
    stage_vec <- c(stage_vec, stage)
    exit_vec <- c(exit_vec, tau)
    event_vec <- c(event_vec, 2)
    a_vec <- c(a_vec, NA)
    x_vec <- c(x_vec, NA)
    x_lead_vec <- c(x_lead_vec, NA)
    #t_inc_vec <- c(t_inc_vec, NA)
  }

  stage_data <- matrix(c(stage_vec, entry_vec, exit_vec, event_vec, a_vec, x_vec, x_lead_vec), ncol = 7, byrow = FALSE)
  colnames(stage_data) <- c("stage", "entry", "exit", "event", "A", "X", "X_lead")

  baseline_data <- matrix(z, ncol = 1)
  colnames(baseline_data) <- c("Z")

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}

d_obs <- function(stage, t, x, z, beta){
  prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
  rbinom(n = 1, size = 1, prob = prob)
}

d_1 <- function(stage, t, x, z, beta){
  return(1)
}

d_0 <- function(stage, t, x, z, beta){
  return(0)
}

d_1_stage_4_obs <- function(stage, t, x, z, beta){
  if (stage <= 4){
    out <- 1
  } else{
    prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
    out <- rbinom(n = 1, size = 1, prob = prob)
  }
  return(out)
}

simulate_policy_data <- function(n, args){
  l <- sapply(
    1:n,
    function(id){
      d <- do.call(what = "smpd", args)
      stage_data <- d$stage_data
      baseline_data <- d$baseline_data

      stage_data <- cbind(id = id, stage_data)
      baseline_data <- cbind(id = id, baseline_data)

      return(list(stage_data = stage_data, baseline_data = baseline_data))
    },
    simplify = "array"
  )

  stage_data <- do.call(what  = "rbind", l["stage_data",])
  stage_data <- as.data.table(stage_data)
  stage_data[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
  stage_data[event %in% c(0), U_0 := 0]
  stage_data[event %in% c(0), U_1 := -X]
  stage_data[, A := as.character(A)]


  setnames(stage_data, "exit", "t")
  stage_data[, entry := NULL]
  stage_data[, X_lead := NULL]

  setcolorder(stage_data, c("id", "stage", "event"))
  setindex(stage_data, NULL)

  baseline_data <- as.data.table(do.call(what  = "rbind", l["baseline_data",]))
  baseline_data[, id := as.numeric(id)]

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}

args0 <- list(
  d = d_obs,
  tau = 10,
  lambda = 0, # exp(-lambda) = mean, rate = 1 / mean
  alpha =  c( # distribution of x
    0, # intercept
    0.5, # t,
    0.1, # t^2, x (the cost) will increase with t
    -0.5, # x_lead
    0.4 # z
  ),
  beta = c( # distribution of a
    0.3, # intercept
    -0.1, # t
    -0.1, # x
    0.3 # z == "a"
  ),
  sigma = 1,
  gamma = -0.1,
  psi = 1, # minimum time increment
  rho = -0.4 # Cox parameter for X_lead (the cost), if negative, the rate will decrease
)


# g-model -----------------------------------------------------------------

# binomial model
g_binomial_linear <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ ., data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "g_binomial"
  return(bm)
}

predict.g_binomial <- function(object, new_X, type = "probs", action_set){
  glm_model <- object$glm_model

  stopifnot(
    all(action_set == c("0","1"))
  )

  # model matrix as data.frame
  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")

  probs <- sapply(as.numeric(action_set), function(a) a * fit + (1-a) * (1-fit))

  return(probs)
}

# policies ----------------------------------------------------------------

always_treat_stage_policy <- function(history){
  pol <- history$H[, c("id", "stage")]
  pol[, d := "1"]
  return(pol)
}

always_treat_policy <- new_policy(
  stage_policies = always_treat_stage_policy,
  full_history = FALSE,
  replicate = TRUE
)


# Q-model -----------------------------------------------------------------

Q_linear <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ A * ., data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "Q_linear"
  return(m)

  return(m)
}

predict.Q_linear <- function(object, new_X, action_set){
  lm_model <- object$lm_model

  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }

  preds <- sapply(
    action_set,
    function(action){
      data <- cbind(A = action, new_X)
      predict(lm_model, newdata = data, type = "response")
    }
  )

  return(preds)
}

# approximations ------------------------------------------------------
n <- 5e5

pd <- simulate_policy_data(n, args0)
pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
true_value_observed <- mean(utility(pd)$U)
rm(pd)

args1 <- args0
args1$d <- d_1
pd1 <- simulate_policy_data(n, args1)
pd1 <- new_policy_data(stage_data = pd1$stage_data, baseline_data = pd1$baseline_data)
true_value_always_treat <- mean(utility(pd1)$U)
rm(pd1)

args1 <- args0
args1$d <- d_1_stage_4_obs
pd1 <- simulate_policy_data(n, args1)
pd1 <- new_policy_data(stage_data = pd1$stage_data, baseline_data = pd1$baseline_data)
true_value_always_treat_policy_partial_4 <- mean(utility(pd1)$U)
rm(pd1)

rm(n)

# ipw -------------------------------------------------

# always treat policy:
set.seed(1)
multi_stage_policy_data <- simulate_policy_data(2e3, args0)
multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)

tmp <- ipw(
  multi_stage_policy_data,
  g_model = g_binomial_linear,
  g_full_history = FALSE,
  policy = always_treat_policy
)
tmp$value
true_value_always_treat
tmp$g_function$gm
args0$beta
rm(tmp)

set.seed(1)
m <- 1e3

pb <- progress::progress_bar$new(
                               format = " simulating [:bar] :percent eta: :eta",
                               total = m, clear = FALSE, width= 60)
estimate_always_treat <- vector(mode = "numeric", length = m)
for (i in seq_along(estimate_always_treat)){

  pd <- simulate_policy_data(2e3, args0)
  pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)

  tmp <- ipw(
    pd,
    g_model = g_binomial_linear,
    g_full_history = FALSE,
    policy = always_treat_policy
  )
  estimate_always_treat[i] <- tmp$value
  pb$tick()
  rm(tmp)
}
mean(estimate_always_treat)
sd(estimate_always_treat)
true_value_always_treat

# dr -------------------------------------------------

# always treat policy up to stage 4
KK <- 4
set.seed(3)
multi_stage_policy_data <- simulate_policy_data(2e4, args0)
multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)

tmp <- dr(
  multi_stage_policy_data,
  g_model = g_binomial_linear,
  q_model = rep(list(Q_linear), multi_stage_policy_data$dim$K),
  g_full_history = FALSE,
  q_full_history = FALSE,
  policy = always_treat_policy
)
tmp$value
mean(tmp$phi_ipw)
mean(tmp$phi_or)
true_value_always_treat_policy_partial_4

set.seed(1)
m <- 5e2
pb <- progress::progress_bar$new(
  format = " simulating [:bar] :percent eta: :eta",
  total = m, clear = FALSE, width= 60)
estimate_always_treat_policy_partial_4 <- vector(mode = "numeric", length = m)
for (i in seq_alongestimate_always_treat_policy_partial_4){
  pb$tick()
  pd <- simulate_policy_data(2e3, args0)
  pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
  pd <- partial(pd, K = KK)

  tmp <- dr(
    pd,
    g_model = g_binomial_linear,
    q_model = rep(list(Q_linear), pd$dim$K),
    g_full_history = FALSE,
    q_full_history = FALSE,
    policy = always_treat_policy
  )
  estimate_always_treat_policy_partial_4[i] <- tmp$value
  rm(tmp)
}
mean(estimate_always_treat_policy_partial_4)
sd(estimate_always_treat_policy_partial_4)
true_value_always_treat_policy_partial_4

# realistic Q-learning ----------------------------------------------------

# always treat policy up to stage 4
KK <- 4
set.seed(1)
multi_stage_policy_data <- simulate_policy_data(2e4, args0)
multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)

RQL_partial_4 <- realistic_Q_learning(
  multi_stage_policy_data,
  alpha = 0.05,
  g_model = g_binomial_linear,
  q_model = rep(list(Q_linear), multi_stage_policy_data$dim$K),
  g_full_history = FALSE,
  q_full_history = FALSE
)
tmp$value

set.seed(1)
m <- 5e2
pb <- progress::progress_bar$new(
  format = " simulating [:bar] :percent eta: :eta",
  total = m, clear = FALSE, width= 60)
estimate_RQL_partial_4 <- vector(mode = "numeric", length = m)
for (i in 1:m){
  pb$tick()
  pd <- simulate_policy_data(2e3, args0)
  pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
  pd <- partial(pd, K = KK)

  tmp <- dr(
    pd,
    g_model = g_binomial_linear,
    q_model = rep(list(Q_linear), pd$dim$K),
    g_full_history = FALSE,
    q_full_history = FALSE,
    policy = get_policy(RQL_partial_4)
  )
  estimate_RQL_partial_4[i] <- tmp$value
  rm(tmp)
}
mean(estimate_RQL_partial_4)


# policy tree -------------------------------------------------------------

KK <- 4
set.seed(1)
multi_stage_policy_data <- simulate_policy_data(2e3, args0)
multi_stage_policy_data <- new_policy_data(stage_data = multi_stage_policy_data$stage_data, baseline_data = multi_stage_policy_data$baseline_data)
multi_stage_policy_data <- partial(multi_stage_policy_data, K = KK)

ptl <- policy_tree_learning(
  multi_stage_policy_data,
  g_model = g_binomial_linear,
  q_model = rep(list(Q_linear), multi_stage_policy_data$dim$K),
  g_full_history = FALSE,
  q_full_history = FALSE,
  policy_full_history = FALSE,
  depth = 2
)
#ptl$pt[[2]]

set.seed(1)
m <- 1e2
pb <- progress::progress_bar$new(
  format = " simulating [:bar] :percent eta: :eta",
  total = m, clear = FALSE, width= 60)
estimate_ptl_4 <- vector(mode = "numeric", length = m)
for (i in 1:m){
  pb$tick()
  pd <- simulate_policy_data(2e3, args0)
  pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
  pd <- partial(pd, K = KK)

  tmp <- dr(
    pd,
    g_model = g_binomial_linear,
    q_model = rep(list(Q_linear), pd$dim$K),
    g_full_history = FALSE,
    q_full_history = FALSE,
    policy = get_policy(ptl)
  )
  estimate_ptl_4[i] <- tmp$value
  rm(tmp)
}
mean(estimate_ptl_4)
true_value_observed
true_value_always_treat_policy_partial_4

