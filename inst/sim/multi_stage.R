library(data.table)

a0 <- function(t, x, beta, ...){
  prob <- lava::expit(beta[1] + (beta[2] * t^2) + (beta[3] * x))
  rbinom(n = 1, size = 1, prob = prob)
}

sim_multi_stage_obs <- function(a,
                                tau,
                                gamma,
                                alpha,
                                sigma,
                                beta,
                                psi,
                                xi,
                                ...){
  a_fun <- a
  stage_vec <- vector("numeric")
  entry_vec <- vector("numeric")
  exit_vec <- vector("numeric")
  event_vec <- vector("numeric")
  a_vec <- vector("numeric")

  # latent variable
  w <- rnorm(n=1)

  # baseline variable
  b <- rbinom(n = 1, size = 1, prob = xi)

  # stage specific variables
  x_vec <- vector("numeric")
  x_lead_vec <- vector("numeric")

  stage <- 1
  entry_vec <- c(entry_vec, 0)
  t <- 0
  x_lead <- 0

  while (t<tau){
    x <- rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead) + (alpha[5] * b), sd = sigma)
    a <- a_fun(stage = stage, t = t, x = x, x_lead = x_lead, b = b, beta = beta)

    exit_vec <- c(exit_vec, t)
    stage_vec <- c(stage_vec, stage)
    event_vec <- c(event_vec, 0)
    x_vec <- c(x_vec, x)
    a_vec <- c(a_vec, a)
    x_lead_vec <- c(x_lead_vec, x_lead)

    if (a == 1){
      entry_vec <- c(entry_vec, t)
    }
    # the time increment comes from an exponential distribution with mean exp(gamma[1] + gamma[2] * x + gamma[3] * w)
    # remember that mean(t_increment)  = 1 / rate
    rate <- exp((gamma[1] + gamma[2] * x + gamma[3] * w))
    t_increment <- if(a == 1) rexp(n = 1, rate = rate) else Inf

    t <- t + t_increment + psi # minimum time increment psi
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
  }

  if (a == 1){
    stage_vec <- c(stage_vec, stage)
    exit_vec <- c(exit_vec, tau)
    event_vec <- c(event_vec, 2)
    a_vec <- c(a_vec, NA)
    x_vec <- c(x_vec, NA)
    x_lead_vec <- c(x_lead_vec, NA)
  }

  stage_data <- matrix(c(stage_vec, entry_vec, exit_vec, event_vec, a_vec, x_vec, x_lead_vec), ncol = 7, byrow = FALSE)
  colnames(stage_data) <- c("stage", "entry", "exit", "event", "A", "X", "X_lead")

  baseline_data <- matrix(b, ncol = 1) # cbind(b, w)
  colnames(baseline_data) <- c("B") # c("B", "W")

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}

sim_multi_stage <- function(n,
                            par = list(tau = 10,
                                       gamma = c(0, -0.2, 0.3),
                                       alpha = c(0, 0.5, 0.2, -0.5, 0.4),
                                       sigma = 1,
                                       beta = c(3, -0.5, -0.5),
                                       psi = 1,
                                       xi = 0.3),
                            a = a0,
                            seed = NULL){

  if (!is.null(seed)) set.seed(seed)

  if (!is.null(a)){
    par <- append(par, list(a = a))
  }

  l <- sapply(
    1:n,
    function(id){
      d <- do.call(what = "sim_multi_stage_obs", par)
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
  stage_data[event %in% c(0), U_A0 := 0]
  stage_data[event %in% c(0), U_A1 := -X]
  stage_data[, A := as.character(A)]

  setnames(stage_data, "exit", "t")
  stage_data[, entry := NULL]
  setcolorder(stage_data, c("id", "stage", "event"))
  setindex(stage_data, NULL)

  baseline_data <- as.data.table(do.call(what  = "rbind", l["baseline_data",]))
  baseline_data[, id := as.numeric(id)]

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}
