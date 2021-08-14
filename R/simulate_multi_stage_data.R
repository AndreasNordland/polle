# simulate_multistage_observation <- function(d, tau, lambda, alpha, sigma, beta, gamma, psi, rho, ...){
#   stage_vec <- vector("numeric")
#   entry_vec <- vector("numeric")
#   exit_vec <- vector("numeric")
#   event_vec <- vector("numeric")
#   a_vec <- vector("numeric")
#
#   # t_inc_vec <- vector("numeric")
#
#   # baseline covariates
#   z <- rbinom(n = 1, size = 1, prob = 0.3)
#   z <- ifelse(z == 1, "a", "b")
#
#   # stage specific covariates
#   x_vec <- vector("numeric")
#   x_lead_vec <- vector("numeric")
#
#   stage <- 1
#   entry_vec <- c(entry_vec, 0)
#   # rate <- lambda
#   # t_increment <- rexp(n = 1, rate = rate)
#   # t <- t_increment
#   t <- 0
#   x_lead <- 0
#
#   while (t<tau){
#     x <- rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead) + (alpha[5] * (z == "a")), sd = sigma)
#     a <- d(stage = stage, t = t, x = x, z = z, beta = beta)
#
#     exit_vec <- c(exit_vec, t)
#     stage_vec <- c(stage_vec, stage)
#     event_vec <- c(event_vec, 0)
#     x_vec <- c(x_vec, x)
#     a_vec <- c(a_vec, a)
#     x_lead_vec <- c(x_lead_vec, x_lead)
#
#     if (a == 1){
#       entry_vec <- c(entry_vec, t)
#     }
#     # the time increment comes from an exponential distribution with mean exp(lambda + rho * x)
#     # remember that mean(t_increment  = 1 / rate)
#     t_increment <- if(a == 1) rexp(n = 1, 1) / exp(lambda + rho * x)  else Inf
#
#     #t_inc_vec <- c(t_inc_vec, t_increment)
#
#     t <- t + t_increment + psi # minimum increment psi
#     stage <- stage + 1
#     x_lead <- x
#   }
#
#   if (a == 0){
#     stage_vec <- c(stage_vec, stage)
#     entry_vec <- c(entry_vec, last(exit_vec))
#     exit_vec <- c(exit_vec, last(exit_vec))
#     event_vec <- c(event_vec, 1)
#     a_vec <- c(a_vec, NA)
#     x_vec <- c(x_vec, NA)
#     x_lead_vec <- c(x_lead_vec, NA)
#     #t_inc_vec <- c(t_inc_vec, NA)
#
#   }
#
#   if (a == 1){
#     stage_vec <- c(stage_vec, stage)
#     exit_vec <- c(exit_vec, tau)
#     event_vec <- c(event_vec, 2)
#     a_vec <- c(a_vec, NA)
#     x_vec <- c(x_vec, NA)
#     x_lead_vec <- c(x_lead_vec, NA)
#     #t_inc_vec <- c(t_inc_vec, NA)
#   }
#
#   stage_data <- matrix(c(stage_vec, entry_vec, exit_vec, event_vec, a_vec, x_vec, x_lead_vec), ncol = 7, byrow = FALSE)
#   colnames(stage_data) <- c("stage", "entry", "exit", "event", "A", "X", "X_lead")
#
#   baseline_data <- matrix(z, ncol = 1)
#   colnames(baseline_data) <- c("Z")
#
#   return(list(stage_data = stage_data, baseline_data = baseline_data))
# }


# simulate_multi_stage_data <- function(n, args){
#   l <- sapply(
#     1:n,
#     function(id){
#       d <- do.call(what = "simulate_multistage_observation", args)
#       stage_data <- d$stage_data
#       baseline_data <- d$baseline_data
#
#       stage_data <- cbind(id = id, stage_data)
#       baseline_data <- cbind(id = id, baseline_data)
#
#       return(list(stage_data = stage_data, baseline_data = baseline_data))
#     },
#     simplify = "array"
#   )
#
#   stage_data <- do.call(what  = "rbind", l["stage_data",])
#   stage_data <- as.data.table(stage_data)
#   stage_data[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
#   stage_data[event %in% c(0), U_0 := 0]
#   stage_data[event %in% c(0), U_1 := -X]
#   stage_data[, A := as.character(A)]
#
#   setnames(stage_data, "exit", "t")
#   stage_data[, entry := NULL]
#   setcolorder(stage_data, c("id", "stage", "event"))
#   setindex(stage_data, NULL)
#
#   baseline_data <- as.data.table(do.call(what  = "rbind", l["baseline_data",]))
#   baseline_data[, id := as.numeric(id)]
#
#   return(list(stage_data = stage_data, baseline_data = baseline_data))
# }
