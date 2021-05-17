library(polle)

par0 <- list(
  mu_L = c(-1.4, 1.1, 1.3),
  mu_C = c(-2.1),
  gamma_C = c(-1.3,1.5),
  gamma_A = -1.2, # gamma_A must be negative
  sigma_L = 1.5,
  sigma_C = 2,
  alpha = 0.02
)

# observed action at stage 1
a_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  n <- length(C_1)
  a_1 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_1))

  return(a_1)
}

# observed action at stage 2
a_20 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1
  A_1 <- data$A_1
  C_2 <- data$C_2
  n <- length(C_1)
  a_2 <- rbinom(n = n, size = 1, prob = lava::expit(par$gamma_A * C_2)) * A_1

  return(a_2)
}

# optimal action at stage 2
d_alpha_opt_20 <- function(data, par){
  C_2 <- data$C_2
  A_1 <- data$A_1

  fit_2 <- lava::expit(par$gamma_A * C_2)
  ((fit_2 > 1 - par$alpha) + (fit_2 >= par$alpha & fit_2 <= 1 - par$alpha) * (C_2 + par$mu_L[3] > 0)) * A_1
}

# optimal action at stage 1
d_alpha_opt_10 <- function(data, par){
  C_1 <- data$C_1
  L_1 <- data$L_1

  fit_1 <- lava::expit(par$gamma_A * C_1)
  out <- ((fit_1 >= par$alpha & fit_1 <= 1 - par$alpha) * (C_1 + par$mu_L[2] + polle:::kappa_1(l_1 = L_1, par = par) + polle:::kappa_2(l_1 = L_1, par = par)) + (fit_1 > 1 - par$alpha)) > 0

  out <- as.numeric(out)
  return(out)
}

# # approximated mean utility
# n <- 2e6
# set.seed(1)
# d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
# two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
# utility(two_stage_policy_data)
# observed_utility <- mean(utility(two_stage_policy_data)$U)
# rm(two_stage_policy_data)
#
# approximated mean utility under the optimal policy
n <- 2e6
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = d_alpha_opt_10, a_2 = d_alpha_opt_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
utility(two_stage_policy_data)
optimal_utility <- mean(utility(two_stage_policy_data)$U)
rm(two_stage_policy_data)


# binomial model
binom_model <- function(A, X){
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

  class(bm) <- "binom_model"
  return(bm)
}

predict.binom_model <- function(object, new_X, type = "probs", action_set){
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

policy_1 <- function(history){
  pol <- history$H[, c("id", "stage")]
  pol[, d := "1"]
  return(pol)
}

policy_0 <- function(history){
  pol <- history$H[, c("id", "stage")]
  pol[, d := "0"]
  return(pol)
}

policy_opt_stage_1 <- function(history){
  stopifnot(
    all(c("C_1", "L_1") %in% colnames(history$H))
  )
  d <- d_alpha_opt_10(data = data.table(C_1 = history$H$C_1, L_1 = history$H$L_1), par = par0)
  pol <- history$H[, c("id", "stage")]
  pol[, d := as.character(d)]
  return(pol)
}

policy_opt_stage_2 <- function(history){
  stopifnot(
    all(c("C_2", "A_1") %in% colnames(history$H))
  )
  d <- d_alpha_opt_20(data = data.table(C_2 = history$H$C_2, A_1 = as.numeric(history$H$A_1)), par = par0)
  pol <- history$H[, c("id", "stage")]
  pol[, d := as.character(d)]
  return(pol)
}


n <- 2e3
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

two_stage_markov_history <- markov_history(two_stage_policy_data)
polle:::get_stage_history(two_stage_policy_data, stage = 2, full_history = F)
two_stage_g_function <- g_function(two_stage_markov_history, binom_model)
two_stage_g_function$gm$glm_model$coefficients
par0$gamma_A
head(predict(two_stage_g_function, new_history = two_stage_markov_history))

head(get_policy_actions(
  two_stage_policy_data,
  policy = list(policy_opt_stage_1, policy_opt_stage_2),
  policy_full_history = T
))

tmp <- fit_g_model(
  two_stage_policy_data,
  g_model = list(binom_model, binom_model),
  g_full_history = FALSE
)
get_function_predictions(two_stage_policy_data, tmp, full_history = FALSE)
rm(tmp)


n <- 2e3
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
tmp <- ipw(
  two_stage_policy_data,
  policy = list(policy_opt_stage_1, policy_opt_stage_2),
  g_model = binom_model,
  g_full_history = FALSE,
  policy_full_history = TRUE
  )
tmp$value
# optimal_utility
rm(tmp)


linear_model <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ A * ., data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "linear_model"
  return(m)

  return(m)
}

Q_res_interept_model <- function(V_res, A, X){
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  data <- cbind(A = A, X)
  lm_model <- lm(V_res ~ 1, data = data, model = FALSE)

  m <- list(
    lm_model = lm_model
  )

  class(m) <- "linear_model"
  return(m)

  return(m)
}

predict.linear_model <- function(object, new_X, action_set){
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

n <- 2e3
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
tmp <- or(
  two_stage_policy_data,
  policy = list(policy_opt_stage_1, policy_opt_stage_2),
  Q_model = list(linear_model, linear_model),
  Q_full_history = TRUE,
  policy_full_history = TRUE
)
tmp$value

q_2 <- structure(list(), class = "q_2")
predict.q_2 <- function(object, new_X, action_set){
  n <- nrow(new_X)

  stopifnot(
    all(action_set == c("0", "1"))
  )

  preds <- matrix(
    c(
      rep(0, n),
      rep(par0$mu_L[3], n)
    ),
    ncol = 2
  )

  return(preds)
}
Q_2 <- structure(
  list(
    qm = q_2,
    X_names = c("L_2", "C_2")
  ),
  class = "q_function"
)

q_1 <- structure(list(), class = "q_1")
predict.q_1 <- function(object, new_X, action_set){
  n <- nrow(new_X)

  stopifnot(
    all(action_set == c("0", "1")),
    all(colnames(new_X) == c("L_1", "C_1"))
  )

  l_1 <- new_X[, colnames(new_X) == "L_1"]

  preds <- matrix(
    c(
      rep(0,n),
      par0$mu_L[2] +
        kappa_1(l_1 = l_1, par = par0) +
        kappa_2(l_1 = l_1, par = par0)
    ),
    ncol = 2
  )

  return(preds)
}
Q_1 <- structure(
  list(
    qm = q_1,
    X_names = c("L_1", "C_1")
  ),
  class = "q_function"
)

n <- 2e3
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)
tmp <- or(
  two_stage_policy_data,
  policy = list(policy_opt_stage_1, policy_opt_stage_2),
  Q_function = list(Q_1, Q_2),
  Q_full_history = TRUE,
  policy_full_history = TRUE
)
tmp$value
# optimal_utility
rm(tmp)


# DR ----------------------------------------------------------------------

# intercept model
intercept_model <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))

  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }

  glm_model <- glm(A ~ 1, data = X, family = binomial(), model = FALSE)

  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "binom_model"
  return(bm)
}

n <- 1e6
set.seed(1)
d <- simulate_two_stage_data(n = n, par = par0, a_1 = a_10, a_2 = a_20)
two_stage_policy_data <- new_policy_data(stage_data = d, baseline_data = d[, .(id =unique(id))]); rm(d)

tmp <- dr(
  two_stage_policy_data,
  policy = list(policy_opt_stage_1, policy_opt_stage_2),
  g_model = list(binom_model, binom_model),
  Q_function = list(Q_1, Q_2),
  g_full_history = FALSE,
  Q_full_history = TRUE,
  policy_full_history = TRUE
)
tmp$value
mean(tmp$phi_or)
mean(tmp$phi_ipw)
