
#' @title Simulate Single-Stage Data
#'
#' @param n Number of observations.
#' @param par Named vector with distributional parameters.
#' \itemize{
#'  \item{} \code{k}: \eqn{\kappa}
#'  \item{} \code{d}: \eqn{\delta}
#'  \item{} \code{a}: \eqn{\alpha}
#'  \item{} \code{b}: \eqn{\beta}
#'  \item{} \code{c}: \eqn{\gamma}
#'  \item{} \code{p}: \eqn{\pi}
#' }
#' @param action_model Function used to specify the action/treatment probability (logit link).
#' @param utility_model Function used to specify the conditional mean utility.
#' @param seed Integer.
#' @param return_model If TRUE, the [lava::lvm] model is returned.
#' @param ... Additional arguments passed to [lava::lvm()].
#' @returns data.frame with n rows and columns Z, L, B, A, and U.
#' @details
#' \code{sim_single_stage} samples \code{n} iid observation
#' \eqn{O = (B, Z, L, A, U)} with the following distribution:
#' \deqn{
#' B \sim Bernoulli(\pi)\\
#' Z, L \sim Uniform([0,1])\\
#' A\mid Z,L,B \sim Bernoulli(expit\{\kappa Z^{-2}(Z+L-1) + \delta B)\\
#' U\mid Z,L,A \sim \mathcal{N}(Z+L+A\cdot\{\gamma Z + \alpha L + \beta\}, 1)
#' }
#' @export
sim_single_stage <- function(n=1e4,
                             par=c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, p = .3),
                             action_model = function(Z, L, B, k, d){
                               k*(Z + L - 1)*Z^(-2) + d*(B==1)
                             },
                             utility_model = function(Z, L, A, a, b, c){
                               Z + L + A*(c*Z + a*L + b)
                             },
                             seed=NULL,
                             return_model=FALSE,
                             ...) {
  if (!is.null(seed)) set.seed(seed)
  m <- lava::lvm(...)
  lava::parameter(m) <- ~ a + b + c + d + k + p
  lava::distribution(m, ~Z+L+B+A) <- list(lava::uniform.lvm(),
                                          lava::uniform.lvm(),
                                          lava::binomial.lvm("identity"),
                                          lava::binomial.lvm("logit"))
  lava::regression(m, ~B) <- function(p){p}
  lava::regression(m, ~A) <- action_model
  lava::regression(m, ~U) <- utility_model

  if (return_model) return(m)
  lava::sim(m, n, p=par)
}

#' @title Simulate Single-Stage Multi-Action Data
#'
#' @param n Number of observations.
#' @param seed Integer.
#' @returns data.frame with n rows and columns z, x, a, and u.
#' @details
#' \code{sim_single_stage_multi_actions} samples \code{n} iid observation
#' \eqn{O = (z, x, a, u)} with the following distribution:
#' \deqn{
#' z, x \sim Uniform([0,1])\\
#' \tilde a \sim \mathcal{N}(0,1)\\
#' a \mid \tilde a \sim
#' \begin{cases}
#' 0 \quad if \quad \tilde a < -1\\
#' 1 \quad if \quad \tilde a -1 \leq a < 0.5\\
#' 2 \quad otherwise
#' \end{cases}\\
#' u \mid z, x \sim \mathcal{N}(x + z + I\{a=2\}(x-0.5) + I\{a=1\}(x^2 + z -0.5), 1)
#' }
#' @export
sim_single_stage_multi_actions <- function(n = 1e3, seed = NULL){
  if (!is.null(seed)) set.seed(seed)

  z <- stats::runif(n)
  x <- stats::runif(n)
  a0 <- stats::rnorm(mean = x, n = n)
  afun <- Vectorize(function(y){
    if (y<(-1)) return(0)
    else if (y<0.5) return(1)
    else return(2)
  })
  a <- afun(a0)
  u <- stats::rnorm(
    mean = x + z + (a==2)*(x-0.5) + (a==1)*(x*x+z-0.5),
    n = n
  )

  d <- data.frame(
    z = z,
    x = x,
    a = a,
    u = u
  )
  return(d)
}


#' @title Simulate Two-Stage Data
#'
#' @param n Number of observations.
#' @param par Named vector with distributional parameters.
#' \itemize{
#'  \item{} \code{gamma}: \eqn{\gamma}
#'  \item{} \code{beta}: \eqn{\beta}
#' }
#' @param seed Integer.
#' @param action_model_1 Function used to specify the action/treatment at stage 1.
#' @param action_model_2 Function used to specify the action/treatment at stage 2.
#' @param deterministic_rewards Logical. If TRUE, the deterministic reward
#' contributions are returned as well (columns U_1_A0, U_1_A1, U_2_A0, U_2_A1).
#' @details
#' \code{sim_two_stage} samples \code{n} iid observation
#' \eqn{O} with the following distribution:
#' \eqn{BB} is a random categorical variable with levels \code{group1},
#' \code{group2}, and \code{group3}. Furthermore,
#' \deqn{
#' B \sim \mathcal{N}(0,1)\\
#' L_{1} \sim \mathcal{N}(0, 1)\\
#' C_{1} \mid L_{1} \sim \mathcal{N}(L_1, 1)\\
#' A_1 \mid C_1 \sim Bernoulli(expit(\beta C_1))\\
#' L_{2} \sim \mathcal{N} (0, 1)\\
#' C_{2} \mid A_1, L_1 \sim \mathcal{N}(\gamma L_1 + A_1, 1)\\
#' A_2 \mid C_2 \sim Bernoulli(expit(\beta C_2))\\
#' L_{3} \sim \mathcal{N} (0, 1)
#' }
#' The rewards are calculated as
#' \deqn{
#' U_1 = L_1\\
#' U_2 = A_1\cdot C_1 + L_2 \\
#' U_3 = A_2\cdot C_2 + L_3.
#' }
#' @returns [data.table] with n rows and columns B, BB, L_1, C_1, A_1, L_2, C_2,
#' A_2, L_3, U_1, U_2, U_3 (,U_1_A0, U_1_A1, U_2_A0, U_2_A1).
#' @export
sim_two_stage <- function(n = 1e4,
                          par = c(gamma = 0.5,  beta = 1),
                          seed = NULL,
                          action_model_1 = function(C_1, beta, ...)
                            stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1)),
                          action_model_2 = function(C_2, beta, ...)
                            stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_2)),
                          deterministic_rewards = FALSE){
  if (!is.null(seed)) set.seed(seed)

  gamma <- getElement(par, "gamma")
  beta <- getElement(par, "beta")

  # baseline
  B <- stats::rnorm(n = n)
  BB <- sample(c("group1", "group2", "group3"), n, replace = TRUE)

  # stage 1
  L_1 <- stats::rnorm(n = n)
  C_1 <- L_1 + stats::rnorm(n = n)
  U_1 <- L_1
  A_1 <- action_model_1(C_1 = C_1, L_1 = L_1, beta = beta, gamma = gamma)
  U_1_0 <- C_1 * 0
  U_1_1 <- C_1

  # stage 2
  L_2 <- stats::rnorm(n = n, 0, 1)
  C_2 <- (gamma * L_1 + A_1) + stats::rnorm(n = n)
  U_2 <- A_1 * C_1 + L_2
  A_2 <- action_model_2(C_1 = C_1, L_1 = L_1, C_2 = C_2, L_2 = L_2, A_1 = A_1, beta = beta, gamma = gamma)
  U_2_0 <- C_2 * 0
  U_2_1 <- C_2

  L_3 <- stats::rnorm(n = n)
  U_3 <- A_2 * C_2 + L_3

  data <- data.table(
    B = B,
    BB = BB,
    L_1 = L_1,
    C_1 = C_1,
    A_1 = A_1,
    L_2 = L_2,
    C_2 = C_2,
    A_2 = A_2,
    L_3 = L_3,
    U_1 = U_1,
    U_2 = U_2,
    U_3 = U_3
  )
  if (deterministic_rewards == TRUE){
    U_1_A0 <- U_1_A1 <- U_2_A0 <- U_2_A1 <- NULL
    data[, U_1_A0 := U_1_0]
    data[, U_1_A1 := U_1_1]
    data[, U_2_A0 := U_2_0]
    data[, U_2_A1 := U_2_1]
  }

  return(data)
}

#' @title Simulate Two-Stage Multi-Action Data
#'
#' @param n Number of observations.
#' @param par Named vector with distributional parameters.
#' \itemize{
#'  \item{} \code{gamma}: \eqn{\gamma}
#'  \item{} \code{beta}: \eqn{\beta}
#'  \item{} \code{prob}: \eqn{p}
#' }
#' @param seed Integer.
#' @param action_model_1 Function used to specify the dichotomous
#' action/treatment at stage 1.
#' @details
#' \code{sim_two_stage_multi_actions} samples \code{n} iid observation
#' \eqn{O} with the following distribution:
#' \eqn{BB} is a random categorical variable with levels \code{group1},
#' \code{group2}, and \code{group3}. Furthermore,
#' \deqn{
#' B \sim \mathcal{N}(0,1)\\
#' L_{1} \sim \mathcal{N}(0, 1)\\
#' C_{1} \mid L_{1} \sim \mathcal{N}(L_1, 1)\\
#' P(A_1='yes'\mid C_1) =  expit(\beta C_1)\\
#' P(A_1='no'\mid C_1) = 1 - P(A_1='yes' \mid C_1)\\
#' L_{2} \sim \mathcal{N} (0, 1)\\
#' C_{2} \mid A_1, L_1 \sim \mathcal{N}(\gamma L_1 + A_1, 1)\\
#' P(A_2='yes') = p_1\\
#' P(A_2='no') = p_2\\
#' P(A_2='default') = p_3\\
#' L_{3} \sim \mathcal{N} (0, 1)
#' }
#' The rewards are calculated as
#' \deqn{
#' U_1 = L_1\\
#' U_2 = A_1\cdot C_1 + L_2 \\
#' U_3 = A_2\cdot C_2 + L_3.
#' }
#' @returns [data.table] with n rows and columns B, BB, L_1, C_1, A_1, L_2, C_2,
#' A_2, L_3, U_1, U_2, U_3.
#' @export
sim_two_stage_multi_actions <- function(n=1e3,
                                        par=list(gamma = 0.5,
                                                 beta = 1,
                                                 prob = c(0.2, 0.4, 0.4)),
                                        seed=NULL,
                                        action_model_1 = function(C_1, beta, ...)
                                          stats::rbinom(n = NROW(C_1), size = 1, prob = lava::expit(beta * C_1))){
  if (!is.null(seed)) set.seed(seed)

  gamma <- getElement(par, "gamma")
  beta <- getElement(par, "beta")
  prob <- getElement(par, "prob")

  # baseline
  B <- stats::rnorm(n = n)
  BB <- sample(c("group1", "group2", "group3"), n, replace = TRUE)

  # stage 1
  L_1 <- stats::rnorm(n = n)
  C_1 <- L_1 + stats::rnorm(n = n)
  U_1 <- L_1
  a_1 <- action_model_1(C_1 = C_1, L_1 = L_1, beta = beta, gamma = gamma)
  A_1 <- ifelse(a_1, "yes", "no")
  U_1_0 <- C_1 * 0
  U_1_1 <- C_1

  # stage 2
  L_2 <- stats::rnorm(n = n, 0, 1)
  C_2 <- (gamma * L_1 + a_1) + stats::rnorm(n = n)
  U_2 <- a_1 * C_1 + L_2
  A_2 <- sample(c("yes", "no", "default"), prob = prob, size = n, replace = TRUE)
  U_2_0 <- C_2 * 0
  U_2_1 <- C_2

  L_3 <- stats::rnorm(n = n)
  U_3 <- (A_2 == "yes") * C_2 + L_3

  data <- data.table(
    B = B,
    BB = BB,
    L_1 = L_1,
    C_1 = C_1,
    A_1 = A_1,
    L_2 = L_2,
    C_2 = C_2,
    A_2 = A_2,
    L_3 = L_3,
    U_1 = U_1,
    U_2 = U_2,
    U_3 = U_3
  )

  return(data)

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
  w <- stats::rnorm(n=1)

  # baseline variable
  b <- stats::rbinom(n = 1, size = 1, prob = xi)

  # stage specific variables
  x_vec <- vector("numeric")
  x_lead_vec <- vector("numeric")

  stage <- 1
  entry_vec <- c(entry_vec, 0)
  t <- 0
  x_lead <- 0

  while (t<tau){
    x <- stats::rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead) + (alpha[5] * b), sd = sigma)
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
    t_increment <- if(a == 1) stats::rexp(n = 1, rate = rate) else Inf

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

#' @title Simulate Multi-Stage Data
#'
#' @param n Number of observations.
#' @param par Named list with distributional parameters.
#' \itemize{
#'  \item{} \code{tau}: \eqn{\tau}
#'  \item{} \code{gamma}: \eqn{\gamma}
#'  \item{} \code{alpha}: \eqn{\alpha}
#'  \item{} \code{beta}: \eqn{\beta}
#'  \item{} \code{psi}: \eqn{\psi}
#'  \item{} \code{xi}: \eqn{\xi}
#' }
#' @param seed Integer.
#' @param a Function used to specify the action/treatment at every stage.
#' @details
#' \code{sim_multi_stage} samples \code{n} iid observation
#' \eqn{O} with the following distribution:
#' \deqn{
#' W \sim \mathcal{N}(0, 1)\\
#' B \sim Ber(\xi)
#' }
#' For \eqn{k\geq 1} let
#' \deqn{
#' (T_k - T_{k-1})| X_{k-1}, A_{k-1}, W \sim
#' \begin{cases}
#' Exp\Big\{\exp\left(\gamma^T [1, X_{k-1}, W] \right) \Big\} + \psi \quad A_{k-1} = 1\\
#' \infty \quad A_{k-1} = 0
#' \end{cases}\\
#' X_{k}\mid T_k, X_{k-1}, B \sim
#' \begin{cases}
#' \mathcal{N}\left\{ \alpha^T [1, T_k, T^2_k, X_{k-1}, B], 1\right\} \quad T_k < \infty \\
#' 0 \quad T_k = \infty
#' \end{cases}\\
#' A_k \mid X_k, T_k \sim
#' \begin{cases}
#' Ber\left\{ expit\left(\beta^T[1, T_{k}^2, X_k] \right)\right\} \quad T_k < \infty\\
#' 0 \quad T_k = \infty,
#' \end{cases}
#' }
#' Note that \eqn{\psi} is the minimum increment.
#' @returns list with elements \code{stage_data} ([data.table]) and
#'  \code{baseline_data} ([data.table]).
#' @export
sim_multi_stage <- function(n,
                            par = list(tau = 10,
                                       gamma = c(0, -0.2, 0.3),
                                       alpha = c(0, 0.5, 0.2, -0.5, 0.4),
                                       beta = c(3, -0.5, -0.5),
                                       psi = 1,
                                       xi = 0.3),
                            a = function(t, x, beta, ...){
                              prob <- lava::expit(beta[1] + (beta[2] * t^2) + (beta[3] * x))
                              stats::rbinom(n = 1, size = 1, prob = prob)
                            },
                            seed = NULL){
  par <- append(par, list(sigma = 1))

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
  U <- exit <- entry <- A <- X <- event <- U_A0 <- U_A1 <-  NULL
  stage_data[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
  stage_data[event %in% c(0), U_A0 := 0]
  stage_data[event %in% c(0), U_A1 := -X]
  stage_data[, A := as.character(A)]

  setnames(stage_data, "exit", "t")
  stage_data[, entry := NULL]
  setcolorder(stage_data, c("id", "stage", "event"))
  setindex(stage_data, NULL)

  id <- NULL
  baseline_data <- as.data.table(do.call(what  = "rbind", l["baseline_data",]))
  baseline_data[, id := as.numeric(id)]

  return(list(stage_data = stage_data, baseline_data = baseline_data))
}

