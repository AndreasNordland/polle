library("lava")
m <- lvm()
parameter(m) <- ~ gamma + beta
distribution(m, ~ L_1 + C_1 ) <- list(gaussian.lvm(), gaussian.lvm())
regression(m, ~ C_1) <- function(L_1)  L_1

transform(m, U_1~L_1) <- function(L_1) L_1
distribution(m, ~U_1_0) <- list(0)
transform(m, U_1_1~C_1) <- function(C_1) C_1

distribution(m, ~A_1) <- binomial.lvm("logit")
regression(m, ~A_1) <- function(C_1, beta) beta * C_1

distribution(m, ~ L_2 + C_2 ) <- list(gaussian.lvm(), gaussian.lvm())
regression(m, ~ C_2) <- function(L_1, A_1, gamma)  (gamma*L_1) + A_1

transform(m, U_2 ~ A_1 + C_1 + L_2) <- function(x) x[1]*x[2] + x[3]
distribution(m, ~U_2_0) <- list(0)
transform(m, U_2_1~C_2) <- function(C_2) C_2

distribution(m, ~A_2) <- binomial.lvm("logit")
regression(m, ~A_2) <- function(C_2, beta) beta * C_2

distribution(m, ~U_3) <- gaussian.lvm()
regression(m, ~ U_3) <- function(A_2, C_2) A_2 * C_2

par0 <- list(
  gamma = 0.5,
  beta = 1
)
data0 <- sim(m, n = 2e5, p=unlist(par0))

lm(C_2 ~ L_1 + A_1, data = data0)
mean(data0$C_2)

# transform
m_opt <- m
transform(m_opt, A_1 ~ C_1 + L_1) <- function(x) d_opt_1(C_1 = x[1], L_1 = x[2])
transform(m_opt, A_2 ~ C_2) <- function(x) d_opt_2(C_2 = x[1])

data_opt0 <- sim(m_opt, n = 2e5, p = unlist(par0))

lm(C_2 ~ L_1 + A_1, data = data_opt0) # incorrect
mean(data_opt0$C_2)
