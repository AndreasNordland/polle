
# data model --------------------------------------------------------------

library("lava")
m <- lvm()
parameter(m) <- ~ a + b + c + d + k
distribution(m, ~Z+L+B+A) <- list(uniform.lvm(),
                                  uniform.lvm(),
                                  binomial.lvm(p=.3),
                                  binomial.lvm("logit"))
regression(m, ~A) <-
  function(Z, L, B, k, d)  k*(Z + L - 1) + d*(B==1)
regression(m, ~U) <-
  function(Z, L, A, a, b, c)  Z + L + A*(c*Z + a*L + b)

par0 <- c(k = .3,  d = .5, a = 1, b = -2.5, c = 3)
d <- sim(m, 2e4, seed =1, p=par0) # seed 3

head(d, 3)


# Policy data -------------------------------------------------------------

library("polle")
pd <- policy_data(d, action="A", covariates=list("Z","B","L"), utility="U")
pd


# Policy ------------------------------------------------------------------

p1 <- policy_def(static_policy(1), replicate=TRUE)


# Policy Evaluation -------------------------------------------------------

vp_glm <- policy_eval(
  pd,
  policy=p1,
  g_models = g_glm(),
  q_models = q_glm(),
)

vp_rf <- policy_eval(
  pd,
  policy=p1,
  g_models = g_rf(seed=1),
  q_models = q_rf(seed=1),
)

vp_rf_cv <- policy_eval(
  pd,
  policy=p1,
  g_models = g_rf(seed=1, num.trees = 500, mtry = 3),
  q_models = q_rf(seed=1, num.trees = 500, mtry = 3),
  type = "cv"
  )

vp_glm
vp_rf
vp_rf_cv

# treatment effect of -2.5
library(tmle)
tmle_glm <- tmle(Y = utility(pd)$U,
     A = as.numeric(get_actions(pd)$A),
     W = get_H(state_history(pd)),
     family = "gaussian",
     Qform = Y ~ A + Z + B + L,
     gform = A ~ Z + B + L
     )

tmle_glm$estimates$ATE$psi
vp_glm

library(SuperLearner)
sl_rf <- create.Learner("SL.ranger", params = list(num.trees = 500, mtry = 3))

tmle_rf <- tmle(Y = utility(pd)$U,
                A = as.numeric(get_actions(pd)$A),
                W = get_H(state_history(pd)),
                family = "gaussian",
                Q.SL.library = sl_rf$names,
                g.SL.library = sl_rf$names,
                V = 2)

tmle_rf$estimates$ATE$psi
vp_rf
vp_rf_cv

#sqrt(tmle_rf$estimates$ATE$var.psi)
