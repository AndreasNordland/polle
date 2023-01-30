library("polle")
library("future.apply")
library("SuperLearner")

d <- sim_single_stage(5e2, seed=1)

pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
pd

random_forest_tune <- list(
  mtry = c(2),
  num.trees = c(500)
)
random_forest_learner <- create.Learner("SL.ranger", tune = random_forest_tune)

model_learner_names <-c(
  "SL.mean",
  random_forest_learner$names
)

plan(list(
  tweak(multisession, workers = 2),
  tweak(multisession, workers = 2)
))

ptl_eval <- policy_eval(
  pd,
  type = "dr",
  policy_learn = policy_learn(
    type = "ptl",
    alpha = 0.05,
    full_history = FALSE,
    L = 2,
    control = control_ptl(
      policy_vars = c("Z", "B", "L"),
      depth = 3,
      split.step = 1,
      hybrid = TRUE,
    ),
    future_args = list(
      future.seed = 1,
      future.globals=c(model_learner_names)
    )
  ),
  q_models = q_sl(SL.library = model_learner_names,
                  cvControl = list(V=2)),
  q_full_history = FALSE,
  g_models = g_sl(SL.library = model_learner_names,
                  cvControl = list(V=2)),
  g_full_history = FALSE,
  M = 2,
  future_args = list(
    future.seed = 1,
    future.globals=c(model_learner_names)
  )
)
ptl_eval
