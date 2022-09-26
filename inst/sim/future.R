library("polle")
library("future.apply")

source(system.file("sim", "single_stage.R", package="polle"))

par0 <- c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, s = 1)
d <- sim_single_stage(5e2, seed=1, par=par0)
head(d, 5)

d
pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
pd

strip_names <- c("strip_glm")

SL.glm_stripped <- function (Y, X, newX, family, obsWeights, model = TRUE, ...)
{
  strip_glm <- function(cm) {
    cm$y = c()
    cm$model = c()

    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()

    attr(cm$terms, ".Environment") = c() # saveRDS saves the objects in environment when glm was called
    attr(cm$formula, ".Environment") = c()

    return(cm)
  }

  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.glm <- suppressWarnings(glm(Y ~ ., data = X, family = family, weights = obsWeights,
                                  model = model))
  # stripping model
  fit.glm <- strip_glm(fit.glm)

  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL_names <- c("SL.glm_stripped")

library(SuperLearner)
random_forest_tune <- list(
  mtry = c(2),
  num.trees = c(500)
)
random_forest_learner <- create.Learner("SL.ranger", tune = random_forest_tune)

g_model_learner_names <-c(
  "SL.mean",
  "SL.glm_stripped",
  random_forest_learner$names
)
q_model_learner_names <-c(
  "SL.mean",
  "SL.glm_stripped",
  random_forest_learner$names
)

plan(list(
  tweak(multisession, workers = 2),
  tweak(multisession, workers = 2)
))
ptl_eval <- policy_eval(
  pd,
  type = "dr",
  cross_fit = TRUE,
  policy_learn = policy_learn(
    type = "ptl",
    alpha = 0.05,
    policy_vars = c("Z", "B", "L"),
    full_history = FALSE,
    depth = 3,
    split.step = 1,
    L = NULL,
    hybrid = TRUE,
    future_args = list(
      future.seed = 1,
      future.packages = c("SuperLearner", "ranger")
      ,future.globals=c(q_model_learner_names, strip_names)
    )
  ),
  q_models = q_sl(SL.library = q_model_learner_names,
                  cvControl = list(V=2)),
  q_full_history = FALSE,
  g_models = g_sl(SL.library = g_model_learner_names,
                  cvControl = list(V=2)),
  g_full_history = FALSE,
  M = 2,
  future_args = list(
    future.seed = 1,
    future.packages = c("SuperLearner", "ranger"),
    future.globals=c(q_model_learner_names, strip_names)
  )
)

ptl_eval
