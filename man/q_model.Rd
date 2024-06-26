% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q_models.R
\docType{class}
\name{q_model}
\alias{q_model}
\alias{q_glm}
\alias{q_glmnet}
\alias{q_rf}
\alias{q_sl}
\alias{q_xgboost}
\title{q_model class object}
\usage{
q_glm(
  formula = ~A * .,
  family = gaussian(),
  model = FALSE,
  na.action = na.pass,
  ...
)

q_glmnet(
  formula = ~A * .,
  family = "gaussian",
  alpha = 1,
  s = "lambda.min",
  ...
)

q_rf(
  formula = ~.,
  num.trees = c(250, 500, 750),
  mtry = NULL,
  cv_args = list(nfolds = 3, rep = 1),
  ...
)

q_sl(
  formula = ~.,
  SL.library = c("SL.mean", "SL.glm"),
  env = as.environment("package:SuperLearner"),
  onlySL = TRUE,
  discreteSL = FALSE,
  ...
)

q_xgboost(
  formula = ~.,
  objective = "reg:squarederror",
  params = list(),
  nrounds,
  max_depth = 6,
  eta = 0.3,
  nthread = 1,
  cv_args = list(nfolds = 3, rep = 1)
)
}
\arguments{
\item{formula}{An object of class \link{formula} specifying the design matrix for
the outcome regression model/Q-model at the given stage. The action at the
given stage is always denoted 'A', see examples. Use
\code{\link[=get_history_names]{get_history_names()}} to see the additional
available variable names.}

\item{family}{A description of the error distribution and link function to
be used in the model.}

\item{model}{(Only used by \code{q_glm}) If \code{FALSE} model frame will
not be saved.}

\item{na.action}{(Only used by \code{q_glm}) A function which indicates what
should happen when the data contain NAs, see \link{na.pass}.}

\item{...}{Additional arguments passed to \code{\link[=glm]{glm()}}, \link[glmnet:glmnet]{glmnet::glmnet},
\link[ranger:ranger]{ranger::ranger} or \link[SuperLearner:SuperLearner]{SuperLearner::SuperLearner}.}

\item{alpha}{(Only used by \code{q_glmnet}) The elasticnet mixing parameter
between 0 and 1. alpha equal to 1 is the lasso penalty, and alpha equal
to 0 the ridge penalty.}

\item{s}{(Only used by \code{q_glmnet}) Value(s) of the penalty parameter
lambda at which predictions are required, see \code{\link[glmnet:predict.glmnet]{glmnet::predict.glmnet()}}.}

\item{num.trees}{(Only used by \code{q_rf}) Number of trees.}

\item{mtry}{(Only used by \code{q_rf}) Number of variables to possibly split
at in each node.}

\item{cv_args}{(Only used by \code{q_rf}) Cross-validation parameters.
Only used if multiple hyper-parameters are given. \code{K} is the number
of folds and
\code{rep} is the number of replications.}

\item{SL.library}{(Only used by \code{q_sl}) Either a character vector of
prediction algorithms or a list containing character vectors,
see \link[SuperLearner:SuperLearner]{SuperLearner::SuperLearner}.}

\item{env}{(Only used by \code{q_sl}) Environment containing the learner
functions. Defaults to the calling environment.}

\item{onlySL}{(Only used by \code{q_sl}) Logical. If TRUE, only saves and computes predictions
for algorithms with non-zero coefficients in the super learner object.}

\item{discreteSL}{(Only used by \code{q_sl}) If TRUE, select the model with
the lowest cross-validated risk.}

\item{objective}{(Only used by \code{q_xgboost}) specify the learning
task and the corresponding learning objective, see \link[xgboost:xgb.train]{xgboost::xgboost}.}

\item{params}{(Only used by \code{q_xgboost}) list of parameters.}

\item{nrounds}{(Only used by \code{q_xgboost}) max number of boosting iterations.}

\item{max_depth}{(Only used by \code{q_xgboost}) maximum depth of a tree.}

\item{eta}{(Only used by \code{q_xgboost}) learning rate.}

\item{nthread}{(Only used by \code{q_xgboost}) number of threads.}
}
\value{
q_model object: function with arguments 'AH'
(combined action and history matrix) and 'V_res' (residual value/expected
utility).
}
\description{
Use \code{q_glm()}, \code{q_glmnet()}, \code{q_rf()}, and \code{q_sl()} to construct
an outcome regression model/Q-model object.
The constructors are used as input for \code{\link[=policy_eval]{policy_eval()}} and \code{\link[=policy_learn]{policy_learn()}}.
}
\details{
\code{q_glm()} is a wrapper of \code{\link[=glm]{glm()}} (generalized linear model).\cr
\code{q_glmnet()} is a wrapper of \code{\link[glmnet:glmnet]{glmnet::glmnet()}} (generalized linear model via
penalized maximum likelihood).\cr
\code{q_rf()} is a wrapper of \code{\link[ranger:ranger]{ranger::ranger()}} (random forest).
When multiple hyper-parameters are given, the
model with the lowest cross-validation error is selected.\cr
\code{q_sl()} is a wrapper of \link[SuperLearner:SuperLearner]{SuperLearner::SuperLearner} (ensemble model).
\code{q_xgboost()} is a wrapper of \link[xgboost:xgb.train]{xgboost::xgboost}.
}
\examples{
library("polle")
### Single stage case
d1 <- sim_single_stage(5e2, seed=1)
pd1 <- policy_data(d1,
                   action="A",
                   covariates=list("Z", "B", "L"),
                   utility="U")
pd1

# available history variable names for the outcome regression:
get_history_names(pd1)

# evaluating the static policy a=1 using inverse
# propensity weighting based on the given Q-model:
pe1 <- policy_eval(type = "or",
                   policy_data = pd1,
                   policy = policy_def(1, name = "A=1"),
                   q_model = q_glm(formula = ~A*.))
pe1

# getting the fitted Q-function values
head(predict(get_q_functions(pe1), pd1))

### Two stages:
d2 <- sim_two_stage(5e2, seed=1)
pd2 <- policy_data(d2,
                  action = c("A_1", "A_2"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))
pd2

# available full history variable names at each stage:
get_history_names(pd2, stage = 1)
get_history_names(pd2, stage = 2)

# evaluating the static policy a=1 using outcome
# regression based on a glm model for each stage:
pe2 <- policy_eval(type = "or",
            policy_data = pd2,
            policy = policy_def(1, reuse = TRUE, name = "A=1"),
            q_model = list(q_glm(~ A * L_1),
                           q_glm(~ A * (L_1 + L_2))),
            q_full_history = TRUE)
pe2

# getting the fitted Q-function values
head(predict(get_q_functions(pe2), pd2))
}
\seealso{
\code{\link[=get_history_names]{get_history_names()}}, \code{\link[=get_q_functions]{get_q_functions()}}.
}
