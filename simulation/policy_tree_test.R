# TODO implement a Q-function based on regression trees. The multi_causal_forest predict function returns the treatment effect estimates and NOT the Q-function values.

# Note that predict.regression_forest does not behave as expected. If newdata is not provided it returns the out-of-bag predictions. Thus predict(object) and predict(object, newdata = X) will not agree.

# Multi-action treatment effect estimation
n <- 250
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- sample(c("A", "B", "C"), n, replace = TRUE)
Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
multi.forest <- multi_causal_forest(X = X, Y = Y, W = W, seed = 1)

# tau.hats, treatment effects for each type of treatment (compared to the remaining treatments)
# tau(X) = E[Y(1) - Y(0)|X = X]
# these are the out of bag predictions
predict(multi.forest)$predictions

# doubly robust scores for 1,...,d actions: Gamma_i = (Y_i - mu^{-k(i)}_{A_i}(X_i)) / e_{A_i}^{-k(i)}(X_i) * A_i + [mu_{a^1}^{-k(i)}(X_i) , ..., mu_{a^d}^{-k(i)}(X_i)]^T
# See Zhou et al (2018)
Gamma.matrix <- double_robust_scores(multi.forest)
policytree:::double_robust_scores.multi_causal_forest
# Gamma.matrix: (YY - mu.matrix) * IPW + mu.matrix
# mu.matrix <- conditional_means(object, ...)
policytree:::conditional_means.multi_causal_forest
# mu.matrix <- object$Y.hat + (1 - object$W.hat) * tau.hat
# Y.hat: E[Y|X]
# Y.hat <- predict(forest.Y)$predictions
# tau.hat <- predict(object, ...)$predictions
# tau.hat_k: (mu_k - Y.hat) / (1 - e_k)

g_regression_tree <- function(
  A,
  X,
  num.trees = 2000,
  sample.weights = NULL,
  clusters = NULL,
  equalize.cluster.weights = FALSE,
  sample.fraction = 0.5,
  mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
  honesty.prune.leaves = TRUE,
  alpha = 0.05,
  imbalance.penalty = 0,
  tune.parameters = "none",
  num.threads = NULL,
  seed = runif(1, 0, .Machine$integer.max)
){

  # from multi_causal_forest
  treatments <- sort(unique(A))
  treatment.names <- as.character(treatments)
  n.treatments <- length(treatments)

  args.orthog <- list(
    X = X,
    num.trees = max(50, num.trees/4),
    sample.weights = sample.weights,
    clusters = clusters,
    equalize.cluster.weights = equalize.cluster.weights,
    sample.fraction = sample.fraction,
    mtry = mtry,
    min.node.size = 5,
    honesty = TRUE,
    honesty.fraction = 0.5,
    honesty.prune.leaves = honesty.prune.leaves,
    alpha = alpha,
    imbalance.penalty = imbalance.penalty,
    ci.group.size = 1,
    tune.parameters = tune.parameters,
    num.threads = num.threads,
    seed = seed)

  A.onevsall <- sapply(treatments, function(treatment) as.integer(A == treatment))
  A.forest <- apply(A.onevsall, 2, function(Ai) do.call(grf::regression_forest, c(Y = list(Ai), args.orthog)))
  A.hat <- sapply(A.forest, function(af) predict(af)$predictions)

  A.hat <- sweep(A.hat, 1, rowSums(A.hat), `/`)
  colnames(A.hat) <- treatment.names

  rtm <- list(
    A.forest = A.forest,
    A.onevsall = A.onevsall,
    A.hat = A.hat
  )

  class(rtm) <- "g_regression_tree"
  return(rtm)
}

predict.g_regression_tree <- function(object, new_X, type = "probs", action_set){
  A.forest <- object$A.forest

  A.hat <- sapply(A.forest, function(af) predict(af, newdata = new_X)$predictions)
  A.hat <- sweep(A.hat, 1, rowSums(A.hat), `/`)

  return(A.hat)
}

# tmp_grtm <- g_regression_tree(A = W, X = X, seed = 1)
# all(multi.forest$W.onevsall == tmp_grtm$A.onevsall)
# all(multi.forest$W.hat == tmp_grtm$A.hat) # remember that the seed has to be the same


# q_regression_tree <- function(V_res, A, X, ...){
#
#   mcf_model <- multi_causal_forest(X = X, W = A, Y = V_res, ...)
#
#   out <- list(
#     mcf_model = mcf_model
#   )
#
#   class(out) <- "q_regression_tree"
#
#   return(out)
# }
#
# predict.q_regression_tree <- function(object, new_X, action_set){
#
# }




