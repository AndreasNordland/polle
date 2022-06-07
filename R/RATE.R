##' Estimation of the Average Treatment Effect among Responders
##'
##' @title Responder Average Treatment Effect
##' @param response Response formula (e.g, Y~D*A)
##' @param post.treatment Post treatment marker formula (e.g., D~W)
##' @param treatment Treatment formula (e.g, A~1)
##' @param data data.frame
##' @param censoring Censoring formula (only for survival endpoints)
##' @param family Exponential family for response (default gaussian)
##' @param M Number of folds in cross-fitting (M=1 is no cross-fitting)
##' @param pr.treatment (optional) Randomization probability of treatment.
##' @param treatment.level Treatment level in binary treatment (default 1)
##' @param SL.args.response Arguments to SuperLearner for the response model
##' @param SL.args.post.treatment Arguments to SuperLearner for the post treatment indicator
##' @param preprocess (optional) Data preprocessing function
##' @param efficient If TRUE, the estimate will be efficient. If FALSE, the estimate will be a simple plug-in estimate.
##' @param ... Additional arguments to lower level functions
##' @return estimate object
##' @author Andreas Nordland, Klaus K. Holst
RATE <- function(response, post.treatment, treatment,
                 data, censoring=NULL, family = gaussian(), M = 5,
                 pr.treatment, treatment.level,
                 SL.args.response = list(family = gaussian(),
                                         SL.library = c("SL.mean", "SL.glm")),
                 SL.args.post.treatment = list(family = binomial(),
                                               SL.library = c("SL.mean", "SL.glm")),
                 preprocess = NULL, efficient = TRUE, ...) {
  dots <- list(...)
  cl <- match.call()

  A <- get_response(treatment, data)
  A.levels <- sort(unique(A))
  if (length(A.levels)!=2) stop("Expected binary treatment variable")
  if (missing(treatment.level)) {
    treatment.level <- A.levels[2]
  }
  if (missing(pr.treatment)) {
    pr.treatment <- mean(A == treatment.level[1])
  }

  fit <- function(train_data, valid_data) {
    slfit <- get("q_sl", asNamespace("polle"))
    D.args <- c(list(formula = post.treatment), SL.args.post.treatment)
    D.fit <- do.call(slfit, D.args)
    Y.args <- c(list(formula = response), SL.args.response)
    Y.fit <- do.call(slfit, Y.args)
    if (!is.null(preprocess)) {
      train_data <- do.call(
        "preprocess",
        c(list(data = train_data, call = cl), dots)
      )
    }
    D.est <- D.fit(train_data)
    Y.est <- Y.fit(train_data)

    A <- as.numeric(get_response(treatment, valid_data) == treatment.level[1])
    D <- as.numeric(get_response(post.treatment, valid_data))
    Y <- get_response(response, valid_data)

    if (!is.null(preprocess)) {
      valid_data <- do.call(
        "preprocess",
        c(list(data = valid_data, call = cl), dots)
      )
    }
    valid_data[lava::getoutcome(treatment)] <- treatment.level[1]
    pr.Ya <- predict(Y.est, valid_data)
    pr.Da <- predict(D.est, valid_data)
    valid_data[lava::getoutcome(treatment)] <- setdiff(A.levels, treatment.level[1])
    pr.Y0 <- predict(Y.est, valid_data)

    phi.a <- A / pr.treatment * (Y - pr.Ya) + pr.Ya
    phi.0 <- (1-A) / (1 - pr.treatment) * (Y - pr.Y0) + pr.Y0

    phi.d <- A / pr.treatment * (D - pr.Da) + pr.Da

    phis <- list(a1 = phi.a, a0 = phi.0, d = phi.d)
    iids <- lapply(phis, function(x) x - mean(x))
    ests <- lapply(phis, mean)
    est <- with(ests, (a1 - a0) / d)
    iid <- 1 / ests$d * (with(iids, a1 - a0) - est * iids$d)
    return(list(estimate = est, iid = iid))
  }

  fit_plug_in <- function(){
    A <- get_response(treatment, data)
    D <- get_response(post.treatment, data)
    Y <- get_response(response, data)

    phi.1 <- A / pr.treatment * Y
    phi.0 <- (1-A) / (1 - pr.treatment) * Y
    phi.D <- A / pr.treatment * D
    phis <- list(a1 = phi.1, a0 = phi.0, d = phi.D)
    ests <- lapply(phis, mean)

    iids <- list(
      a1 = phi.1 - A / pr.treatment * mean(phi.1),
      a0 = phi.0 - (1-A) / (1 - pr.treatment) * mean(phi.0),
      d = phi.D - A / pr.treatment * mean(phi.D)
    )

    est <- with(ests, (a1 - a0) / d)
    iid <- 1 / ests$d * (with(iids, a1 - a0) - est * iids$d)
    return(list(estimate = est, iid = iid))
  }

  n <- nrow(data)
  est <- 0
  iid <- numeric(n)

  if(efficient == TRUE){
    if (M < 2) {
      est_f <- fit(data, data)
      iid <- est_f$iid
      est <- est_f$estimate
    } else {
      folds <- split(sample(1:n, n), rep(1:M, length.out = n))
      for (f in folds) {
        train_data <- data[-f, ]
        valid_data <- data[f, ]
        est_f <- fit(train_data, valid_data)
        iid[f] <- est_f$iid
        est <- est + length(f) / n * est_f$estimate
      }
    }
  } else{
    est_f <- fit_plug_in()
    iid <- est_f$iid
    est <- est_f$estimate
  }

  lava::estimate(NULL, coef = est, iid = cbind(iid), labels = "rate")
}
