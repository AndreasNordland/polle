check_actions <- function(actions, policy_data){
  ## checking the format of the actions data.table
  if (!is.data.table(actions))
    stop("actions must be a data.table.")
  if (!any("d" %in% colnames(actions)))
    stop("actions must have an action varible named 'd'.")
  if (!all(key(actions) == c("id", "stage")))
    stop("actions must have keys 'id' and 'stage'.")

  ## checking that the actions comply with the stage action sets
  K <- get_K(policy_data)
  stage_action_sets <- get_stage_action_sets(policy_data)
  stage <- NULL
  for(k_ in 1:K){
    if (!all(unlist(actions[stage == k_, "d"]) %in% stage_action_sets[[k_]])){
      mes <- "The policy actions does not comply with the stage action sets of the policy data object."
      stop(mes)
    }
  }
}

## internal: contrast matrix mapping the 4 per-subgroup potential outcome means
## of each policy to the 2 subgroup average treatment effects, i.e. for each
## policy block [E[U(a2)|d=a2], E[U(a1)|d=a2], E[U(a2)|d=a1], E[U(a1)|d=a1]] the
## two rows compute E[U(a2)|d=a2] - E[U(a1)|d=a2] and
## E[U(a2)|d=a1] - E[U(a1)|d=a1].
subgroup_contrast_matrix <- function(n_mean) {
  n_pol <- n_mean / 4L
  C <- matrix(0, nrow = 2L * n_pol, ncol = n_mean)
  for (k in seq_len(n_pol)) {
    col0 <- 4L * (k - 1L)
    row0 <- 2L * (k - 1L)
    C[row0 + 1L, col0 + 1L] <- 1
    C[row0 + 1L, col0 + 2L] <- -1
    C[row0 + 2L, col0 + 3L] <- 1
    C[row0 + 2L, col0 + 4L] <- -1
  }
  return(C)
}

## internal: extract the coefficients, influence curve, variance and labels of a
## policy_eval object, applying the subgroup contrast when requested. For
## target = "subgroup" the object stores the 4 per-subgroup potential outcome
## means per policy; with contrast = TRUE these are collapsed to the 2 subgroup
## average treatment effects (the quantities reported by default). For
## target = "value" the coefficients are returned as stored.
policy_eval_parts <- function(object, contrast = TRUE) {
  target <- get_element(object, "target")
  coef <- get_element(object, "coef")
  IC <- get_element(object, "IC", check_name = FALSE)
  vcov <- get_element(object, "vcov", check_name = FALSE)
  ## normalise a stored diagonal vcov vector to a matrix:
  if (!is.null(vcov) && !is.matrix(vcov)) {
    vcov <- diag(vcov, nrow = length(vcov))
  }
  labels <- get_element(object, "name", check_name = FALSE)

  if (identical(target, "subgroup") && isTRUE(contrast)) {
    ## the contrast matrix pairs adjacent means (E[U(a2)|d=k] - E[U(a1)|d=k]),
    ## so compute the contrasts by direct differencing of the paired columns
    ## rather than via C %*% coef. A matrix product would propagate the NA of an
    ## empty/below-minimum subgroup into the other contrasts because 0 * NA = NA
    ## in R.
    odd <- seq.int(1L, length(coef), by = 2L)
    coef <- coef[odd] - coef[odd + 1L]
    if (!is.null(IC)) {
      IC <- IC[, odd, drop = FALSE] - IC[, odd + 1L, drop = FALSE]
    }
    if (!is.null(vcov)) {
      C <- subgroup_contrast_matrix(nrow(vcov))
      vcov <- C %*% vcov %*% t(C)
    }
    labels <- get_element(object, "contrast_name", check_name = FALSE)
  }
  names(coef) <- labels

  return(list(coef = coef, IC = IC, vcov = vcov, labels = labels))
}

#' @rdname policy_eval
#' @export
coef.policy_eval <- function(object, contrast = TRUE, ...) {
  return(policy_eval_parts(object, contrast = contrast)[["coef"]])
}

#' @rdname policy_eval
#' @export
IC.policy_eval <- function(x, contrast = TRUE, ...) {
  ic <- policy_eval_parts(x, contrast = contrast)[["IC"]]
  if (is.null(ic)) {
    return(NULL)
  }
  return(cbind(ic))
}

#' @rdname policy_eval
#' @export
vcov.policy_eval <- function(object, contrast = TRUE, ...) {
  parts <- policy_eval_parts(object, contrast = contrast)
  ic <- parts[["IC"]]
  if (!is.null(ic)) {
    n <- nrow(ic)
    return(crossprod(ic) / (n * n))
  }
  return(parts[["vcov"]])
}

#' @rdname policy_eval
#' @export
print.policy_eval <- function(x,
                              digits = 4L,
                              width = 35L,
                              std.error = TRUE,
                              p.value = TRUE,
                              ...) {
  est <- estimate(x, ...)
  print(
    est,
    digits = digits,
    width = width,
    std.error = std.error,
    p.value = p.value,
    ...
  )
}

#' @rdname policy_eval
#' @export
summary.policy_eval <- function(object, contrast = TRUE, labels = NULL, ...) {
  parts <- policy_eval_parts(object, contrast = contrast)
  if (is.null(labels)) {
    labels <- parts[["labels"]]
    if (is.null(labels)) {
      p <- length(parts[["coef"]])
      target <- get_element(object, "target")
      if (p == 1) {
        labels <- target
      } else {
        labels <- paste0(target, seq(p))
      }
    }
  }
  ic <- parts[["IC"]]
  ## the additional arguments (...) are deliberately not forwarded to
  ## lava::estimate(): the returned estimate object can be updated in a
  ## subsequent step (e.g. to change the confidence level). This also avoids
  ## passing the 'contrast' argument on to lava::estimate().
  if (is.null(ic)) {
    est <- lava::estimate(
      NULL,
      coef = parts[["coef"]],
      vcov = parts[["vcov"]],
      labels = labels
    )
  } else {
    est <- lava::estimate(
      NULL,
      coef = parts[["coef"]],
      IC = ic,
      labels = labels
    )
  }
  return(est)
}

#' @rdname policy_eval
#' @export
estimate.policy_eval <- function(x, labels = NULL, contrast = TRUE, ...) {
  return(summary(x, contrast = contrast, labels = labels, ...))
}

#' @rdname policy_eval
#' @export
"merge.policy_eval" <- function(x, y, ..., paired = TRUE) {
  dots <- list(...)
  idx <- names(dots) %in% formalArgs(lava::estimate.default)[-1]
  est_args <- list()
  if (length(idx)>0) {
    est_args <- dots[which(idx)]
    dots <- dots[-which(idx)]
  }
  m <- lapply(c(list(x, y), dots), function(p)
    do.call(estimate, c(list(p),est_args)))
  m <- do.call("merge", c(m, list(paired=paired)))
  return(m)
}

#' @rdname policy_eval
#' @export
"+.policy_eval" <- function(x,...) {
  merge(x, ...)
}

#' @title Conditional Policy Evaluation
#' @description \code{conditional()} is used to calculate the
#' policy value for each group defined by a given baseline variable.
#' @param object Policy evaluation object created by [policy_eval()].
#' @param policy_data Policy data object created by [policy_data()].
#' @param baseline Character string.
#' @returns object of inherited class 'estimate', see [lava::estimate.default].
#' The object is a list with elements 'coef' (policy value estimate for each
#' group) and 'IC' (influence curve estimate matrix).
#' @examples
#' library("polle")
#' library("data.table")
#' setDTthreads(1)
#' d <- sim_single_stage(n=2e3)
#' pd <- policy_data(d,
#'                   action = "A",
#'                   baseline = c("B"),
#'                   covariates = c("Z","L"),
#'                   utility = "U")
#'
#' # static policy:
#' p <- policy_def(1)
#'
#' pe <- policy_eval(pd,
#'                   policy = p)
#'
#' # conditional value for each group defined by B
#' conditional(pe, pd, "B")
#' @export
conditional <- function(object, policy_data, baseline)
  UseMethod("conditional")

#' @export
conditional.policy_eval <- function(object, policy_data, baseline) {
  policy_eval <- object

  if (!inherits(policy_eval, "policy_eval")) {
    stop("policy_eval must be of inherited class 'policy_eval'.")
  }
  if (!inherits(policy_data, "policy_data")) {
    stop("policy_data must be of inherited class 'policy_data'.")
  }
  if (!is.character(baseline) || length(baseline) != 1) {
    stop("baseline must be a single character.")
  }
  if (get_element(policy_eval, "target") != "value") {
    stop("only implemented for target = 'value'.")
  }

  baseline_data <- policy_data[["baseline_data"]]

  # checking IDs
  check <- all.equal(
    policy_eval[["id"]],
    baseline_data[["id"]]
  )
  if (!check) {
    stop("ID's does not match.")
  }

  # getting the doubly robust score:
  z <- IC(policy_eval) + coef(policy_eval)

  by <- baseline_data[, baseline, with = FALSE]
  agg <- aggregate(z, by = by, mean)
  coef <- agg[["V1"]]

  n <- get_n(policy_data)
  groups <- agg[[baseline]]
  IC <- matrix(0, nrow = nrow(baseline_data), ncol = length(groups))
  for (j in seq_along(coef)) {
    idx <- baseline_data[[baseline]] == groups[j]
    ic <- z[idx, ] - coef[j]
    IC[idx, j] <- ic / sum(idx) * n
  }
  est <- estimate(NULL,
    coef = coef,
    IC = cbind(IC),
    id = baseline_data[["id"]],
    labels = paste(baseline, groups, sep = ":")
  )
  return(est)
}

#' @export
get_g_functions.policy_eval <- function(object){
  getElement(object, "g_functions")
}

#' @export
get_q_functions.policy_eval <- function(object){
  getElement(object, "q_functions")
}

#' @export
get_policy.policy_eval <- function(object){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy(po)
  return(pf)
}

#' @title Get Policy Actions
#'
#' @description \code{get_policy_actions()} extract the actions dictated by the
#' (learned and possibly cross-fitted) policy a every stage.
#' @param object Object of class [policy_eval].
#' @returns [data.table::data.table] with keys \code{id} and \code{stage} and action variable
#' \code{d}.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # defining a policy learner based on cross-fitted doubly robust Q-learning:
#' pl <- policy_learn(type = "drql",
#'                    control = control_drql(qv_models = list(q_glm(~C_1), q_glm(~C_1+C_2))),
#'                    full_history = TRUE,
#'                    L = 2) # number of folds for cross-fitting
#'
#' # evaluating the policy learner using 2-fold cross fitting:
#' pe <- policy_eval(type = "dr",
#'                    policy_data = pd,
#'                    policy_learn = pl,
#'                    q_models = q_glm(),
#'                    g_models = g_glm(),
#'                    M = 2) # number of folds for cross-fitting
#'
#' # Getting the cross-fitted actions dictated by the fitted policy:
#' head(get_policy_actions(pe))
#' @export
get_policy_actions <- function(object)
  UseMethod("get_policy_actions")

#' @export
get_policy_actions.policy_eval <- function(object){
  getElement(object, "policy_actions")
}

#' @export
get_policy_functions.policy_eval <- function(object, stage, ...){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy_functions(po, stage = stage, ...)
  return(pf)
}

#' @title Plot histogram of the influence curve for a \code{policy_eval} object
#'
#' @param x Object of class [policy_eval]
#' @param ... Additional arguments
#' @examples
#' d <- sim_two_stage(2e3, seed=1)
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = "BB",
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#'
#' pe <- policy_eval(pd,
#'                   policy_learn = policy_learn())
#'
#' plot(pe)
#' @export
plot.policy_eval <- function(x, ...){
    ic <- IC(x)
    se <- sqrt(mean(ic^2))
    graphics::hist(ic,
                   xlab = "IC",
                   main = "Histogram of Influence Curve Terms",
                   prob = TRUE,
                   ...)
    graphics::curve(stats::dnorm(x, mean=0, sd=se),
                    add = TRUE,
                    lwd = 2,
                    col = "red")
    grDevices::dev.flush()

  invisible()
}
