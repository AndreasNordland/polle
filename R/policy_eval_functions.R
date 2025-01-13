check_actions <- function(actions, policy_data){
  # checking the format of the actions data.table
  if (!is.data.table(actions))
    stop("actions must be a data.table.")
  if (!any("d" %in% colnames(actions)))
    stop("actions must have an action varible named 'd'.")
  if (!all(key(actions) == c("id", "stage")))
    stop("actions must have keys 'id' and 'stage'.")

  # checking that the actions comply with the stage action sets
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

#' @rdname policy_eval
#' @export
coef.policy_eval <- function(object, ...) {
  return(get_element(object, "coef"))
}

#' @rdname policy_eval
#' @export
IC.policy_eval <- function(x, ...) {
  res <- cbind(get_element(x, "IC", check_name = FALSE))
  return(res)
}

#' @rdname policy_eval
#' @export
vcov.policy_eval <- function(object, ...) {
  ic <- IC(object)
  if (is.null(ic))
    return(NULL)
  n <- nrow(ic)
  return(crossprod(ic)/(n*n))
}

#' @rdname policy_eval
#' @export
print.policy_eval <- function(x,
                              digits = 4L,
                              width = 35L,
                              std.error = TRUE,
                              level = 0.95,
                              p.value = TRUE,
                              ...) {
  est <- estimate(x, level = level, ...)
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
summary.policy_eval <- function(object, ...) {
  lava::estimate(object, ...)
}

#' @rdname policy_eval
#' @export
estimate.policy_eval <- function(x,
                                 labels = get_element(x,
                                   "name",
                                   check_name = FALSE
                                 ),
                                 level = 0.95,
                                 ...) {
  p <- length(coef(x))
  if (is.null(labels)) {
    target <- get_element(x, "target")
    if (p == 1) {
      labels <- target
    } else {
      labels <- paste0(target, seq(p))
    }
  }
  ic <- IC(x)
  if (is.null(ic)) {
    est <- lava::estimate(
      NULL,
      coef = coef(x),
      vcov = NULL,
      labels = labels,
      ...
    )
  } else {
    if (!is.null(x$variance_type) &&
        x$variance_type == "mean") {
      V <- Reduce(`+`, lapply(pe2$IC, lava::var_ic))
      V <- V / length(pe2$IC)
      est <- lava::estimate(
        NULL,
        coef = coef(x),
        vcov = V,
        labels = labels,
        level = level,
        ...
      )
    } else {
      est <- lava::estimate(
        NULL,
        coef = coef(x),
        IC = ic,
        labels = labels,
        level = level,
        ...
      )
    }
  }
  return(est)
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
