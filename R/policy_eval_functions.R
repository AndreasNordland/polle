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
  return(object$value_estimate)
}

#' @rdname policy_eval
#' @export
IC.policy_eval <- function(x, ...) {
  res <- cbind(getElement(x, "IC"))
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
print.policy_eval <- function(x, ...) {
  print(summary(x, ...))
}


#' @rdname policy_eval
#' @export
summary.policy_eval <- function(object, ...){
  lava::estimate(object, ...)
}

#' @rdname policy_eval
#' @export
estimate.policy_eval <- function(x, ..., labels=x$name) {
  p <- length(coef(x))
  if (is.null(labels)) {
    if (p==1) {
      "value"
    } else {
      labels <- paste0("value", seq(p))
    }
  }
  ic <- IC(x)
  if (is.null(ic)){
    est <- lava::estimate(NULL, coef=coef(x), vcov=NULL, ...) # labels=labels
  }
  else
    est <- lava::estimate(NULL, coef=coef(x), IC=ic, labels=labels, ...)
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
#' pe
#'
#' # conditional value for each group defined by B
#' conditional(pe, pd, "B")
#' @export
conditional <- function(object, policy_data, baseline)
  UseMethod("conditional")

#' @export
conditional.policy_eval <- function(object, policy_data, baseline){
  policy_eval <- object

  if (!inherits(policy_eval, "policy_eval"))
    stop("policy_eval must be of inherited class 'policy_eval'.")
  if (!inherits(policy_data, "policy_data"))
    stop("policy_data must be of inherited class 'policy_data'.")
  if (!is.character(baseline) | length(baseline)!= 1)
    stop("baseline must be a single character.")

  baseline_data <- policy_data[["baseline_data"]]

  # checking IDs
  check <- all.equal(
    policy_eval[["id"]],
    baseline_data[["id"]]
  )
  if (!check)
    stop("ID's does not match.")

  # getting the doubly robust score:
  z <- IC(policy_eval) + coef(policy_eval)

  by <- baseline_data[, baseline, with = FALSE]
  agg <- aggregate(z, by = by, mean)
  coef <- agg[["V1"]]

  groups <- agg[[baseline]]
  IC <- matrix(0, nrow = nrow(baseline_data), ncol = length(groups))
  for (j in seq_along(coef)){
    idx <- baseline_data[[baseline]] == groups[j]
    id <- baseline_data[["id"]][idx]
    ic <- z[idx,] - coef[j]
    IC[idx,j] <- ic
  }
  est <- estimate(NULL,
                  coef = coef,
                  IC = cbind(IC),
                  id = baseline_data[["id"]],
                  labels = paste(baseline, groups, sep = ":"))
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
#' @returns [data.table] with keys \code{id} and \code{stage} and action variable
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
get_policy_functions.policy_eval <- function(object, stage){
  po <- getElement(object, "policy_object")
  if (is.null(po)){
    mes <- "Learned policy is not available."
    stop(mes)
  }
  pf <- get_policy_functions(po, stage = stage)
  return(pf)
}

#' @title Plot Diagnostics for a \code{policy_eval} Object
#'
#' @param x Object of class [policy_eval]
#' @param which A subset of the numbers 1:2
#' \itemize{
#'  \item{1} Histogram of the influence curve terms
#'  \item{2} Plot of the policy actions
#' }
#' @param policy_data Object of class [policy_data]
#' @param stage Stage number for plot 2
#' @param history_variables character vector of length 2 for plot 2
#' @param ... Additional arguments passed to [hist()] and [plot()]
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
#' plot(pe,
#'      which = c(1,2),
#'      policy_data = pd,
#'      stage = 1,
#'      history_variables = c("C", "L"))
#' @export
plot.policy_eval <- function(x,
                             which = c(1),
                             policy_data = NULL,
                             stage = 1,
                             history_variables = NULL,
                             ...){

  if (!is.numeric(which) || any(which < 1) || any(which > 2))
    stop("'which' must be in 1:2")
  show <- rep(FALSE, 2)
  show[which] <- TRUE

  if (!is.null(stage)){
    if (!(is.numeric(stage) & (length(stage) == 1)))
      stop("stage must be an integer greater than 0.")
    if (!(stage %% 1 == 0))
      stop("stage must be an integer greater than 0.")
    if (stage<=0)
      stop("stage must be an integer greater than 0.")
  }
  stage_ <- stage

  if (show[1L]) {
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
  }
  if (show[2L]){
    if (is.null(policy_data) | !inherits(policy_data, "policy_data"))
      stop("For plot 2 please provide policy_data.")
    K <- get_K(policy_data)
    if (!(stage_ <= K))
      stop("stage must be lower than or equal to the maximal number of stages.")
    if (is.null(history_variables))
      stop("For plot 2 please provide history_variables (character vector of length 2).")
    if (!(is.character(history_variables) & length(history_variables)==2))
      stop("For plot 2 please provide history_variables (character vector of length 2).")


    if (!all.equal(x[["id"]], get_id(policy_data)))
      stop("policy_eval and policy_data IDs does not match.")

    plot_data <- data.table::merge.data.table(
      get_policy_actions(x)[stage ==stage_],
      get_history(policy_data)[["H"]][stage ==stage_]
    )

    if (!all(history_variables %in% colnames(plot_data)))
      stop("Invalid history_variables.")

    d <- as.factor(plot_data[["d"]])
    main <- paste("Plot of policy actions at stage ", stage_, sep = "")
    xx <- plot_data[[history_variables[1]]]
    if (is.character(xx))
      xx <- factor(xx, levels = sort(unique(xx)))
    yy <- plot_data[[history_variables[2]]]
    if (is.character(yy))
      yy <- factor(yy, levels = sort(unique(yy)))
    graphics::plot.default(
      x = xx,
      xlab = history_variables[1],
      xaxt = 'n',
      y = yy,
      yaxt = 'n',
      ylab = history_variables[2],
      main = main,
      col=d,
      ...)
    graphics::legend('topright', legend = levels(d), col = 1:8, cex = 0.8, pch = 1)
    if (is.factor(xx)){
      graphics::axis(1, at=(1:length(levels(xx))),labels=levels(xx))
    } else {
      graphics::axis(1)
    }
    if (is.factor(yy)){
      graphics::axis(2, at=(1:length(levels(yy))),labels=levels(yy))
    } else {
      graphics::axis(2)
    }


    grDevices::dev.flush()
  }

  invisible()
}
