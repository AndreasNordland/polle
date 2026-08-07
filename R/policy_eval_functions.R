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

## internal: construct an lava::estimate object from policy_eval object
## For target = "subgroup" the object stores the 4 per-subgroup potential outcome
## means per policy; with contrast = TRUE these are collapsed to the 2 subgroup
## average treatment effects (the quantities reported by default). For
## target = "value" the coefficients are returned as stored.
policy_eval_estimate <- function(object, contrast) {
  target <- get_element(object, "target")
  coef <- get_element(object, "coef")
  IC <- get_element(object, "IC", check_name = FALSE)
  labels <- get_element(object, "name", check_name = FALSE)
  id <- get_element(object, "id")

  if (identical(target, "subgroup") && isTRUE(contrast)) {
    odd <- seq.int(1L, length(coef), by = 2L)
    coef <- coef[odd] - coef[odd + 1L]
    if (!is.null(IC)) {
      IC <- IC[, odd, drop = FALSE] - IC[, odd + 1L, drop = FALSE]
    }
    labels <- get_element(object, "contrast_name", check_name = FALSE)
  }

  if (!is.null(IC)) {
    est <- lava::estimate(NULL,
                          coef = coef,
                          IC = IC,
                          labels = labels,
                          id = id)

  } else {
    est <- lava::estimate(NULL,
                          coef = coef,
                          vcov = NA,
                          labels = labels)
  }

  return(est)
}

## internal: build a tidy coef_table (data.table) summarising the estimates by
## policy, subgroup and (when available) threshold. For target = "subgroup" both
## the per-subgroup potential outcome means (contrast = FALSE) and the subgroup
## average treatment effects (contrast = TRUE) are stacked and distinguished by
## the logical `contrast` column. For target = "value" a single set of rows is
## produced. The columns are:
##   policy        - the policy label (e.g. "blip(eta=28)"); NA if unnamed.
##   threshold     - the numeric blip/ptl threshold; only populated when
##                   authoritative (i.e. a policy_object is stored, M = 1).
##   subgroup      - the subgroup the row refers to (e.g. "d=1"); NA for value.
##   term          - the estimated parameter label.
##   estimate      - the point estimate.
##   se            - the standard error (sqrt of the variance diagonal).
##   subgroup_prop - the proportion of observations in the subgroup; NA for
##                   value and when the subgroup indicator is unavailable
##                   (e.g. nrep > 1).
##   contrast      - FALSE for the per-subgroup means, TRUE for the subgroup
##                   average treatment effects.
policy_eval_table <- function(object, contrast) {
  target <- get_element(object, "target")
  meta <- get_element(object, "meta")
  est <- policy_eval_estimate(object = object, contrast = contrast)

  if (target == "value") {
    browser()
  } else if (target == "subgroup") {
    subgroup_indicator <- get_element(object, "subgroup_indicator")
    subgroup_proportion <- colMeans(subgroup_indicator)
    if (isTRUE(contrast)) {
      odd <- seq.int(1L, nrow(meta), by = 2L)
      meta <- meta[odd, ]
      meta$action <- NULL
      meta$subgroup <- NULL
    } else {
      subgroup_proportion <- rep(subgroup_proportion, each = 2)
    }
    tab <- meta
    tab <- cbind(data.table(name = names(coef(est)),
                            coef = coef (est)),
                 meta,
                 data.table(subgroup_proportion = subgroup_proportion))
    return(tab)
  }

  ## which contrast views to build:
  if (identical(target, "subgroup")) {
    views <- c(FALSE, TRUE)
  } else {
    views <- FALSE
  }

  ## authoritative thresholds (only available when a policy_object is stored,
  ## i.e. M = 1). A stored threshold of 0 is the "no threshold" sentinel:
  policy_object <- object[["policy_object"]]
  threshold <- policy_object[["threshold"]]
  if (!is.null(threshold) && all(threshold == 0)) {
    threshold <- NULL
  }

  ## per-policy subgroup indicator columns [d==a2, d==a1]; used for the subgroup
  ## proportion. NULL for target = "value" and for nrep > 1:
  subgroup_indicator <- object[["subgroup_indicator"]]
  subgroup_prop_cols <- NULL
  if (!is.null(subgroup_indicator)) {
    subgroup_prop_cols <- colMeans(subgroup_indicator)
  }

  ## avoid R CMD check NOTEs for data.table non-standard evaluation:
  policy <- estimate <- se <- subgroup <- subgroup_prop <- NULL

  tabs <- lapply(views, function(v) {
    est <- policy_eval_estimate(object, contrast = v)
    coef <- coef(est)
    labels <- names(coef)
    se_ <- tryCatch(
      sqrt(diag(vcov(est))),
      error = function(e) rep(NA_real_, length(coef))
    )

    ## split "<term>: d=<policy>" into term and policy:
    has_policy <- grepl(": d=", labels, fixed = TRUE)
    pol <- ifelse(has_policy, sub("^.*: d=", "", labels), NA_character_)
    term <- sub(": d=.*$", "", labels)

    ## subgroup label parsed from the term (e.g. "...|d=1]"):
    has_subgroup <- grepl("|d=", term, fixed = TRUE)
    sub_lab <- ifelse(
      has_subgroup,
      paste0("d=", sub("^.*\\|d=([^]]+)\\].*$", "\\1", term)),
      NA_character_
    )

    dt <- data.table::data.table(
      policy = pol,
      threshold = NA_real_,
      subgroup = sub_lab,
      term = term,
      estimate = as.numeric(coef),
      se = as.numeric(se_),
      subgroup_prop = NA_real_,
      contrast = v
    )

    ## distinct policies in order of appearance:
    pol_levels <- unique(pol)

    ## map authoritative threshold by policy index (policy blocks are ordered by
    ## the sorted unique threshold, matching get_policy.blip / get_policy.ptl):
    if (!is.null(threshold) && length(threshold) == length(pol_levels)) {
      thr_map <- threshold
      names(thr_map) <- pol_levels
      dt[, threshold := unname(thr_map[policy])]
    }

    ## subgroup proportion by within-policy position (per policy the rows are
    ## ordered [d==a2 (,) d==a1]; the indicator columns are [d==a2, d==a1]):
    if (!is.null(subgroup_prop_cols) && identical(target, "subgroup")) {
      per <- if (isTRUE(v)) 1L else 2L
      for (pl in seq_along(pol_levels)) {
        idx <- which(dt[["policy"]] == pol_levels[pl])
        within <- seq_along(idx)
        sub_col <- ((within - 1L) %/% per) + 1L
        col <- (pl - 1L) * 2L + sub_col
        dt[idx, subgroup_prop := subgroup_prop_cols[col]]
      }
    }

    dt
  })

  out <- data.table::rbindlist(tabs)
  return(out[])
}

#' @rdname policy_eval
#' @export
coef.policy_eval <- function(object, contrast = TRUE, ...) {
  coef(policy_eval_estimate(object, contrast = contrast))
}

#' @rdname policy_eval
#' @export
IC.policy_eval <- function(x, contrast = TRUE, ...) {
  IC(policy_eval_estimate(x, contrast = contrast))
}

#' @rdname policy_eval
#' @export
vcov.policy_eval <- function(object, contrast = TRUE, ...) {
  vcov(policy_eval_estimate(object, contrast = contrast))
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
summary.policy_eval <- function(object, contrast = TRUE, return_table = FALSE, ...) {
  if (isTRUE(return_table)) {
    tab <- policy_eval_table(object, contrast = contrast)
    return(tab)
  }
  policy_eval_estimate(object, contrast = contrast)
}

#' @rdname policy_eval
#' @export
estimate.policy_eval <- function(x, ...) {
  summary(x, contrast = TRUE)
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
  coef <- agg[, 2]

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
