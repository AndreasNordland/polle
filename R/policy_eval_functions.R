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

## internal: build a tidy data.table summarising the estimates. The fixed
## prefix columns are always emitted (in this order):
##   policy        - the policy label (e.g. "blip(eta=28)"); NA if unnamed.
##   term          - the estimated parameter label from input_meta$target
##                   (contrast rows: the parsed contrast term).
##   action        - the action label; NA for value target and contrast rows.
##   subgroup      - numeric 1/0 subgroup indicator; NA for value target.
##   subgroup_prop - proportion of observations in the subgroup; NA for value
##                   target and when subgroup_indicator is unavailable.
##   estimate      - the point estimate.
##   se            - the standard error (sqrt of the variance diagonal).
##   contrast      - FALSE for value and per-subgroup means; TRUE for the
##                   subgroup average treatment effects.
## The variable suffix columns are the remaining columns of input_meta (e.g.
## threshold, alpha, type, K, quantile_prob_threshold), appended after the
## fixed prefix.
##
## Filtering by `contrast`:
##   - target = "value":    contrast is ignored; a single-view table is returned.
##   - target = "subgroup": contrast = FALSE returns the per-subgroup potential
##                          outcome means; contrast = TRUE returns the subgroup
##                          average treatment effects.
policy_eval_table <- function(object, contrast = TRUE) {
  target <- get_element(object, "target")
  input_meta <- get_element(object, "input_meta")
  name <- get_element(object, "name")

  ## parse the policy label from a "TERM: d=POLICY" string:
  parse_policy <- function(x) {
    ifelse(grepl(": d=", x, fixed = TRUE),
           sub("^.*: d=", "", x),
           NA_character_)
  }

  ## safe standard error extraction:
  se_of <- function(est) {
    tryCatch(sqrt(diag(vcov(est))),
             error = function(e) rep(NA_real_, length(coef(est))))
  }

  if (identical(target, "value")) {
    est <- policy_eval_estimate(object, contrast = FALSE)
    tab <- data.table::data.table(
      policy        = parse_policy(name),
      term          = as.character(input_meta[["target"]]),
      action        = NA_character_,
      subgroup      = NA_real_,
      subgroup_prop = NA_real_,
      estimate      = unname(as.numeric(coef(est))),
      se            = unname(as.numeric(se_of(est))),
      contrast      = FALSE
    )
    extra_cols <- setdiff(names(input_meta), "target")
    if (length(extra_cols)) {
      tab <- cbind(tab, input_meta[, extra_cols, with = FALSE])
    }
    return(tab[])
  }

  if (!identical(target, "subgroup")) {
    stop("unknown target: ", target)
  }

  ## subgroup target: build either the means view (contrast = FALSE) or the
  ## contrasts view (contrast = TRUE).
  subgroup_indicator <- object[["subgroup_indicator"]]
  sp_cols <- if (!is.null(subgroup_indicator)) colMeans(subgroup_indicator) else NULL

  if (isFALSE(contrast)) {
    est <- policy_eval_estimate(object, contrast = FALSE)
    ## per-policy blocks of 4 rows; sp_cols has 2 entries per policy
    ## (d==a2 first, d==a1 second). Broadcast to 4 rows per policy.
    if (!is.null(sp_cols)) {
      sp <- rep(sp_cols, each = 2)
    } else {
      sp <- rep(NA_real_, nrow(input_meta))
    }
    tab <- data.table::data.table(
      policy        = parse_policy(name),
      term          = as.character(input_meta[["target"]]),
      action        = as.character(input_meta[["action"]]),
      subgroup      = as.numeric(input_meta[["subgroup"]]),
      subgroup_prop = as.numeric(sp),
      estimate      = unname(as.numeric(coef(est))),
      se            = unname(as.numeric(se_of(est))),
      contrast      = FALSE
    )
    extra_cols <- setdiff(names(input_meta), c("target", "action", "subgroup"))
    if (length(extra_cols)) {
      tab <- cbind(tab, input_meta[, extra_cols, with = FALSE])
    }
    return(tab[])
  }

  ## contrast = TRUE
  est_c <- policy_eval_estimate(object, contrast = TRUE)
  contrast_name <- get_element(object, "contrast_name", check_name = FALSE)

  ## for each 4-row policy block in input_meta, keep rows 1 and 3 (subgroup 1
  ## and subgroup 0):
  n_blocks <- nrow(input_meta) %/% 4L
  keep <- as.vector(vapply(
    seq_len(n_blocks),
    function(b) (b - 1L) * 4L + c(1L, 3L),
    integer(2L)
  ))
  meta_c <- input_meta[keep, ]

  ## term for contrast rows: parsed from contrast_name by stripping ": d=..."
  term_c <- sub(": d=.*$", "", contrast_name)

  ## subgroup proportion for contrast rows: sp_cols is already ordered
  ## [pol1_d==a2, pol1_d==a1, pol2_d==a2, pol2_d==a1, ...] which matches the
  ## contrast row order (subgroup 1 then subgroup 0 per policy).
  if (!is.null(sp_cols)) {
    sp <- as.numeric(sp_cols)
  } else {
    sp <- rep(NA_real_, nrow(meta_c))
  }

  tab <- data.table::data.table(
    policy        = parse_policy(contrast_name),
    term          = term_c,
    action        = NA_character_,
    subgroup      = as.numeric(meta_c[["subgroup"]]),
    subgroup_prop = sp,
    estimate      = unname(as.numeric(coef(est_c))),
    se            = unname(as.numeric(se_of(est_c))),
    contrast      = TRUE
  )
  extra_cols <- setdiff(names(input_meta), c("target", "action", "subgroup"))
  if (length(extra_cols)) {
    tab <- cbind(tab, meta_c[, extra_cols, with = FALSE])
  }
  return(tab[])
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
