#' @rdname policy_data
#' @export
print.policy_data <- function(x, digits = 2, ...){
  s <- summary(x, probs=NULL)
  cat(
    paste("Policy data with n = ", s$n,
          " observations and maximal K = ", s$K,
          " stages.", sep = "")
  )
  cat("\n\n")
  print(s$tab)

  cat("\n")

  bas_var <- paste("Baseline covariates: ", s$baseline_covar, sep = "")
  cat(paste(strwrap(bas_var, 60), collapse="\n"))

  cat("\n")

  state_var <- paste("State covariates: ", s$stage_covar, sep = "")
  cat(paste(strwrap(state_var, 60), collapse="\n"))

  cat("\n")
  mean_utility <- mean(get_utility(x)$U)
  mean_utility <- round(mean_utility, digits = digits)

  cat(
    paste("Average utility: ", mean_utility, sep = "")
  )
  cat("\n")
}

#' @rdname policy_data
#' @export
summary.policy_data <- function(object, probs=seq(0, 1, .25), ...) {
  K <- get_K(object)
  n <- get_n(object)
  stage <- event <- NULL  # R-check glob. var.
  action_set <- get_action_set(object)
  stage_data <- getElement(object, "stage_data")
  st <- stage_data[event == 0,][, c("stage", "A"), with = FALSE]
  st$A <- factor(st$A, levels = action_set)
  colnames(st) <- c("stage", "action")
  tab <- table(st)
  stable <- addmargins(tab, 2, FUN = list(n = sum))
  bc <- paste(object$colnames$baseline_names, collapse = ", ")
  sc <- paste(object$colnames$state_names, collapse = ", ")
  stagedist <- list()
  dt <- get_cum_rewards(object)
  if (!is.null(probs))
    for (i in seq_len(K+1)) {
      ss <- dt[stage==i]
      val <- stats::quantile(ss[["U"]], probs = probs, ...)
      stagedist <- append(stagedist, list(val))
    }
  res <- list(n=n, K=K, tab=stable, stagedist=stagedist,
              baseline_covar=bc, stage_covar=sc)
  class(res) <- "summary.policy_data"
  res
}

#' Plot policy data for given policies
#'
#' @param x Object of class [policy_data]
#' @param policy An object or list of objects of class [policy]
#' @param which A subset of the numbers 1:2
#' \itemize{
#'  \item{1} Spaghetti plot of the cumulative rewards
#'  \item{2} Plot of the policy actions for a given stage
#' }
#' @param stage Stage number for plot 2
#' @param history_variables character vector of length 2 for plot 2
#' @param jitter numeric
#' @param ... Additional arguments
#' @examples
#' library("polle")
#' library("data.table")
#' setDTthreads(1)
#' d3 <- sim_multi_stage(2e2, seed = 1)
#' pd3 <- policy_data(data = d3$stage_data,
#'                    baseline_data = d3$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#'
#' # specifying two static policies:
#' p0 <- policy_def(c(1,1,0,0), name = "p0")
#' p1 <- policy_def(c(1,0,0,0), name = "p1")
#'
#' plot(pd3)
#' plot(pd3, policy = list(p0, p1))
#'
#' # learning and plotting a policy:
#'  pe3 <- policy_eval(pd3,
#'                     policy_learn = policy_learn(),
#'                     q_models = q_glm(formula = ~t + X + X_lead))
#' plot(pd3, list(get_policy(pe3), p0))
#'
#' # plotting the recommended actions at a specific stage:
#' plot(pd3, get_policy(pe3),
#'      which = 2,
#'      stage = 2,
#'      history_variables = c("t","X"))
#' @export
plot.policy_data <- function(x,
                             policy=NULL,
                             which = c(1),
                             stage = 1,
                             history_variables = NULL,
                             jitter=.05,
                             ...) {
  policy_data <- x
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
    dt <- get_cum_rewards(policy_data, policy = policy)
    if (nrow(dt) == 0)
      stop("No observations comply with the given policy/policies.")
    lev <- unique(dt[["policy_group"]])
    id <- stage <- NULL  # R-check glob. var.
    dots <- list(...)
    pargs <- c("col","lty","pch")
    for (p in pargs) {
      if (is.null(dots[[p]])) {
        dots[[p]] <- seq_len(length(policy)+1)
      }
      dots[[p]] <- rep(dots[[p]], length.out=length(policy)+1)
    }
    ylab <- "Cumulative reward"
    xlab <- "Stage"
    legend <- "topleft"
    for (p in c("xlab","ylab","legend")) {
      if (!is.null(dots[[p]])) {
        assign(p, dots[[p]])
        dots[[p]] <- NULL
      }
    }
    plot(U ~ stage, data=dt, type="n", xlab="", ylab="", axes=FALSE)
    for (i in seq_along(lev)) {
      di <- subset(dt, dt$policy_group==lev[i])
      wide <- t(dcast(di,
                      id ~ stage, value.var="U")[,-1])
      args <- dots
      for (p in pargs)
        args[[p]] <- args[[p]][i]

      do.call(graphics::points, c(list(U ~ base::jitter(as.numeric(stage), jitter),
                                       data=di),
                                  args))
      do.call(graphics::matlines, c(list(wide), args))
    }
    graphics::title(xlab=xlab, ylab=ylab)
    if (length(lev)>0 && !is.null(legend)) {
      graphics::legend(legend, legend=lev,
                       col=dots$col,
                       pch=dots$pch,
                       lty=dots$lty)
    }
    graphics::box()
    graphics::axis(2)
    graphics::axis(1, at=1:get_K(x))
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

    plot_data <- data.table::merge.data.table(
      policy(policy_data)[stage ==stage_],
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

#' @export
print.summary.policy_data <- function(x, ...) {
  cat(paste("Policy data with n = ", x$n,
            " observations and maximal K = ",
            x$K, " stages.\n", sep = ""))
  cat("\n")
  print(x$tab)
  cat("\n")
  cat(paste("Baseline covariates: ", x$baseline_covar, sep = ""))
  cat("\n")
  cat(paste("Stage covariates: ", x$stage_covar, sep = ""))
  cat("\n")

  cat("\n")
  for (i in 1:(x$K+1)) {
    cat("Cumulative reward distribution at stage ",i,":\n", sep="")
    print(x$stagedist[[i]])
    cat("\n")
  }
}

#' Copy Policy Data Object
#'
#' Objects of class [policy_data] contains elements of class [data.table].
#' \code{data.table} provide functions that operate on objects by reference.
#' Thus, the \code{policy_data} object is not copied when modified by reference,
#' see examples. An explicit copy can be made by \code{copy_policy_data}. The
#' function is a wrapper of [data.table::copy()].
#' @param object Object of class [policy_data].
#' @return Object of class [policy_data].
#' @export
#' @examples
#' library("polle")
#' ### Single stage case: Wide data
#' d1 <- sim_single_stage(5e2, seed=1)
#' head(d1, 5)
#' # constructing policy_data object:
#' pd1 <- policy_data(d1,
#'                    action="A",
#'                    covariates=c("Z", "B", "L"),
#'                    utility="U")
#' pd1
#'
#' # True copy
#' pd2 <- copy_policy_data(pd1)
#' # manipulating the data.table by reference:
#' pd2$baseline_data[, id := id + 1]
#' head(pd2$baseline_data$id - pd1$baseline_data$id)
#'
#' # False copy
#' pd2 <- pd1
#' # manipulating the data.table by reference:
#' pd2$baseline_data[, id := id + 1]
#' head(pd2$baseline_data$id - pd1$baseline_data$id)
copy_policy_data <- function(object){

  object$stage_data <- copy(object$stage_data)
  object$baseline_data <- copy(object$baseline_data)

  return(object)
}

partial_stage_data <- function(stage_data, K, deterministic_rewards){
  required_names <- c("id", "stage", "event", "A", "U")

  # filtering stage_data rows up till stage K:
  stage <- NULL
  stage_data_K <- stage_data[stage <= K, ]

  # filtering the residual stage data rows for stages above K:
  stage_data_res <- stage_data[stage > K, required_names, with = FALSE]

  # summarizing residual stage data as a single row:
  event <- U <- NULL
  stage_data_res_sum <- stage_data_res[
    ,
    list(
      stage = min(stage), # min(stage) is K + 1
      event = max(event), # max(event) is the event at stage K*, which is either 1 or 2
      U = sum(U) # sum(U) is the sum of the utility contributions from stage K+1 to K*
    ),
    by = "id"
  ]

  # updated action set:
  action_set <- unname(sort(unlist(unique(stage_data_K[,"A"]))))
  deterministic_rewards_K <- paste("U_A", action_set, sep = "")

  # removing excess deterministic reward variables:
  tmp <- setdiff(deterministic_rewards, deterministic_rewards_K)
  if (length(tmp) > 0)
    stage_data_K[, (tmp) := NULL]

  stage_data_res_sum[, (deterministic_rewards_K) := NA]
  # binding stage_data_K with stage_data_res_sum
  stage_data <- rbindlist(list(stage_data_K, stage_data_res_sum),
                          fill = TRUE,
                          use.names = TRUE)

  # setting keys and index
  setkeyv(stage_data, c("id", "stage"))
  setindexv(stage_data, "event")

  out <- list(
    stage_data = stage_data,
    action_set = action_set,
    deterministic_rewards = deterministic_rewards_K
  )

  return(out)
}

#' Trim Number of Stages
#'
#' \code{partial} creates a partial policy data object by trimming
#'  the maximum number of stages in the policy data object to a fixed
#'  given number.
#' @param object Object of class [policy_data].
#' @param K Maximum number of stages.
#' @return Object of class [policy_data].
#' @examples
#' library("polle")
#' ### Multiple stage case
#' d <- sim_multi_stage(5e2, seed = 1)
#' # constructing policy_data object:
#' pd <- policy_data(data = d$stage_data,
#'                    baseline_data = d$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#' pd
#' # Creating a partial policy data object with 3 stages
#' pd3 <- partial(pd, K = 3)
#' pd3
#' @export
partial <- function(object, K)
  UseMethod("partial")

#' @export
partial.policy_data <- function(object, K){
  # input checks:
  if (!is.numeric(K))
    stop("K must be an integer greater than or equal to 1.")
  if (!((K %% 1 == 0) & (K>0)))
    stop("K must be an integer greater than or equal to 1.")

  # copy object to avoid reference issues in data.table
  object <- copy_policy_data(object)

  object_K <- get_K(object)
  if(K >= object_K)
    return(object)

  # transforming the stage data:
  psd <- partial_stage_data(
    stage_data = object[["stage_data"]],
    K = K,
    deterministic_rewards = object[["colnames"]][["deterministic_rewards"]]
  )

  # updating the object
  object[["stage_data"]] <- psd[["stage_data"]]
  object[["dim"]][["K"]] <- K
  object[["stage_action_sets"]] <- object[["stage_action_sets"]][1:K]
  object[["action_set"]] <- psd[["action_set"]]
  object[["colnames"]][["deterministic_rewards"]] <- psd[["deterministic_rewards"]]

  return(object)
}

#' Subset Policy Data on ID
#'
#' \code{subset_id} returns a policy data object containing the given IDs.
#' @param object Object of class [policy_data].
#' @param id character vectors of IDs.
#' @param preserve_action_set If TRUE, the action sets must be preserved.
#' @returns Object of class [policy_data].
#' @examples
#' library("polle")
#' ### Single stage:
#' d <- sim_single_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd
#'
#' # getting the observation IDs:
#' get_id(pd)[1:10]
#'
#' # subsetting on IDs:
#' pdsub <- subset_id(pd, id = 250:500)
#' pdsub
#' get_id(pdsub)[1:10]
#' @export
subset_id <- function(object, id, preserve_action_set = TRUE)
  UseMethod("subset_id")

#' @export
subset_id.policy_data <- function(object, id, preserve_action_set = TRUE){
  if (!all(id %in% get_id(object)))
    stop("Invalid subset of IDs.")
  id_ <- id; rm(id)

  action_set <- NULL
  stage_action_sets <- NULL
  if (preserve_action_set == TRUE){
    action_set <- object$action_set
    stage_action_sets <- object$stage_action_sets
  }

  spd <- new_policy_data(
    stage_data = object$stage_data[id %in% id_],
    baseline_data = object$baseline_data[id %in% id_],
    action_set = action_set,
    stage_action_sets = stage_action_sets
  )

  return(spd)
}

#' Get the full history for a given stage
#'
#' @noRd
#' @param object Object of class [policy_data()].
#' @param stage stage number.
#' @return Object of class "history".
full_history <- function(object, stage){
  K <- get_K(object)
  if (stage > K)
    stop("The stage number must be lower or equal to maximal number of stages observed.")

  object_colnames <- getElement(object, "colnames")
  stage_data <- getElement(object, "stage_data")
  state_names <- getElement(object_colnames, "state_names")
  baseline_data <- getElement(object, "baseline_data")
  baseline_names <- getElement(object_colnames, "baseline_names")
  action_set <- get_action_set(object)
  stage_action_sets <- get_stage_action_sets(object)
  deterministic_rewards <- getElement(object_colnames, "deterministic_rewards")
  stage_ <- stage; rm(stage)

  # getting stage specific history names:
  AH_names <- c("id", "stage", "A", state_names)
  # filtering rows which have an action (event = 0):
  event <- NULL
  AH <- stage_data[event == 0, ]
  # filtering rows up till the given stage number:
  stage <- NULL
  AH <- AH[stage <= stage_, AH_names, with = FALSE]
  # filtering observations with an action at the given stage:
  AH <- AH[, if(any(stage == stage_)) .SD, id]
  # transforming the data from long to wide format:
  AH <- dcast(AH, id ~ stage, value.var = AH_names[-c(1,2)])
  # inserting stage column:
  AH[, stage := stage_]
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_names) > 0){
    AH[baseline_data, (baseline_names) := mget(paste0('i.', baseline_names))]
  }
  # setting key and column order
  setkeyv(AH, c("id", "stage"))
  setcolorder(AH, neworder = c("id", "stage"))

  # separating the action at the given stage
  action_name <- paste("A", stage_, sep = "_")
  A_names <- c("id", "stage", action_name)
  A <- AH[ , A_names, with = FALSE]
  H <- AH[, c(action_name) := NULL]

  # getting the accumulated utility and deterministic utility contributions
  U_bar <- id <- NULL
  U <- stage_data[stage <= stage_][, U_bar := sum(U), id]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", deterministic_rewards)
  U <- U[ , U_names, with = FALSE]

  history <- list(
    H = H,
    A = A,
    U = U,
    action_name = action_name,
    deterministic_rewards = deterministic_rewards,
    action_set = action_set,
    stage_action_set = stage_action_sets[[stage_]],
    stage = stage_
  )
  class(history) <- "history"

  return(history)
}

#' Get the state/Markov-type history for a given stage
#'
#' @noRd
#' @param object object of class [policy_data()].
#' @param stage stage number.
#' @return Object of class "history".
stage_state_history <- function(object, stage){

  if (stage > object$dim$K)
    stop("The stage number must be lower or equal to maximal number of stages observed.")

  stage_data <- object$stage_data
  state_names <- object$colnames$state_names
  baseline_data <- object$baseline_data
  baseline_names <- object$colnames$baseline_names
  action_set <- get_action_set(object)
  stage_action_sets <- get_stage_action_sets(object)
  deterministic_rewards <- object$colnames$deterministic_rewards
  stage_ <- stage

  # getting the state names:
  AH_names <- c("id", "stage", "A", state_names)
  # filtering rows which have an action (event = 0):
  event <- NULL
  AH <- stage_data[event == 0, ]
  rm(event)
  # filtering observations with an action at the given stage:
  AH <- AH[stage == stage_, AH_names, with = FALSE]
  # merging the state history and the the baseline data:
  if(length(baseline_names) > 0){
    AH[baseline_data, (baseline_names) := mget(paste0('i.', baseline_names))]
  }

  ### constructing H and A:
  # separating the action at the given stage
  A_names <- c("id", "stage", "A")
  A <- AH[,  A_names, with = FALSE]
  H <- AH[, c("A") := NULL]

  # getting the accumulated utility and deterministic utility contributions
  U <- U_bar <- NULL
  U <- stage_data[stage <= stage_][, U_bar := sum(U), by = "id"]
  U <- U[event == 0][stage == stage_,]
  U_names <- c("id", "stage", "U_bar", deterministic_rewards)
  U <- U[, U_names, with = FALSE]

  id_names <- c("id", "stage")

  history <- list(
    H = H,
    A = A,
    U = U,
    action_name = "A",
    deterministic_rewards = deterministic_rewards,
    action_set = action_set,
    stage_action_set = stage_action_sets[[stage_]],
    stage = stage
  )
  class(history) <- "history"

  return(history)
}

#' Get the state/Markov-type history for all stages
#'
#' @noRd
#' @param object object of class [policy_data()].
#' @return Object of class "history".
state_history <- function(object){
  stage_data <- object$stage_data
  state_names <- object$colnames$state_names
  baseline_data <- object$baseline_data
  baseline_names <- object$colnames$baseline_names
  action_set <- object$action_set

  # getting stage specific history names:
  AH_names <- c("id", "stage", "A", state_names)
  # filtering rows which have an action (event = 0):
  event <- NULL
  AH <- stage_data[event == 0, ]
  AH <- AH[, AH_names, with = FALSE]
  rm(event)
  # merging the stage specific histories and the the baseline data by reference:
  if(length(baseline_names) > 0){
    AH[baseline_data, (baseline_names) := mget(paste0('i.', baseline_names))]
  }

  ### constructing H and A:
  # separating the action at the given stage
  A_names <- c("id", "stage", "A")
  A <- AH[ , A_names, with = FALSE]
  H <- AH[, c("A") := NULL]

  history <- list(
    H = H,
    A = A,
    action_name = "A",
    action_set = action_set
  )
  class(history) <- "history"

  return(history)
}

get_stage_data <- function(object)
  UseMethod("get_stage_data")

get_stage_data.policy_data <- function(object){
  out <- getElement(object, "stage_data")
  return(out)
}

#' @name history
#' @rdname get_history
NULL

#' Get History Object
#'
#' \code{get_history} summarizes the history and action at a given stage from a
#' [policy_data] object.
#' @param object Object of class [policy_data].
#' @param stage Stage number. If NULL, the state/Markov-type history across
#' all stages is returned.
#' @param full_history Logical. If TRUE, the full history is returned
#' If FALSE, only the state/Markov-type history is returned.
#' @details
#' Each observation has the sequential form
#' \deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
#' for a possibly stochastic number of stages K.
#' \itemize{
#'  \item{} \eqn{B} is a vector of baseline covariates.
#'  \item{} \eqn{U_k} is the reward at stage k (not influenced by the action \eqn{A_k}).
#'  \item{} \eqn{X_k} is a vector of state covariates summarizing the state at stage k.
#'  \item{} \eqn{A_k} is the categorical action at stage k.
#' }
#' @returns Object of class [history]. The object is a list
#' containing the following elements:
#' \item{\code{H}}{[data.table] with keys id and stage and with variables
#'                 \{\eqn{B}, \eqn{X_k}\} (state history) or
#'                 \{\eqn{B}, \eqn{X_1}, \eqn{A_1}, ..., \eqn{X_k}\}
#'                 (full history), see details.}
#' \item{\code{A}}{[data.table] with keys id and stage and variable \eqn{A_k}, see
#'                 details.}
#' \item{action_name}{Name of the action variable in \code{A}.}
#' \item{action_set}{Sorted character vector defining the action set.}
#' \item{U}{(If \code{stage} is not NULL) [data.table] with keys id and stage
#'          and with variables U_bar and U_Aa for every a in the actions set.
#'          U_bar is the accumulated rewards up till and including the given
#'          stage, i.e., \eqn{\sum_{j=1}^k U_j}. U_Aa is the deterministic
#'          reward of action a.}
#' @examples
#' library("polle")
#' ### Single stage:
#' d1 <- sim_single_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
#' pd1
#'
#' # In the single stage case, set stage = NULL
#' h1 <- get_history(pd1)
#' head(h1$H)
#' head(h1$A)
#'
#' ### Two stages:
#' d2 <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd2 <- policy_data(d2,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd2
#' # getting the state/Markov-type history across all stages:
#' h2 <- get_history(pd2)
#' head(h2$H)
#' head(h2$A)
#'
#' # getting the full history at stage 2:
#' h2 <- get_history(pd2, stage = 2, full_history = TRUE)
#' head(h2$H)
#' head(h2$A)
#' head(h2$U)
#'
#' # getting the state/Markov-type history at stage 2:
#' h2 <- get_history(pd2, stage = 2, full_history = FALSE)
#' head(h2$H)
#' head(h2$A)
#'
#' ### Multiple stages
#' d3 <- sim_multi_stage(5e2, seed = 1)
#' # constructing policy_data object:
#' pd3 <- policy_data(data = d3$stage_data,
#'                    baseline_data = d3$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#' pd3
#'
#' # getting the full history at stage 2:
#' h3 <- get_history(pd3, stage = 2, full_history = TRUE)
#' head(h3$H)
#' # note that not all observations have two stages:
#' nrow(h3$H) # number of observations with two stages.
#' get_n(pd3) # number of observations in total.
#' @export
get_history <- function(object, stage = NULL, full_history = FALSE)
  UseMethod("get_history")

#' @export
get_history.policy_data <- function(object, stage = NULL, full_history = FALSE){
  # input checks:
  if (!is.logical(full_history) | (length(full_history) != 1))
    stop("full_history must be TRUE or FALSE")
  if (!is.null(stage)){
    if (!(is.numeric(stage) & (length(stage) == 1)))
      stop("stage must be an integer greater than 0.")
    if (!(stage %% 1 == 0))
      stop("stage must be an integer greater than 0.")
    if (stage<=0)
      stop("stage must be an integer greater than 0.")
  }

  if (full_history == TRUE){
    if (is.null(stage)) stop("Please provide a stage number.")
    his <- full_history(object, stage = stage)
  } else{
    if (is.null(stage)){
      his <- state_history(object)
    } else{
      his <- stage_state_history(object, stage = stage)
    }
  }
  return(his)
}

#' Get history variable names
#'
#' \code{get_history_names()} returns the state covariate names of the history data
#' table for a given stage. The function is useful when specifying
#' the design matrix for [g_model] and [q_model] objects.
#' @param object Policy data object created by [policy_data()].
#' @param stage Stage number. If NULL, the state/Markov-type history variable
#' names are returned.
#' @return Character vector.
#' @examples
#' library("polle")
#' ### Multiple stages:
#' d3 <- sim_multi_stage(5e2, seed = 1)
#' pd3 <- policy_data(data = d3$stage_data,
#'                    baseline_data = d3$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#' pd3
#' # state/Markov type history variable names (H):
#' get_history_names(pd3)
#' # full history variable names (H_k) at stage 2:
#' get_history_names(pd3, stage = 2)
#' @export
get_history_names <- function(object, stage)
  UseMethod("get_history_names")

#' @export
get_history_names.policy_data <- function(object, stage = NULL){
  if (is.null(stage)){
    history <- get_history(object, full_history = FALSE)
  } else{
    history <- get_history(object, stage = stage, full_history = TRUE)
  }
  H <- getElement(history, "H")
  stopifnot(!is.null(H))
  history_names <- names(H)[!(names(H) %in% c("id", "stage"))]
  return(history_names)
}


#' Get the Utility
#'
#' \code{get_utility()} returns the utility, i.e., the sum of the rewards,
#' for every observation in the policy data object.
#'
#' @param object Object of class [policy_data].
#' @returns [data.table] with key id and numeric variable U.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the utility:
#' head(get_utility(pd))
#'@export
get_utility <- function(object)
  UseMethod("get_utility")

#' @export
get_utility.policy_data <- function(object){
  stage_data <- get_stage_data(object)
  id <- U <- NULL
  U <- stage_data[, list(U = sum(U)), id]
  return(U)
}

get_rewards <- function(object){
  stage_data <- get_stage_data(object)
  R <- stage_data[, c("id", "stage", "U"), with = FALSE]
  return(R)
}

#' Get Actions
#'
#' \code{get_actions} returns the actions at every stage for every observation
#' in the policy data object.
#' @param object Object of class [policy_data].
#' @returns [data.table] with keys id and stage and character variable A.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the actions:
#' head(get_actions(pd))
#' @export
get_actions <- function(object)
  UseMethod("get_actions")

#' @export
get_actions.policy_data <- function(object){
  stage_data <- get_stage_data(object)
  event <- NULL
  actions <- stage_data[event == 0, c("id", "stage", "A"), with = FALSE]
  return(actions)
}

#' Get IDs
#'
#' \code{get_id} returns the ID for every observation in the policy data object.
#' @param object Object of class [policy_data] or [history].
#' @returns Character vector.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the IDs:
#' head(get_id(pd))
#' @export
get_id <- function(object)
  UseMethod("get_id")

#' @export
get_id.policy_data <- function(object){
  id <- unique(object$stage_data$id)
  return(id)
}

#' Get IDs and Stages
#'
#' \code{get_id} returns the stages for every ID for every observation in the policy data object.
#' @param object Object of class [policy_data] or [history].
#' @returns [data.table] with keys id and stage.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the IDs and stages:
#' head(get_id_stage(pd))
#' @export
get_id_stage <- function(object)
  UseMethod("get_id_stage")

#' @export
get_id_stage.policy_data <- function(object){
  stage_data <- get_stage_data(object)
  id_stage_names <- c("id", "stage")
  event <- NULL
  id_stage <- stage_data[event == 0, ][, id_stage_names, with = FALSE]

  return(id_stage)
}

#' Get Maximal Stages
#'
#' \code{get_K} returns the maximal number of stages for the observations in
#' the policy data object.
#' @param object Object of class [policy_data].
#' @returns Integer.
#' @examples
#' d <- sim_multi_stage(5e2, seed = 1)
#' pd <- policy_data(data = d$stage_data,
#'                    baseline_data = d$baseline_data,
#'                    type = "long",
#'                    id = "id",
#'                    stage = "stage",
#'                    event = "event",
#'                    action = "A",
#'                    utility = "U")
#' pd
#' # getting the maximal number of stages:
#' get_K(pd)
#' @export
get_K <- function(object)
  UseMethod("get_K")

#' @export
get_K.policy_data <- function(object){
  K <- object$dim$K
  return(K)
}

#' Get Number of Observations
#'
#' \code{get_n} returns the number of observations in
#' the policy data object.
#' @param object Object of class [policy_data].
#' @returns Integer.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the number of observations:
#' get_n(pd)
#' @export
get_n <- function(object)
  UseMethod("get_n")

#' @export
get_n.policy_data <- function(object){
  n <- object$dim$n
  return(n)
}

#' Get Action Set
#'
#' \code{get_action_set} returns the action set, i.e., the possible
#' actions at each stage for the policy data object.
#' @param object Object of class [policy_data].
#' @returns Character vector.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the actions set:
#' get_action_set(pd)
#' @export
get_action_set <- function(object)
  UseMethod("get_action_set")

#' @export
get_action_set.policy_data <- function(object){
  action_set <- object$action_set
  return(action_set)
}

#' Get Stage Action Sets
#'
#' \code{get_stage_action_sets} returns the action sets at each stage, i.e.,
#' the possible actions at each stage for the policy data object.
#' @param object Object of class [policy_data].
#' @returns List of character vectors.
#' @examples
#' ### Two stages:
#' d <- sim_two_stage_multi_actions(5e2, seed=1)
#' # constructing policy_data object:
#' pd <- policy_data(d,
#'                   action = c("A_1", "A_2"),
#'                   baseline = c("B"),
#'                   covariates = list(L = c("L_1", "L_2"),
#'                                     C = c("C_1", "C_2")),
#'                   utility = c("U_1", "U_2", "U_3"))
#' pd
#'
#' # getting the stage actions set:
#' get_stage_action_sets(pd)
#' @export
get_stage_action_sets <- function(object)
  UseMethod("get_stage_action_sets")

#' @export
get_stage_action_sets.policy_data <- function(object){
  stage_action_sets <- getElement(object, "stage_action_sets")
  return(stage_action_sets)
}




