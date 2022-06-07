
#' Create Policy Learner Object
#'
#' @param type Character string. Type of policy learner method:
#' \itemize{
#'   \item{} "rql": Realistic Quality/Q-learning
#'   \item{} "rqvl": Realistic V-restricted Q-learning
#'   \item{} "ptl": Policy Tree Learning
#' }
#' @param alpha probability limit for determining realistic actions.
#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         qv_models = NULL, qv_full_history = FALSE,
                         policy_vars = NULL, # note that the order of the policy_vars dictates the form of X in policy_tree
                         policy_full_history = FALSE,
                         L = NULL, seed = NULL,
                         ...){
  type <- tolower(type)

  fm <- formals()
  fm[["..."]] <- NULL
  cl <- match.call(expand.dots=TRUE)
  for (i in setdiff(names(fm), names(cl)))
    cl[i] <- list(fm[[i]])

  pl_args <- as.list(cl)[-1]
  pl_args[["type"]] <- NULL

  if (type %in% c("rql", "ql", "q_learning", "q-learning")) {
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){
      fm <- formals()
      cl <- match.call(expand.dots=TRUE)
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      rql_args <- append(pl_args, eval_args)

      do.call(what = "rql", rql_args)
    }
  }
  else if (type %in% c("rqvl", "qvl", "qv_learning", "qv-learning")) {
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){
      fm <- formals()
      cl <- match.call(expand.dots=TRUE)
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      rqvl_args <- append(pl_args, eval_args)

      do.call(what = "rqvl", rqvl_args)
    }
  } else if (type %in% c("ptl", "policytree", "policy_tree")){
    if (!require("policytree")) {
      stop("The policytree package is required to perform value searching using trees.")
    }
    pl <- function(policy_data,
                   g_models = NULL, g_functions = NULL, g_full_history = FALSE,
                   q_models, q_full_history = FALSE, verbose = FALSE){

      fm <- formals()
      cl <- match.call(expand.dots=TRUE)
      for (i in setdiff(names(fm), names(cl)))
        cl[i] <- list(fm[[i]])

      eval_args <- as.list(cl)[-1]
      ptl_args <- append(pl_args, eval_args)

      do.call(what = "ptl", ptl_args)
    }
  } else{
    stop("Unknown type of policy learner. Use 'rql', 'rqvl' or 'ptl'")
  }
  class(pl) <- c("policy_function", "function")
  return(pl)
}

#' @export
print.policy_function <- function(x) {
  cp <- capture.output(print.function(x))
  cp <- paste(cp[1:3], collapse = "")
  cp <- gsub(" ", "", cp, fixed = TRUE)
  cp <- substr(cp,1, nchar(cp)-1)
  print(cp, quote = FALSE)

}
