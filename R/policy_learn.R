#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         qv_models = NULL, qv_full_history = FALSE,
                         policy_vars = NULL, policy_full_history = FALSE, # note that the order of the policy_vars dictates the form of X in policy_tree
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
  } else if (type %in% c("bowl", "owl", "outcome_weighted_learning")){
    pl <- function(...){

      eval_args <- list(...)
      bowl_args <- append(pl_args, eval_args)

      do.call(what = "bowl", bowl_args)
    }
  } else{
    stop("Unknown type of policy learner. Use 'rql', 'rqvl', 'ptl' or bowl.")
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
