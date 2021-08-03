#' @export
policy_learn <- function(type = "rql",
                         alpha = 0,
                         qv_models = NULL, qv_full_history = FALSE,
                         policy_vars = NULL, policy_full_history = FALSE,
                         L = NULL,
                         ...){
  type <- tolower(type)

  fm <- formals()[-(1:3)]
  fm[["..."]] <- NULL
  cl <- match.call(expand.dots=TRUE)
  for (i in setdiff(names(fm), names(cl)))
    cl[i] <- list(fm[[i]])

  pl_args <- as.list(cl)[-c(1:2)]

  if (type %in% c("rql", "ql", "q_learning", "q-learning")) {
    pl <- function(...){
      eval_args <- list(...)
      rql_args <- append(pl_args, eval_args)

      do.call(what = "rql", rql_args)
    }
  }
  else if (type %in% c("rqvl", "qvl", "qv_learning", "qv-learning")) {
    pl <- function(...){
      eval_args <- list(...)
      rqvl_args <- append(pl_args, eval_args)

      do.call(what = "rqvl", rqvl_args)
    }
  } else if (type %in% c("ptl", "policytree", "policy_tree")){
    pl <- function(...){
      eval_args <- list(...)
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

  return(pl)
}
