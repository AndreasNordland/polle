get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}

get_design <- function(formula, data) {
  tt <- terms(formula, data=data)
  attr(tt, "intercept") <- 1
  tt <- delete.response(tt)
  op <- options(na.action = "na.pass")

  mf <- tryCatch(model.frame(tt, data=data, drop.unused.levels = TRUE),
                    error = function(e) e
  )
  if (inherits(mf, "error")) {
    formula <- delete.response(terms(formula))
    mf$message <-
      paste0(mf$message, " when calling model.frame with formula:\n",
             format(formula))
    stop(mf)
  }

  xlevels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  options(op)
  idx_inter <- which(colnames(x) == "(Intercept)")
  if (length(idx_inter)>0)
    x <- x[,-idx_inter, drop = FALSE]
  colnames(x) <- gsub("[^[:alnum:].]", "_", colnames(x))
  return(list(terms=tt, xlevels=xlevels, x=x))
}

apply_design <- function(design, data){
  terms <- getElement(design, "terms")
  xlevels <- getElement(design, "xlevels")

  op <- options(na.action = "na.pass")
  mf <- model.frame(terms,
                    data=data,
                    xlev = xlevels,
                    drop.unused.levels=FALSE)
  newx <- model.matrix(mf, data=data, xlev = xlevels)
  options(op)

  idx_inter <- which(colnames(newx) == "(Intercept)")
  if (length(idx_inter)>0)
    newx <- newx[,-idx_inter, drop = FALSE]

  colnames(newx) <- gsub("[^[:alnum:].]", "_", colnames(newx))
  return(newx)
}

supp_warnings <- function(expr, mess, fun) {
  if(!is.character(mess))
    stop()
  if(!is.character(fun))
    stop()
  if(length(mess) != length(fun))
    stop()

  eval.parent(
    substitute(
      withCallingHandlers(expr, warning = function (w) {
        mess_ <- mess
        fun_ <- fun
        cm   <- conditionMessage(w)
        cc <- conditionCall(w)
        cond_cc <- FALSE
        if (is.call(cc) & length(as.character(cc))>0){
          cc <- as.character(cc)[[1]]
          cond_cc <- (cc == fun_)
        }
        cond_cm <- (cm == mess_)
        if (any(cond_cm & cond_cc))
          tryInvokeRestart("muffleWarning")
      })
    )
  )
}
