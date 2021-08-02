score_class <- function(prob, obs, levels=NULL, weights = NULL,
                       metric = c("brier", "logscore"), messages = 0) {
    if (is.null(levels) && length(colnames(prob))==length(levels))
      levels <- colnames(prob)
    if (is.factor(obs) && is.null(levels)) {
      levels <- levels(obs)
    }
    if (is.null(levels)) stop("missing definition of levels")
    cl <- levels
    if (is.factor(obs)) {
      cl.obs <- levels(obs)
    } else {
      cl.obs <- unique(obs)
    }
    newcl <- which(!cl.obs %in% cl)
    if (length(newcl)) {
        if (messages > 0)
            warning("new classes among observations")
        temp <- array(0, dim = c(nrow(prob), length(newcl)))
        colnames(temp) <- cl.obs[newcl]
        prob <- cbind(prob, temp)
    }
    y <- outer(obs, levels, "==")
    Bi <- apply((prob - y)^2, 1, sum)
    li <- apply(log(prob) * y, 1, function(x) sum(x[is.finite(x)],
        na.rm = TRUE))
    if (!is.null(weights)) {
        B <- stats::weighted.mean(Bi, w = weights, na.rm = TRUE)
        L <- stats::weighted.mean(li, w = weights, na.rm = TRUE)
    } else {
        B <- mean(Bi, na.rm = TRUE)
        L <- mean(li, na.rm = TRUE)
    }
    if (tolower(metric[1])%in%"brier") return(B)
    return(L)
}


expand.list <- function(...) {
  dots <- list(...)
  nam <- names(dots)
  nulls <- c()
  for (i in seq_along(dots)) {
    if (is.null(dots[[i]])) {
      dots[[i]] <- NA
      nulls <- c(nulls, i)
    }
  }
  names(dots) <- nam
  dat <- do.call(expand.grid, c(dots, list(KEEP.OUT.ATTRS = FALSE)))
  lapply(seq(NROW(dat)),
         function(i) {
           res <- as.list(dat[i,])
           if (length(nulls)>0) res[nulls] <- list(NULL)
           res
           })
}
