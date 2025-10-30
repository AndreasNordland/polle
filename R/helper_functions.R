am <- Vectorize(
  function(a, action_set) {
    am <- action_set %in% a
    return(am)
  },
  vectorize.args = "a"
)
action_matrix <- function(a, action_set) {
  t(am(a = a, action_set = action_set))
}

get_a_values <- function(a, action_set, values, na.rm = TRUE) {
  stopifnot(
    is.data.table(values),
    all(key(values) == c("id", "stage")),
    length(action_set) == ncol(values[, -c("id", "stage"), with = FALSE]),
    length(a) == nrow(values)
  )
  P <- action_matrix(a = a, action_set = action_set) * values[, -c("id", "stage"), with = FALSE]
  P <- apply(P, 1, sum, na.rm = na.rm)
  out <- data.table(values[, c("id", "stage"), with = FALSE], P = P)

  return(out)
}

ipw_weight <- function(D, G){
  stopifnot(
    is.vector(D) | is.matrix(D),
    is.vector(G) | is.matrix(G),
    all(dim(G) == dim(D)),
    all(is.na(D) == is.na(G))
  )

  ## replacing NA values with 1:
  D[is.na(D)] <- 1
  G[is.na(G)] <- 1

  if (is.vector(D)) {
    out <- D / G
  } else {
    out <- apply(D / G, 1, prod)
  }

  if (any(is.na(out))) {
    mes <- "The policy dictates actions with fitted probability 0. Unable to calculate inverse probability weights."
    stop(mes)
  }

  return(out)
}

colprod <- function(M) {
  if (is.vector(M)) {
    out <- M
  } else {
    out <- apply(M, 1, prod, na.rm = TRUE)
  }
  return(out)
}

remove_null_elements <- function(x) {
  Filter(Negate(is.null), x)
}

get_element <- function(x, name, check_name = TRUE) {
  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a character string.")
  }
  if (check_name == TRUE) {
    if (!(name %in% names(x))) {
      stop("named element does not exist.")
    }
  }
  ## element with exact match:
  y <- getElement(x, name = name)

  return(y)
}

get_col <- function(dt, col_name) {
  if (!data.table::is.data.table(dt)) {
    stop("Input must be a data.table")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("col_name must be a single character string")
  }

  if (!col_name %in% names(dt)) {
    stop(sprintf("Column '%s' not found in data.table", col_name))
  }
  dt[[col_name]]
}

#' Create Random Folds of Indices
#'
#' @param number Integer. Number of folds to create
#' @param sample_size Integer. Total number of indices to split
#'
#' @return List of integer vectors containing indices, or NULL if number <= 1
#'
#' @examples
#' sample_folds(3, 10)
#' sample_folds(5, 100)
#' @noRd
sample_folds <- function(number, sample_size) {
  ## input checks:
  if (!is.numeric(number) || !is.numeric(sample_size)) {
    stop("Arguments must be numeric")
  }
  if (number < 0 || sample_size < 0) {
    stop("Arguments must be non-negative")
  }
  if (number %% 1 != 0 || sample_size %% 1 != 0) {
    stop("Arguments must be whole numbers")
  }
  if (sample_size < number) {
    stop ("sample_size is less than the number of folds")
  }

  if (number > 1) {
    folds <- split(sample(1:sample_size, sample_size), rep(1:number, length.out = sample_size))
    folds <- lapply(folds, sort)
  } else {
    folds <- NULL
  }
  return(folds)
}
