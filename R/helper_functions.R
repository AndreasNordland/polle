am <- Vectorize(
  function(a, action_set){
    am <- action_set %in% a
    return(am)
  },
  vectorize.args = "a"
)
action_matrix <- function(a, action_set){
  t(am(a = a, action_set = action_set))
}

get_a_values <- function(a, action_set, values){

  stopifnot(
    is.data.table(values),
    all(key(values) == c("id", "stage")),
    length(action_set) == ncol(values[ ,-c("id", "stage"), with = FALSE]),
    length(a) == nrow(values)
  )

  P <- action_matrix(a = a, action_set = action_set) * values[ ,-c("id", "stage"), with = FALSE]
  P <- apply(P, 1, sum)
  out <- data.table(values[ , c("id", "stage"), with = FALSE], P = P)

  return(out)
}


ipw_weight <- function(D, G){
  stopifnot(
    is.vector(D) | is.matrix(D),
    is.vector(G) | is.matrix(G),
    all(dim(G) == dim(D))
  )

  if(is.vector(D)){
    out <- D / G
  } else{
    out <- apply(D / G, 1, prod, na.rm = TRUE)
  }

  stopifnot(
    !any(is.na(out))
  )

  return(out)
}

colprod <- function(M){
  if(is.vector(M)){
    out <- M
  } else{
    out <- apply(M, 1, prod, na.rm = TRUE)
  }
  return(out)
}

remove_null_elements <- function(x)  x[!sapply(x, is.null)]
