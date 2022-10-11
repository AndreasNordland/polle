# sim_multi_action <- function(n = 1e4, seed = NULL, return_model = FALSE){
#   suppressPackageStartupMessages(require("lava"))
#   if (!is.null(seed)) set.seed(seed)
#   m <- lava::lvm() |>
#     distribution(~ x+z, uniform.lvm()) |>
#     regression(u ~ a+x+z, function(a,x,z)
#     { x + z + (a==2)*(x-0.5) + (a==1)*(x*x+z-0.5) }) |>
#     regression(a0 ~ x) |>
#     intervention(a ~ a0, function(x) {
#       if (x<(-1)) return(0)
#       else if (x<0.5) return(1)
#       else return(2)})
#
#   if (return_model) return(m)
#   lava::sim(m, n)
# }

sim_multi_action <- function(n = 1e4, seed = NULL){
  if (!is.null(seed)) set.seed(seed)

  z <- runif(n)
  x <- runif(n)
  a0 <- rnorm(mean = x, n = n)
  afun <- Vectorize(function(y){
    if (y<(-1)) return(0)
    else if (y<0.5) return(1)
    else return(2)
  })
  a <- afun(a0)
  u <- rnorm(
    mean = x + z + (a==2)*(x-0.5) + (a==1)*(x*x+z-0.5),
    n = n
  )

  d <- data.frame(
    z = z,
    x = x,
    a = a,
    u = u
  )
  return(d)
}
