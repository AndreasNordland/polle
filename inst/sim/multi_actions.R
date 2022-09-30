sim_multi_action <- function(n = 1e4, seed = NULL, return_model = FALSE){
  suppressPackageStartupMessages(require("lava"))
  if (!is.null(seed)) set.seed(seed)
  m <- lava::lvm() |>
    distribution(~ x+z, uniform.lvm()) |>
    regression(u ~ a+x+z, function(a,x,z)
    { x + z + (a==1)*(2+x) + (a==2)*(x*x+z) }) |>
    regression(a0 ~ x) |>
    intervention(a ~ a0, function(x) {
      if (x<(-1)) return(0)
      else if (x<0) return(1)
      else return(2)})

  if (return_model) return(m)
  lava::sim(m, n)
}
