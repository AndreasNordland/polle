
sim_single_stage <- function(n=1e4,
                             par=c(k = .1,  d = .5, a = 1, b = -2.5, c = 3),
                             A_function=function(Z, L, B, k, d) {
                               return(k*(Z + L - 1)*Z^(-2) + d*(B==1))
                             },
                             seed=NULL,
                             return_model=FALSE, ...) {
  suppressPackageStartupMessages(require("lava"))
  if (!is.null(seed)) set.seed(seed)
  m <- lava::lvm()
  parameter(m) <- ~ a + b + c + d + k
  distribution(m, ~Z+L+B+A) <- list(uniform.lvm(),
                                    uniform.lvm(),
                                    binomial.lvm(p=.3),
                                    binomial.lvm("logit"))
  regression(m, ~A) <- A_function
  regression(m, ~U) <-
    function(Z, L, A, a, b, c)  Z + L + A*(c*Z + a*L + b)
  if (return_model) return(m)
  lava::sim(m, n)
}
