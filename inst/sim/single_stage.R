sim_single_stage <- function(n=1e4,
                             par=c(k = .1,  d = .5, a = 1, b = -2.5, c = 3, p = .3),
                             action_model = function(Z, L, B, k, d){
                               k*(Z + L - 1)*Z^(-2) + d*(B==1)
                             },
                             utility_model = function(Z, L, A, a, b, c){
                               Z + L + A*(c*Z + a*L + b)
                             },
                             seed=NULL,
                             return_model=FALSE, ...) {
  suppressPackageStartupMessages(require("lava"))
  if (!is.null(seed)) set.seed(seed)
  m <- lava::lvm()
  parameter(m) <- ~ a + b + c + d + k + p
  distribution(m, ~Z+L+B+A) <- list(uniform.lvm(),
                                    uniform.lvm(),
                                    binomial.lvm("identity"),
                                    binomial.lvm("logit"))
  regression(m, ~B) <- function(p){p}
  regression(m, ~A) <- action_model
  regression(m, ~U) <- utility_model

  if (return_model) return(m)
  lava::sim(m, n, p=par)
}
