## test_that("policy_eval_sequential has the correct output in the single stage case", {

##   z <- 1:1e2
##   a <- c(rep(1, 50), rep(2, 50))
##   y <- a * 2
##   p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
##   d <- data.table(z = z, a = a, y = y, p = p)
##   rm(a, z, y)
##   pd <- policy_data(
##     data = d,
##     action = "a",
##     covariates = c("z", "p"),
##     utility = c("y")
##   )

##   p <- policy_def(function(p) p, name = "test")

##   ## ref_pe <- mean((d$a == d$p) / 0.5 * (d$y - d$z) + d$z)
##   ## ref_IC <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z - ref_pe

##   set.seed(1)
##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     policy = p,
##     target = "value",
##     g_models = g_glm(~1),
##     q_models = q_degen(var = "z"),
##     train_block_size = 60,
##     M = 2
##   )

##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     policy = p,
##     target = "value",
##     g_models = g_glm(~1),
##     q_models = q_degen(var = "z"),
##     train_block_size = 60,
##     M = 1
##   )


##   pl <- policy_learn(
##     type = "blip",
##     threshold = c(25, 75),
##     control = control_blip(blip_models = q_degen(var = "z"))
##   )

##   set.seed(1)
##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     policy_learn = pl,
##     target = "value",
##     g_models = g_glm(~1),
##     q_models = q_degen(var = "z"),
##     train_block_size = 60,
##     M = 2
##   )

##   coef(pe)

##   vcov(pe)


## })

## test_that("policy_eval_sequential has the correct output in the single stage case for target = 'subgroup'", {

##   z <- 1:1e2
##   a <- c(rep(1, 50), rep(2, 50))
##   y <- a * 2
##   p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
##   d <- data.table(z = z, a = a, y = y, p = p)
##   rm(a, z, y)
##   pd <- policy_data(
##     data = d,
##     action = "a",
##     covariates = c("z", "p"),
##     utility = c("y")
##   )

##   p <- policy_def(function(p) p, name = "test")

##   ## ref_pe <- mean((d$a == d$p) / 0.5 * (d$y - d$z) + d$z)
##   ## ref_IC <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z - ref_pe

##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     policy = p,
##     target = "subgroup",
##     g_models = g_glm(~1),
##     q_models = q_degen(var = "z"),
##     train_block_size = 60,
##     M = 2
##   )

##   pl <- policy_learn(
##     type = "blip",
##     threshold = c(25, 75),
##     control = control_blip(blip_models = q_degen(var = "z"))
##   )

##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     policy_learn = pl,
##     target = "subgroup",
##     g_models = g_glm(~1),
##     q_models = q_degen(var = "z"),
##     train_block_size = 60,
##     M = 2
##   )

##   coef(pe)

##   vcov(pe)
## })
