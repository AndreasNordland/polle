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


##   args <- list(
##     target = "value",
##     type = "dr",
##     policy = p,
##     policy_learn = NULL,
##     g_models = g_glm(~1),
##     g_functions = NULL,
##     g_full_history = FALSE,
##     save_g_functions = TRUE,
##     q_models = q_degen(var = "z"),
##     q_functions = NULL,
##     q_full_history = FALSE,
##     save_q_functions = TRUE,
##     min_subgroup_size = 1,
##     name = "test"
##   )

##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     args = args,
##     train_block_size = 40,
##     M = 2
##   )

##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     args = args,
##     train_block_size = 40,
##     M = 1
##   )


##   pl <- policy_learn(
##     type = "blip",
##     threshold = c(25, 75),
##     control = control_blip(blip_models = q_degen(var = "z"))
##   )

##   args <- list(
##     target = "value",
##     type = "dr",
##     policy = NULL,
##     policy_learn = pl,
##     g_models = g_glm(~1),
##     g_functions = NULL,
##     g_full_history = FALSE,
##     save_g_functions = TRUE,
##     q_models = q_degen(var = "z"),
##     q_functions = NULL,
##     q_full_history = FALSE,
##     save_q_functions = TRUE,
##     min_subgroup_size = 1,
##     name = "test"
##   )

##   set.seed(1)
##   pe <- policy_eval_sequential(
##     policy_data = pd,
##     args = args,
##     train_block_size = 40,
##     M = 2
##   )

##   coef(pe)

##   vcov(pe)


## })
