
test_that("q_rf",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    policy_eval(
      policy_data = pd1,
      policy_learn = policy_learn(type = "rql", alpha = 0.05),
      g_models = g_glm(),
      g_full_history = FALSE,
      q_models = q_rf()
    ),
    NA
  )

})

test_that("q_sl",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(200, seed=1)
  d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
  pd1 <- policy_data(d1,
                     action="A",
                     covariates = list("Z", "B", "L", "BB"),
                     utility="U")

  expect_error(
    suppressWarnings({
      policy_eval(
        policy_data = pd1,
        policy_learn = policy_learn(type = "rql", alpha = 0.05),
        g_models = g_glm(),
        g_full_history = FALSE,
        q_models = q_sl()
      ) }),
    NA
  )
})
