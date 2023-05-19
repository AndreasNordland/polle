test_that("policy_learn with type drql works as intended",{
  d <- sim_two_stage(200, seed=1)

  pd <- policy_data(d,
                    action = c("A_1", "A_2"),
                    baseline = c("BB", "B"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  pl <- policy_learn(type = "blip",
                     control = control_blip())

})
