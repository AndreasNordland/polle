test_that("policy_eval checks inputs",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d <- sim_single_stage(1e2, seed=1)
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")

  p <- policy_def(1)
  pl <- policy_learn()

  # policy_data
  expect_error(
    policy_eval(
      policy_data = d,
      policy = p
    ),
    "policy_data must be of inherited class 'policy_data'."
  )

  # policy
  expect_error(
    policy_eval(
      policy_data = pd
    ),
    "Provide either policy or policy_learn."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = function() 1
    ),
    "policy must be of inherited class 'policy'."
  )

  # policy_learn
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = NULL,
      policy_learn = p
    ),
    "policy_learn must be of inherited class 'policy_learn'."
  )
  expect_error(
    policy_eval(
      policy_data = pd,
      policy = p,
      policy_learn = p
    ),
    "Provide either policy or policy_learn."
  )

  # g-models
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_model = g_glm()),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_models = list(g_glm())),
    NA
  )
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_model = function() 1),
    "g_models must be a single g_model or a list of K g_models's."
  )

  gfun <- fit_g_functions(pd, g_glm(), FALSE)
  qfun <- fit_Q_functions(pd, q_models = q_glm(), policy_actions = p(pd))

  # g-functions
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                g_functions = gfun),
    NA
  )
  expect_error(
    pe <- policy_eval(policy_data = pd,
                policy = p,
                g_functions = qfun),
    "g_functions must be of class 'g_functions'."
  )

  # q-functions
  expect_error(
    policy_eval(policy_data = pd,
                policy = p,
                q_functions = qfun),
    NA
  )
  expect_error(
    pe <- policy_eval(policy_data = pd,
                      policy = p,
                      q_functions = gfun),
    "q-functions must be of class 'q_functions'."
  )
})



test_that("policy_eval evaluates on a subset of the data with missing actions",{
  source(system.file("sim", "single_stage.R", package="polle"))
  d1 <- sim_single_stage(1e2, seed=1)
  pd1 <- policy_data(d1, action = "A", covariates = c("Z"), utility = "U")

  pd2 <- subset(pd1, id = get_id(pd1)[d1$A == "0"])

  ### ipw
  expect_error(
    pe1_ipw <- policy_eval(pd1, policy = policy_def(1), type = "ipw"),
    NA
  )
  expect_error(
    pe2_ipw <- policy_eval(policy_data = pd2,
                           g_functions = get_g_functions(pe1_ipw),
                           policy = policy_def(1),
                           type = "ipw"),
    NA
  )
  expect_equal(
    pe2_ipw$value_estimate,
    0
  )
  expect_equal(
    pe2_ipw$IC,
    (pe1_ipw$IC[d1$A == "0"] + pe1_ipw$value_estimate)
  )

  ### or
  expect_error(
    pe1_or <- policy_eval(pd1, policy = policy_def(1), type = "or"),
    NA
  )
  expect_error(
    pe2_or <- policy_eval(policy_data = pd2,
                          q_functions = get_q_functions(pe1_or),
                          policy = policy_def(1),
                          type = "or"),
    NA
  )
  expect_equal(
    pe2_or$IC + pe2_or$value_estimate,
    (pe1_or$IC[d1$A == "0"] + pe1_or$value_estimate)
  )
})

test_that("policy_eval handles varying stage action sets",{
  source(system.file("sim", "two_stage_multi_actions.R", package="polle"))
  d <- sim_two_stage_multi_actions(n = 1e2)

  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))

  p <- policy_def(
    c("yes", "no")
  )
  expect_error(
    pe <- policy_eval(pd,
                policy = p,
                type = "dr",
                g_models = list(g_glm(), g_rf())),
    NA
  )

  p <- policy_def(
    c("default", "default")
  )
  expect_error(
    policy_eval(pd,
                policy = p,
                type = "dr",
                g_models = list(g_glm(), g_rf())),
    "The policy actions does not comply with the stage action sets of the policy data object."
  )

})
