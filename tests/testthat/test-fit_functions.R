test_that("fit_functions handle multiple thresholds", {
    d <- sim_single_stage(200, seed = 1)
    pd <- policy_data(d,
        action = "A",
        covariates = list("Z", "B", "L"),
        utility = "U"
    )

    pl <- policy_learn(
        type = "blip",
        threshold = c(0, 1),
        control = control_blip()
    )

    expect_no_error(
        ff <- fit_functions(
            policy_data = pd,
            type = "dr",
            policy_learn = pl,
            g_models = g_glm(),
            g_full_history = FALSE,
            q_models = q_glm(),
            q_full_history = FALSE
        )
    )

    expect_is(ff$policy_object, class = "policy_object")
})
