# polle 1.5.1
    - documentation and print method for policy_eval() improved
    - multiple thresholds for policy_learn()
    - bug fixes: get_policy_functions(), predict.blip_function(), predict.q_glmnet()

# polle 1.5
    - added target argument in policy_eval for estimating the subgroup average treatment effect.
    - added threshold argument in policy_learn for learning the optimal subgroup.
    - added vignette for learning and evaluating the optimal subgroup.

# polle 1.4
    - vignettes added for policy_data, policy_learn and policy_eval.

# polle 1.3
    - new policy_learn type: "blip". Similar to "drql", but only a single model is fitted using the doubly robust score for the blip.
    - q_sl now uses the folds from policy_learn when it is used a policy model for "blip" and "drql".
    - g_xgboost and q_xgboost.

# polle 1.2
    - action sets can now vary across stages (stage_action_sets)
    - g_empir() is a new g-model useful for calculating the empirical (conditional)
    probabilities
    - conditional() estimates the group specific policy value estimates
    - progressr is now implemented for policy_eval()
    - sim_single_stage(), sim_two_stage(), and sim_multi_stage() are new functions
    for simulating data.

# polle 1.0
    - Package documentation: https://arxiv.org/abs/2212.02335
