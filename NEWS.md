# polle 1.0
    - Package documentation: https://arxiv.org/abs/2212.02335

# polle 1.2
    - action sets can now vary across stages (stage_action_sets)
    - g_empir() is a new g-model useful for calculating the empirical (conditional)
    probabilities
    - conditional() estimates the group specific policy value estimates
    - progressr is now implemented for policy_eval()
    - sim_single_stage(), sim_two_stage(), and sim_multi_stage() are new functions
    for simulating data.

# polle 1.3
    - new policy_learn type: "blip". Similar to "drql", but only a single model is fitted using the doubly robust score for the blip.
    - q_sl now uses the folds from policy_learn when it is used a policy model for "blip" and "drql".
    - g_xgboost and q_xgboost.
