# polle 1.6.7
    - `q_sl()` and `g_sl()` are reinstated, with `SuperLearner (>=2.0-42)`
    in `Suggests` rather than `Depends`.

# polle 1.6.6
    - bug fix: the risk set used by `fit_c_functions()` and
        `predict.c_functions()` now correctly excludes subjects whose
        terminal event has already occurred (events in {0, 2} at stages 1..K
        and {1, 2} at stage K+1).
    - bug fix: `policy_eval(type = "dr")$coef_ipw` is now correctly
        computed when some subjects terminate before stage K+1.
    - breaking: `policy_eval(type = "ipw")` and `policy_eval(type = "or")`
        are no longer supported. The accompanying inverse-probability-weighted
        and outcome-regression-only estimates remain available on a
        `type = "dr"` fit as `coef_ipw` and `coef_or`.
        Calling `policy_eval()` with `type = "ipw"` or `type = "or"` now
        raises an informative error pointing at the migration path.
    

# polle 1.6.5
    - `SuperLearner` was removed from the package dependencies in
      anticipation of its CRAN archival; it has since been reinstated as
      `Suggests`, see the 1.6.6 entry above. `q_sl()` and `g_sl()` were
      temporarily unavailable in this release.
    - `control_owl()` gains a `solver` argument, defaulting to `"ipop"`.
      `DTRlearn2` (>= 2.1) changed the default weighted-SVM solver of
      `DTRlearn2::owl()` to `"svm"` (via `WeightSVM::wsvm`)
    - bug fix: final-outcome and discrete-censoring model fitting to complete after
        warnings, retaining the fitted models and preserving the learner warnings.
    - bug fix: set Q_{K+1} = U for early terminal events under missing final outcome

# polle 1.6.4
    - new citation, <doi:10.18637/jss.v116.i04>, Journal of Statistical Software
    
# polle 1.6.3
    - policy_def() gains natural_action and stage_number arguments
    - bug fix: c_cox() now passes time, time2 and event to mets::phreg

# polle 1.6.1
    - policy learning and evaluation under right-censoring/missing outcome

# polle 1.6.0
    - online estimation/sequential validation for (subgroup) policy evaluation
    - improved functionality for subgroup analysis (subgroup average treatment effect)
    
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
