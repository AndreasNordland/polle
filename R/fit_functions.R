fit_functions <- function(policy_data,
                          type = type,
                          policy = NULL, policy_learn = NULL,
                          g_models = NULL, g_functions = NULL, g_full_history,
                          q_models = NULL, q_functions = NULL, q_full_history){

  # fitting the g-functions (if NULL and if g_models is not NULL):
  if (is.null(g_functions)){
    # g-models are not fitted if type is "or".
    if (!is.null(g_models) & (type %in% c("dr", "ipw"))){
      g_functions <- fit_g_functions(policy_data, g_models = g_models, full_history = g_full_history)
    }
  }

  # learning the policy:
  policy_object <- NULL
  if (is.null(policy)){
    policy_object <- policy_learn(
      policy_data = policy_data,
      g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
      q_models = q_models, q_full_history = q_full_history
    )
    policy <- get_policy(policy_object)
  }

  # getting the policy actions:
  policy_actions <- policy(policy_data = policy_data)

  # fitting the Q-functions (if NULL and if q_models is not NULL):
  if(is.null(q_functions)){
    if(!is.null(getElement(policy_object, "q_functions"))){
      q_functions <- getElement(policy_object, "q_functions")
    } else{
      # q-models are not fitted if type is "ipw".
      if (!is.null(q_models) & (type %in% c("dr", "or"))){
        q_functions <- fit_Q_functions(policy_data,
                                       policy_actions = policy_actions,
                                       q_models = q_models, full_history = q_full_history)
      }
    }
  }

  out <- list(g_functions = g_functions,
              q_functions = q_functions,
              policy_object = policy_object)

  return(out)

}
