fit_functions <- function(policy_data,
                             policy = NULL, policy_learner = NULL,
                             g_models = NULL, g_functions = NULL, g_full_history,
                             q_models = NULL, q_functions = NULL, q_full_history, ...){

  # if ((is.null(g_models) & is.null(g_functions)) | (!is.null(g_functions) & !is.null(g_models))) stop("Provide either g-models or g-functions.")
  # if (!is.null(g_functions)){
  #   if(!(class(g_functions)[[1]] == "nuisance_functions")) stop("g-functions must be of class 'nuisance_functions'.")
  # }
  #
  # if ((is.null(q_models) & is.null(q_functions)) | (!is.null(q_models) & !is.null(q_functions))) stop("Provide either q-models or q-functions.")
  # if (!is.null(q_functions)){
  #   if(!(class(q_functions)[[1]] == "nuisance_functions")) stop("q-functions must be of class 'nuisance_functions'.")
  # }

  if ((is.null(policy) & is.null(policy_learner)) | (!is.null(policy_learner) & !is.null(policy))) stop("Provide either policy or policy_learner.")

  # fitting the g-functions (if g_models is not NULL):
  if (is.null(g_functions)){
    if (!is.null(g_models)){
      g_functions <- fit_g_functions(policy_data, models = g_models, full_history = g_full_history)
    }
  }

  # learning the policy:
  policy_object <- NULL
  if (is.null(policy)){
    policy_object <- policy_learner(
      policy_data = policy_data,
      g_models = g_models, g_functions = g_functions, g_full_history = g_full_history,
      q_models = q_models, q_functions = q_functions, q_full_history = q_full_history,
      ...
    )
    policy <- get_policy(policy_object)
  }

  # getting the policy actions:
  policy_actions <- policy(policy_data = policy_data)

  # fitting the Q-functions:
  if(is.null(q_functions)){
    if(!is.null(policy_object$q_functions)){
      q_functions <- policy_object$q_functions
    } else{
      if (!is.null(q_models)){
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
