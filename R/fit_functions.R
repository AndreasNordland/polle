fit_functions <- function(policy_data,
                          type = type,
                          policy = NULL, policy_learn = NULL,
                          g_models = NULL, g_functions = NULL, g_full_history,
                          q_models = NULL, q_functions = NULL, q_full_history,
                          c_models = NULL, c_functions = NULL, c_full_history,
                          m_model = NULL, m_function = NULL, m_full_history) {
  ## fitting the g-functions (if NULL and if g_models is not NULL):
  if (is.null(g_functions)) {
    # g-models are not fitted if type is "or".
    if (!is.null(g_models) && (type %in% c("dr", "ipw"))) {
      g_functions <- fit_g_functions(
        policy_data,
        g_models = g_models,
        full_history = g_full_history
      )
    }
  }

  ## fitting the c-functions (if NULL and if c_models is not NULL):
  if (is.null(c_functions)) {
    if (!is.null(c_models)) {
      c_functions <- fit_c_functions(
        policy_data,
        c_models = c_models,
        full_history = c_full_history
      )
    }
  }

  ## fitting m-function:
  if (is.null(m_function)) {
    m_function <- fit_m_function(
      policy_data,
      m_model = m_model,
      full_history = m_full_history
    )
  }

  ## learning the policy:
  policy_object <- NULL
  if (is.null(policy)) {
    policy_object <- policy_learn(
      policy_data = policy_data,
      g_models = g_models, g_functions = g_functions,
      g_full_history = g_full_history,
      q_models = q_models, q_full_history = q_full_history
    )
    ## getting the policy associated with the default threshold:
    ## the policy is only used to fit the Q-functions, and
    ## the threshold at stage 1 does not affect the learned
    ## policy at later stages and thus the fitted Q-funcion
    threshold <- get_element(policy_object, "threshold", check_name = FALSE)[1]
    policy <- get_policy(policy_object, threshold = threshold)
  }

  # getting the policy actions:
  policy_actions <- policy(policy_data = policy_data)

  ## the policy actions are needed to fit the Q-functions.
  ## note that the policy actions at the first stage will not
  ## affect the fit.

  # fitting the Q-functions (if NULL and if q_models is not NULL):
  if (is.null(q_functions)) {
    if (!is.null(getElement(policy_object, "q_functions"))) {
      q_functions <- get_element(policy_object, "q_functions")
    } else {
      # q-models are not fitted if type is "ipw".
      if (!is.null(q_models) && (type %in% c("dr", "or"))) {
        q_functions <- fit_Q_functions(
          policy_data,
          policy_actions = policy_actions,
          q_models = q_models, full_history = q_full_history,
          m_function = m_function
        )
      }
    }
  }

  out <- list(
    g_functions = g_functions,
    q_functions = q_functions,
    c_functions = c_functions,
    m_function = m_function,
    policy_object = policy_object
  )
  return(out)
}
