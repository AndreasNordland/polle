fit_functions <- function(policy_data,
                          type,
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
      q_models = q_models, q_full_history = q_full_history,
      c_models = c_models, c_functions = c_functions, c_full_history = c_full_history,
      m_model = m_model, m_function = m_function, m_full_history = m_full_history
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

#' Cross-fit functions
#'
#' \code{crossfit_function()} is used to cross-fit a list of c-models
#'
#' @param folds List of vectors of indices for each validation fold, see examples.
#' @param policy_data Policy data object created by [policy_data()].
#' @param fun Function used to fit the models, either \code{fit_g_functions()}, \code{fit_c_functions()}
#' @param models single model or list of models like [g_sl()].
#' @param full_history If TRUE, the full history is used to fit each model.
#' If FALSE, the single stage/"Markov type" history is used to fit each model.
#' @param save_models Logical. Should the cross-fitted models be saved.
#' @param future_args arguments passed to [future.apply::future_lapply].
#' @noRd
crossfit_function <- function(folds,
                              policy_data,
                              fun,
                              models,
                              full_history,
                              save_cross_fit_models = FALSE,
                              future_args = list(future.seed = TRUE)){
  id <- get_id(policy_data)
  K <- get_K(policy_data)
  force(fun)
  force(models)
  force(full_history)

  future_args <- append(
    future_args,
    list(X = folds,
         FUN = function(f){
           train_id <- id[-f]
           train_policy_data <- subset_id(policy_data, train_id)
           if (get_K(train_policy_data) != K)
             stop("The number of stages K varies across the training policy data folds.")
           train_functions <- fun(policy_data = train_policy_data,
                                  models,
                                  full_history = full_history)

           valid_id <- id[f]
           valid_policy_data <- subset_id(policy_data, valid_id)
           valid_values <- predict(train_functions, valid_policy_data)

           if (save_cross_fit_models == FALSE)
             train_functions <- NULL

           out <- list(
             train_functions = train_functions,
             valid_values = valid_values,
             valid_id = valid_id
           )
           return(out)
         })
  )

  fit_cf <- do.call(what = future.apply::future_lapply, future_args)
  fit_cf <- simplify2array(fit_cf)

  functions <- fit_cf["train_functions", ]
  values <- fit_cf["valid_values", ]
  valid_ids <- fit_cf["valid_id", ]

  values <- rbindlist(values)
  setkeyv(values, c("id", "stage"))

  out <- list(
    functions = functions,
    values = values,
    valid_ids = valid_ids
  )
  return(out)
}
