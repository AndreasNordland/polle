test_that("predict.q_glmnet return a vector", {
  d <- sim_single_stage(200, seed = 1)
  pd <- policy_data(d,
      action = "A",
      covariates = list("Z", "B", "L"),
      utility = "U"
      )

  his <- get_history(pd, stage = 1)
  Q <- get_utility(pd)$U

  qfun <- fit_Q_function(history = his, Q = Q, q_model = q_glmnet())

  qm <- qfun$q_model

  AH <- cbind(A = get_A(his), get_H(his))
  pred <- predict.q_glmnet(qm, new_AH = AH)

  expect_true(
      is.vector(pred)
  )
})

test_that("q_glmnet formats data correctly via the formula", {
    d1 <- sim_single_stage(200, seed = 1)
    d1$BB <- sample(c("group 1", "group & 2", "group & 3"), size = 200, replace = TRUE)
    pd1 <- policy_data(d1,
        action = "A",
        covariates = list("Z", "B", "L", "BB"),
        utility = "U"
    )

    expect_error(
        pe <- policy_eval(
            policy_data = pd1,
            policy_learn = policy_learn(type = "ql", alpha = 0.05),
            q_models = q_glmnet(formula = ~ A * .)
        ),
        NA
    )
})


test_that("q_glmnet handles missing covariates", {
    d <- sim_two_stage(2e3, seed = 1)
    d$C_1 <- NULL
    pd <- policy_data(d,
        action = c("A_1", "A_2"),
        baseline = c("BB", "B"),
        covariates = list(
            L = c("L_1", "L_2"),
            C = c(NA, "C_2")
        ), # C_1 is missing
        utility = c("U_1", "U_2", "U_3")
    )
    p <- policy_def(1, reuse = TRUE)

    expect_error(
        suppressWarnings({
            pe <- policy_eval(
                policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet()
            )
        })
    )


    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            type = "or",
            q_models = q_glmnet(~ L + A)
        ),
        NA
    )
    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            type = "or",
            q_models = list(q_glmnet(~ L + A), q_glmnet())
        ),
        NA
    )

    d <- sim_two_stage(2e3, seed = 1)
    d$C_1[1:10] <- NA
    pd <- policy_data(d,
        action = c("A_1", "A_2"),
        baseline = c("BB", "B"),
        covariates = list(
            L = c("L_1", "L_2"),
            C = c("C_1", "C_2")
        ),
        utility = c("U_1", "U_2", "U_3")
    )

    expect_error(
        expect_warning(
            policy_eval(
                policy_data = pd,
                policy = p,
                type = "or",
                q_models = q_glmnet()
            ),
            "The regression variables C have missing NA values."
        )
    )
})

test_that("testing that q_glmnet returns the prediction associated with lambda.min", {

  par0 <- list(a = 0, b = 0.0, c = 0)
  sim_d <- function(n,
                    par = par0,
                    potential_outcomes = FALSE,
                    action_function = function(H){
                      n <- nrow(H)
                      rbinom(n = n, size = 1, prob = 0.5)
                    }) {
    W <- runif(n = n, min = -1, max = 1)
    L <- runif(n = n, min = -1, max = 1)
    H <- data.table(W = W, L = L)
    A <- as.numeric(action_function(H))
    U1 <- W + L + (par$c*W + par$a*L + par$b) # U^1
    U0 <- W + L # U^0
    U <- A * U1 + (1 - A) * U0 + rnorm(n = n)
    out <- data.table(W = W, L = L, A = A, U = U)
    if (potential_outcomes == TRUE) {
      out$U0 <- U0
      out$U1 <- U1
    }
    return(out)
  }

  set.seed(1)
  d<-sim_d(400)

  pd <- policy_data(
    d,
    action = "A",
    covariates = list("W", "L"),
    utility = "U"
  )
  his <- get_history(pd)

  pl <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glmnet(~.)),
    threshold=0
  )

  po <- pl(
    policy_data = pd,
    g_models = g_glm(~1),
    q_models = q_glmnet(~.*A)
  )

  qf <- get_q_functions(po)

  ## fitted values of the Q-function using s = "lambda.min"
  expect_equal(
    qf$stage_1$q_model$s,
    "lambda.min"
  )
  pred_lambda.min <- predict(qf, pd)

  ## predictions from a cv.glmnet object uses s = "lambda.1se" per default
  ## see https://cran.r-project.org/web/packages/glmnet/glmnet.pdf#page=59
  m_glmnet <- qf$stage_1$q_model$model
  ## applying the design (formula) to the action and history matrix:
  pred_data <- apply_design(design = qf$stage_1$q_model$design, data = cbind(A = "1", his$H))
  pred_lambda.min_ref <- predict(qf$stage_1$q_model$model, newx = pred_data, s = "lambda.min")

  expect_equal(
    pred_lambda.min$Q_1,
    pred_lambda.min_ref[,1] |> unname()
  )
})
