test_that("policy_eval with target = 'sub_effect' checks inputs.", {
    d <- sim_single_stage(1e2, seed = 1)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            target = "test"
        ),
        "target must be either 'value' or 'subgroup'."
    )

    d <- sim_single_stage_multi_actions(1e2, seed = 1)
    pd <- policy_data(d, action = "a", covariates = c("z"), utility = "u")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup average treatment effect evaluation is not implemented for more than two actions."
    )

    d <- sim_two_stage(1e2, seed = 1)
    pd <- policy_data(
        data = d,
        action = c("A_1", "A_2"),
        covariates = list(L = c("L_1", "L_2")),
        utility = "U_3"
    )
    p <- policy_def(1, reuse = TRUE)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup average treatment effect evaluation is not implemeted for multiple stages."
    )
})


test_that("policy_eval with target 'subgroup' agrees with targeted::cate.", {
    n <- 1e3
    Z <- rnorm(n = n)
    A <- rbinom(size = 1, n = n, prob = 0.5)
    U <- rnorm(mean = Z + Z * A, n = n)
    d <- data.table(Z = Z, A = A, U = U)
    rm(Z, A, U)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    ##
    ## no cross-fitting:
    ##

    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "sub_effect",
        M = 1
    )

    ## implementation from the targeted package:
    d$d <- p(pd)$d

    ca <- targeted::cate(
        cate_model = ~ factor(d) - 1,
        response_model = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 1,
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)[c("factor(d)1", "factor(d)0")])
    )

    expect_equal(
        IC(pe) |> unname(),
      IC(ca)[, c("factor(d)1", "factor(d)0"), drop=FALSE] |>
      unname()
    )

    ##
    ## cross-fitting: pooled estimate and variance
    ##

    set.seed(1)
    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "sub_effect",
        cross_fit_type = "pooled",
        variance_type = "pooled",
        M = 2
    )

    set.seed(1)
    ca <- targeted::cate(
        cate_model = ~ factor(d) - 1,
        response_model = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 2,
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)[c("factor(d)1", "factor(d)0")])
    )

    expect_equal(
        IC(pe),
      IC(ca)[, c("factor(d)1", "factor(d)0")] |>
      unname()
    )

})

test_that("policy_eval with target 'sub_effect' has the correct outputs: test1.", {
    test_output <- function(pe) {
        ## value_estimate
        expect_true(
            !is.null(coef(pe)) && is.numeric(coef(pe))
        )

        ## IC in group d=1 should be zero for Z < 0
        expect_true(
            all(IC(pe)[d$Z <= 0, 1] == 0)
        )
        ## ## ... and for the other subgroup:
        ## expect_true(
        ##     all(IC(pe)[d$Z >0, 2] == 0)
        ## )

        ## id
        expect_true(
            all(pe$id == 1:1e2)
        )

        ## type
        expect_equal(
            pe$type,
            "dr"
        )

        ## target
        expect_equal(
            pe$target,
            "subgroup"
        )
    }

    d <- sim_single_stage(1e2, seed = 1)
    d$A <- c(rep(0, 50), rep(1, 50))
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    ## no cross-fitting
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup"
        )
    )
    test_output(pe)

    ## cross-fitting: stacked estimator
    set.seed(1)
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup",
            M = 2,
            cross_fit_type = "stacked",
            variance_type = "stacked"
        )
    )
    test_output(pe)

    ## cross-fitting: pooled estimator
    set.seed(1)
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "sub_effect",
            M = 2,
            cross_fit_type = "pooled",
            variance_type = "pooled"
        )
    )
    test_output(pe)

    ## cross-fitting: pooled estimator, complete variance estimate
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup",
            M = 2,
            cross_fit_type = "pooled",
            variance_type = "complete"
        )
    )
    test_output(pe)
})

test_that("policy_eval with target 'subgroup' has the correct outputs: test2.", {
    z <- 1:1e2
    a <- c(rep(1, 50), rep(2, 50))
    y <- a * 2
    p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
    d <- data.table(z = z, a = a, y = y, p = p)
    rm(a, z, y)
    pd <- policy_data(
        data = d,
        action = "a",
        covariates = c("z", "p"),
        utility = c("y")
    )

    ## his <- get_history(pd, stage = 1)
    ## qfun <- fit_Q_function(history = his, Q = d$y, q_degen(var = "z"))
    ## predict.Q_function(qfun, new_history = his)

    p <- policy_def(function(p) p, name = "p")

    ref_Z <- cbind(
        (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
        (d$a == 2) / 0.5 * (d$y - d$z) + d$z
    )
    ref_blip <- ref_Z[, 2] - ref_Z[, 1]
    ref_sub <- mean(ref_blip[d$p == 2])
    ref_sub_comp <- mean(ref_blip[d$p == 1])
    ref_IC <- 2 * (d$p == 2) * (ref_blip - ref_sub)
    ref_IC_comp <- 2 * (d$p == 1) * (ref_blip - ref_sub_comp)

    ##
    ## no cross-fitting
    ##

    sub <- policy_eval(
        target = "subgroup",
        policy_data = pd,
        policy = p,
        q_models = polle:::q_degen(var = "z"),
        g_models = g_glm(~1)
    )

    expect_equal(
        coef(sub) |> unname(),
        c(ref_sub, ref_sub_comp)
    )

    expect_equal(
        IC(sub) |> unname(),
        cbind(ref_IC, ref_IC_comp) |> unname()
    )

    expect_equal(
      names(sub$coef),
      c("E[U(2)-U(1)|d=2]: d=p", "E[U(2)-U(1)|d=1]: d=p")
    )

    ##
    ## cross-fitting
    ##

    ## in each training, the empirical propensity is no longer 0.5
    ## instead a g_model is fitted on the complete data:
    gf <- fit_g_functions(pd, g_models = g_glm(~1))

    sub <- policy_eval(
        target = "sub_effect",
        policy_data = pd,
        policy = p,
        q_models = polle:::q_degen(var = "z"),
        g_functions = gf,
        M = 2,
    )

    expect_equal(
        coef(sub) |> unname(),
        c(ref_sub, ref_sub_comp)
    )

    expect_equal(
        IC(sub) |> unname(),
        cbind(ref_IC, ref_IC_comp) |> unname()
    )
})

test_that("policy_eval with target 'subgroup' returns NA when no subjects are in the subgroup.", {

  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p1 <- c(rep(1, 50), rep(2, 50))
  p2 <- c(rep(1, 100))

  d <- data.table(z = z, a = a, y = y, p1 = p1, p2 = p2)
  rm(a, z, y, p1, p2)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p1", "p2"),
    utility = c("y")
  )

  ref_Z <- cbind(
  (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
  (d$a == 2) / 0.5 * (d$y - d$z) + d$z
  )
  ref_blip <- ref_Z[, 2] - ref_Z[, 1]


  p <- policy_def(1)

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )

  expect_equal(
    coef(sub) |> unname(),
    c(as.numeric(NA), mean(ref_blip))
  )

  expect_equal(
    unname(IC(sub)[,1,drop=FALSE]),
    cbind(rep(as.numeric(NA), 1e2))
  )

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

  ## cross-fitting

  ## in each training, the empirical propensity is no longer 0.5
  ## instead a g_model is fitted on the complete data:
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  set.seed(1)
  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    M = 2
  )

  expect_equal(
    coef(sub) |> unname(),
    c(as.numeric(NA), mean(ref_blip))
  )

  expect_equal(
    unname(IC(sub)[,1,drop=FALSE]),
    cbind(rep(as.numeric(NA), 1e2))
  )

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

  set.seed(1)
  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    M = 2,
    cross_fit_type = "stacked",
    variance_type = "stacked"
  )

  expect_equal(
    coef(sub) |> unname(),
    c(as.numeric(NA), mean(ref_blip))
  )

  expect_equal(
    unname(IC(sub)[,1,drop=FALSE]),
    cbind(rep(as.numeric(NA), 1e2))
  )

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

})

test_that("policy_eval with target 'subgroup' works with policy_learning with multiple thresholds.", {

  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p1 <- c(rep(1, 50), rep(2, 50))
  p2 <- c(rep(1, 100))

  d <- data.table(z = z, a = a, y = y, p1 = p1, p2 = p2)
  rm(a, z, y)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p1", "p2"),
    utility = c("y")
  )

  ref_Z <- cbind(
  (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
  (d$a == 2) / 0.5 * (d$y - d$z) + d$z
  )
  ref_blip <- ref_Z[, 2] - ref_Z[, 1]

  ref_sub2_eta50 <- mean(ref_blip[d$p1 == 2])
  ref_IC2_eta50 <- 2 * (d$p1 == 2) * (ref_blip - ref_sub2_eta50)
  ref_sub1_eta50 <- mean(ref_blip[d$p1 == 1])
  ref_IC1_eta50 <- 2 * (d$p1 == 1) * (ref_blip - ref_sub1_eta50)

  ref_sub1_eta101 <- mean(ref_blip[d$p2 == 1])
  ref_IC1_eta101 <- (d$p2 == 1) * (ref_blip - ref_sub1_eta101)

  pl <- policy_learn(
    type = "blip",
    threshold = c(50, 101),
    control = control_blip(blip_models = polle:::q_degen(var = "z"))
  )

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

  expect_equal(
    sub$name,
    names(sub$coef)
  )

  expect_equal(
    sub$name,
    c("E[U(2)-U(1)|d=2]: d=blip(eta=50)", "E[U(2)-U(1)|d=1]: d=blip(eta=50)",
      "E[U(2)-U(1)|d=2]: d=blip(eta=101)", "E[U(2)-U(1)|d=1]: d=blip(eta=101)")
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub2_eta50, ref_sub1_eta50, as.numeric(NA), ref_sub1_eta101)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC2_eta50, ref_IC1_eta50, rep(as.numeric(NA), 1e2), ref_IC1_eta101) |> unname()
  )

  expect_equal(
    sub$subgroup_indicator,
    cbind(p1 == 2, p1 == 1, p2 == 2, p2 == 1)
  )

  ## cross-fitting:

  ## in each training, the empirical propensity is no longer 0.5
  ## instead a g_model is fitted on the complete data:
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  set.seed(1)
  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    cross_fit_type = "pooled",
    variance_type = "pooled",
    M = 2)

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

  expect_equal(
    sub$name,
    names(sub$coef)
  )

  expect_equal(
    sub$name,
    c("E[U(2)-U(1)|d=2]: d=blip(eta=50)", "E[U(2)-U(1)|d=1]: d=blip(eta=50)",
      "E[U(2)-U(1)|d=2]: d=blip(eta=101)", "E[U(2)-U(1)|d=1]: d=blip(eta=101)")
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub2_eta50, ref_sub1_eta50, as.numeric(NA), ref_sub1_eta101)
  )

  expect_equal(
    IC(sub),
    cbind(ref_IC2_eta50, ref_IC1_eta50, rep(as.numeric(NA), 1e2), ref_IC1_eta101) |> unname()
  )

  expect_equal(
    sub$subgroup_indicator,
    cbind(p1 == 2, p1 == 1, p2 == 2, p2 == 1)
  )

  ## stacked:

  set.seed(1)
  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    M = 2,
    cross_fit_type = "stacked",
    variance_type = "stacked"
  )

  expect_no_error(
    tmp <- capture.output(print(sub))
  )

  expect_equal(
    sub$name,
    names(sub$coef)
  )

  expect_equal(
    sub$name,
    c("E[U(2)-U(1)|d=2]: d=blip(eta=50)", "E[U(2)-U(1)|d=1]: d=blip(eta=50)",
      "E[U(2)-U(1)|d=2]: d=blip(eta=101)", "E[U(2)-U(1)|d=1]: d=blip(eta=101)")
  )

  ref_sub2_eta50_fold1 <- mean(ref_blip[sub$folds[[1]]][d$p1[sub$folds[[1]]] == 2])
  ref_sub2_eta50_fold2 <- mean(ref_blip[sub$folds[[2]]][d$p1[sub$folds[[2]]] == 2])
  ref_sub1_eta50_fold1 <- mean(ref_blip[sub$folds[[1]]][d$p1[sub$folds[[1]]] == 1])
  ref_sub1_eta50_fold2 <- mean(ref_blip[sub$folds[[2]]][d$p1[sub$folds[[2]]] == 1])

  ref_sub2_eta101_fold1 <- mean(ref_blip[sub$folds[[1]]][d$p2[sub$folds[[1]]] == 2])
  ref_sub2_eta101_fold2 <- mean(ref_blip[sub$folds[[2]]][d$p2[sub$folds[[2]]] == 2])
  ref_sub1_eta101_fold1 <- mean(ref_blip[sub$folds[[1]]][d$p2[sub$folds[[1]]] == 1])
  ref_sub1_eta101_fold2 <- mean(ref_blip[sub$folds[[2]]][d$p2[sub$folds[[2]]] == 1])

  expect_equal(
    c(
      mean(c(ref_sub2_eta50_fold1, ref_sub2_eta50_fold2)),
      mean(c(ref_sub1_eta50_fold1, ref_sub1_eta50_fold2)),
      mean(c(ref_sub2_eta101_fold1, ref_sub2_eta101_fold2)),
      mean(c(ref_sub1_eta101_fold1, ref_sub1_eta101_fold2))

    ),
    coef(sub) |> unname()
  )

})

test_that("policy_eval with target 'subgroup' returns NA when the subgroup count is below the minimum.", {

  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p1 <- c(rep(1, 4), rep(2, 96))
  p2 <- c(rep(2, 4), rep(1, 96))
  d <- data.table(z = z, a = a, y = y, p1 = p1, p2 = p2)
  rm(a, z, y, p1, p2)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p1", "p2"),
    utility = c("y")
  )

  p1 <- policy_def(function(p1) p1, name = "p1") # 4 observations in the subgroup d = 1
  p2 <- policy_def(function(p2) p2, name = "p2") # 4 observations in the subgroup d = 2

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p1,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )

  expect_equal(
    coef(sub) |> is.na() |> unname(),
    c(FALSE, FALSE)
  )

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p1,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1),
    min_subgroup_size = 5
  )

  expect_equal(
    coef(sub) |> is.na() |> unname(),
    c(FALSE, TRUE)
  )

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p2,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1),
    min_subgroup_size = 5
  )

  expect_equal(
    coef(sub) |> is.na() |> unname(),
    c(TRUE, FALSE)
  )

  ## cross-fitting

  ## in each training, the empirical propensity is no longer 0.5
  ## instead a g_model is fitted on the complete data:
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p2,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    min_subgroup_size = 5,
    M = 2
  )

  expect_equal(
    coef(sub) |> is.na() |> unname(),
    c(TRUE, FALSE)
  )

  sub <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy = p2,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    min_subgroup_size = 5,
    M = 2,
    cross_fit_type = "stacked"
  )

  expect_equal(
    coef(sub) |> is.na() |> unname(),
    c(TRUE, FALSE)
  )

})
