test_that("policy_eval with target = 'subgroup' checks inputs.", {
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
            target = "subgroup"
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
            target = "subgroup"
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

    ## no cross-fitting:

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
        cate.model = ~ factor(d) - 1,
        response.model = U ~ A * Z,
        treatment.model = A ~ 1,
        data = d,
        second.order = FALSE,
        nfolds = 1,
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)[c("factor(d)1", "factor(d)0")])
    )

    expect_equal(
      IC(pe) |> unname(),
      IC(ca)[, c("factor(d)1", "factor(d)0"), drop=FALSE] |>
      unname(),
      check.attributes = FALSE
    )

    ## cross-fitting: pooled estimate and variance

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
        cate.model = ~ factor(d) - 1,
        response.model = U ~ A * Z,
        treatment.model = A ~ 1,
        second.order = FALSE,
        data = d,
        nfolds = 2,
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)[c("factor(d)1", "factor(d)0")])
    )

    expect_equal(
      IC(pe) |> unname(),
      IC(ca)[, c("factor(d)1", "factor(d)0")] |>
      unname(),
      check.attributes = FALSE
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

    ## name
    expect_equal(
      pe$name,
      names(pe$coef)
    )
    ## the object stores the 4 per-subgroup potential outcome means:
    expect_equal(
      pe$name,
      c(
        "E[U(1)|d=1]: d=test",
        "E[U(0)|d=1]: d=test",
        "E[U(1)|d=0]: d=test",
        "E[U(0)|d=0]: d=test"
      )
    )
    ## the subgroup average treatment effects reported by default:
    expect_equal(
      names(coef(pe)),
      c("E[U(1)-U(0)|d=1]: d=test", "E[U(1)-U(0)|d=0]: d=test")
    )
    expect_equal(
      pe$contrast_name,
      c("E[U(1)-U(0)|d=1]: d=test", "E[U(1)-U(0)|d=0]: d=test")
    )
  }

  d <- sim_single_stage(1e2, seed = 1)
  d$A <- c(rep(0, 50), rep(1, 50))
  pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
  p <- policy_def(function(Z) (Z > 0) * 1, name = "test")

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

    ## no cross-fitting

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
      cbind(ref_IC, ref_IC_comp) |> unname(),
      check.attributes = FALSE
    )

    expect_equal(
      names(sub$coef),
      c(
        "E[U(2)|d=1]: d=p",
        "E[U(1)|d=1]: d=p",
        "E[U(2)|d=0]: d=p",
        "E[U(1)|d=0]: d=p"
      )
    )
    expect_equal(
      names(coef(sub)),
      c("E[U(2)-U(1)|d=1]: d=p", "E[U(2)-U(1)|d=0]: d=p")
    )

    ## cross-fitting

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
      cbind(ref_IC, ref_IC_comp) |> unname(),
      check.attributes = FALSE
    )
})

test_that("policy_eval with target 'subgroup' exposes the 4 means and 2 contrasts via the contrast argument.", {
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
    pol <- policy_def(function(p) p, name = "p")

    ref_Z <- cbind(
        (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
        (d$a == 2) / 0.5 * (d$y - d$z) + d$z
    )
    ## the 4 per-subgroup potential outcome means
    ## [E[U(2)|d=2], E[U(1)|d=2], E[U(2)|d=1], E[U(1)|d=1]]:
    ref_means <- c(
        mean(ref_Z[d$p == 2, 2]),
        mean(ref_Z[d$p == 2, 1]),
        mean(ref_Z[d$p == 1, 2]),
        mean(ref_Z[d$p == 1, 1])
    )

    ref_IC <- cbind(
      2 * (d$p == 2) * (ref_Z[, 2] - ref_means[1]),
      2 * (d$p == 2) * (ref_Z[, 1] - ref_means[2]),
      2 * (d$p == 1) * (ref_Z[, 2] - ref_means[3]),
      2 * (d$p == 1) * (ref_Z[, 1] - ref_means[4])
    )
    ## the 2 subgroup average treatment effects reported by default:
    ref_contrasts <- c(ref_means[1] - ref_means[2], ref_means[3] - ref_means[4])

    sub <- policy_eval(
        target = "subgroup",
        policy_data = pd,
        policy = pol,
        q_models = polle:::q_degen(var = "z"),
        g_models = g_glm(~1)
    )

    ## the object stores the 4 means and the two labellings:
    expect_equal(unname(sub$coef), ref_means)
    expect_length(sub$name, 4)
    expect_length(sub$contrast_name, 2)

    ## coef() returns the 2 contrasts by default and the 4 means with
    ## contrast = FALSE:
    expect_equal(unname(coef(sub)), ref_contrasts)
    expect_equal(names(coef(sub)), sub$contrast_name)
    expect_equal(unname(coef(sub, contrast = FALSE)), ref_means)
    expect_equal(names(coef(sub, contrast = FALSE)), sub$name)

    ## the value of the estimate does not depend on the contrast argument:
    expect_equal(coef(sub), coef(estimate(sub)))

    ## summary() mirrors coef(): 2 contrasts by default, 4 means otherwise:
    expect_equal(unname(coef(summary(sub))), ref_contrasts)
    expect_equal(names(coef(summary(sub))), sub$contrast_name)
    expect_equal(unname(coef(summary(sub, contrast = FALSE))), ref_means)
    expect_equal(names(coef(summary(sub, contrast = FALSE))), sub$name)

    ## the influence curve / variance also follow the contrast argument:
    expect_equal(ncol(IC(sub)), 2)
    expect_equal(ncol(IC(sub, contrast = FALSE)), 4)
    expect_equal(dim(vcov(sub)), c(2L, 2L))
    expect_equal(dim(vcov(sub, contrast = FALSE)), c(4L, 4L))
    expect_equal(
      IC(sub, contrast = FALSE) |> unname(),
      ref_IC,
      check.attributes = FALSE
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
    cbind(rep(as.numeric(0), 1e2)),
    check.attributes = FALSE
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
    cbind(rep(as.numeric(0), 1e2))
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
    cbind(rep(as.numeric(0), 1e2)),
    check.attributes = FALSE
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

  ## the object stores the 4 per-subgroup potential outcome means per policy:
  mean_names <- c(
    "E[U(2)|d=1]: d=blip(eta=50)", "E[U(1)|d=1]: d=blip(eta=50)",
    "E[U(2)|d=0]: d=blip(eta=50)", "E[U(1)|d=0]: d=blip(eta=50)",
    "E[U(2)|d=1]: d=blip(eta=101)", "E[U(1)|d=1]: d=blip(eta=101)",
    "E[U(2)|d=0]: d=blip(eta=101)", "E[U(1)|d=0]: d=blip(eta=101)"
  )
  ## the subgroup average treatment effects reported by default:
  contrast_names <- c(
    "E[U(2)-U(1)|d=1]: d=blip(eta=50)", "E[U(2)-U(1)|d=0]: d=blip(eta=50)",
    "E[U(2)-U(1)|d=1]: d=blip(eta=101)", "E[U(2)-U(1)|d=0]: d=blip(eta=101)"
  )

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
    mean_names
  )

  expect_equal(
    names(coef(sub)),
    contrast_names
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub2_eta50, ref_sub1_eta50, as.numeric(NA), ref_sub1_eta101)
  )

  expect_equal(
    IC(sub) |> unname(),
    cbind(ref_IC2_eta50,
          ref_IC1_eta50,
          rep(as.numeric(0), 1e2),
          ref_IC1_eta101) |> unname(),
    check.attributes = FALSE
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
    mean_names
  )

  expect_equal(
    names(coef(sub)),
    contrast_names
  )

  expect_equal(
    coef(sub) |> unname(),
    c(ref_sub2_eta50, ref_sub1_eta50, as.numeric(NA), ref_sub1_eta101)
  )

  expect_equal(
    IC(sub) |> unname(),
    cbind(ref_IC2_eta50, ref_IC1_eta50, rep(as.numeric(0), 1e2), ref_IC1_eta101) |> unname(),
    check.attributes = FALSE
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
    mean_names
  )

  expect_equal(
    names(coef(sub)),
    contrast_names
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

test_that("policy_eval runs with policy_learn using quantile_prob_thres ", {

  d1 <- sim_single_stage(1e3, seed=1)
  pd1 <- policy_data(d1, action = "A", covariates = c("Z"), utility = "U")

  qs <- c(0.25, 0.5, 0.75)

  learn1 <- policy_learn(type = "blip",
                         control = control_blip(
                           blip_models = q_glm(~ .),
                           quantile_prob_threshold = qs
                         ))
  expect_error(
    pe1 <- policy_eval(
      policy_data = pd1,
      policy_learn = learn1,
      target = "subgroup",
      g_models = g_glm(~ 1),
      q_models = q_glm(~ A * (.)),
      M = 2
    ),
    NA
  )

})

test_that("get_q_functions() from a learned blip policy is a reusable q_functions object (target = 'subgroup')", {
  set.seed(1)
  n <- 200
  a <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  z <- rbinom(n, 1, 0.5)
  y <- 1 + a + x - a * x + z + rnorm(n)
  d <- data.frame(y = y, a = a, x = x, z = z)

  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("x", "z"),
    utility = "y"
  )

  ## direct subgroup evaluation with a fixed policy (baseline sanity check)
  p1 <- policy_def(function(x) (x > 0) * 1)
  expect_no_error(
    pe1 <- policy_eval(
      pd,
      policy = p1,
      q_models = q_glm(~ A * x * z),
      g_models = g_glm(~ 1),
      target = "subgroup"
    )
  )

  ## learn a subgroup (blip) policy and evaluate it
  pl1 <- policy_learn(
    type = "blip",
    control = control_blip(blip_models = q_glm(~ x + z))
  )
  expect_no_error(
    pe_pl1 <- policy_eval(
      pd,
      policy_learn = pl1,
      q_models = q_glm(~ A * x * z),
      g_models = g_glm(~ 1),
      target = "subgroup"
    )
  )

  ## extract the learned policy and the fitted nuisance functions
  po1 <- get_policy(pe_pl1)
  gf <- get_g_functions(pe_pl1)
  qf <- get_q_functions(pe_pl1)

  ## the extracted nuisance functions must carry their documented classes.
  ## g_functions currently does; q_functions currently does NOT
  ## (bug under investigation: blip policy object drops the "q_functions" class).
  expect_true(inherits(gf, "g_functions"))
  expect_true(inherits(qf, "q_functions"))

  ## reuse the learned policy + fitted nuisance functions to evaluate the
  ## subgroup effect on the x > 0 subset. Currently errors with
  ## "q_functions must be of class 'q_functions'." due to the class above.
  pd_subset <- policy_data(
    data = d[d$x > 0, ],
    action = "a",
    covariates = c("x", "z"),
    utility = "y"
  )
    expect_no_error(
      pe_pl1_subset <- policy_eval(
        policy_data = pd_subset,
        policy = po1,
        q_functions = qf,
        g_functions = gf,
        target = "subgroup"
      )
    )
})

test_that("policy_eval target = 'subgroup' summary table has the expected schema for blip", {
  z <- 1:1e2
  a <- c(rep(1, 50), rep(2, 50))
  y <- a * 2
  p1 <- c(rep(1, 50), rep(2, 50))
  p2 <- rep(1, 100)
  d <- data.table(z = z, a = a, y = y, p1 = p1, p2 = p2)
  pd <- policy_data(
    data = d,
    action = "a",
    covariates = c("z", "p1", "p2"),
    utility = c("y")
  )

  ## ------------------------------------------------------------------
  ## blip with a single user-supplied threshold (no cross-fitting):
  ## ------------------------------------------------------------------
  pl1 <- policy_learn(
    type = "blip",
    threshold = 50,
    control = control_blip(blip_models = polle:::q_degen(var = "z"))
  )
  sub1 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl1,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )
  m1 <- summary(sub1, return_table = TRUE, contrast = FALSE)
  c1 <- summary(sub1, return_table = TRUE, contrast = TRUE)

  expect_true(data.table::is.data.table(m1))
  expect_true(data.table::is.data.table(c1))

  ## schema: means view has the fixed prefix + input_meta columns
  ## (action, subgroup + policy-level meta: type, policy, alpha, threshold):
  expect_equal(
    names(m1),
    c("name", "estimate", "se", "subgroup_proportion",
      "action", "subgroup", "type", "policy", "alpha", "threshold")
  )
  expect_equal(
    names(c1),
    c("name", "estimate", "se", "subgroup_proportion",
      "subgroup", "type", "policy", "alpha", "threshold")
  )

  ## means: 4 rows for one policy; contrasts: 2 rows.
  expect_equal(nrow(m1), 4L)
  expect_equal(nrow(c1), 2L)

  ## estimates / se agree with the object accessors:
  expect_equal(m1$estimate, unname(sub1$coef))
  expect_equal(m1$se, unname(sqrt(diag(vcov(sub1, contrast = FALSE)))))
  expect_equal(c1$estimate, unname(coef(sub1)))
  expect_equal(c1$se, unname(sqrt(diag(vcov(sub1, contrast = TRUE)))))

  ## the row-labels come from name / contrast_name:
  expect_equal(m1$name, sub1$name)
  expect_equal(c1$name, sub1$contrast_name)

  ## input_meta columns:
  expect_equal(m1$action, c("2", "1", "2", "1"))
  expect_equal(m1$subgroup, c(1, 1, 0, 0))
  expect_equal(m1$type, rep("blip", 4L))
  ## default name defaults to type; policy column mirrors it:
  expect_equal(m1$policy, rep("blip", 4L))
  expect_equal(m1$threshold, rep(50, 4L))
  expect_equal(c1$subgroup, c(1, 0))
  expect_equal(c1$type, rep("blip", 2L))
  expect_equal(c1$policy, rep("blip", 2L))
  expect_equal(c1$threshold, rep(50, 2L))

  ## subgroup_proportion matches colMeans(subgroup_indicator):
  sp1 <- colMeans(sub1$subgroup_indicator)
  expect_equal(m1$subgroup_proportion, rep(sp1, each = 2L))
  expect_equal(c1$subgroup_proportion, sp1)

  ## output_meta is absent when the threshold is not quantile-based:
  expect_null(sub1$output_meta)

  ## ------------------------------------------------------------------
  ## blip with multiple user-supplied thresholds (no cross-fitting):
  ## ------------------------------------------------------------------
  pl2 <- policy_learn(
    type = "blip",
    threshold = c(50, 101),
    control = control_blip(blip_models = polle:::q_degen(var = "z"))
  )
  sub2 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl2,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )
  m2 <- summary(sub2, return_table = TRUE, contrast = FALSE)
  c2 <- summary(sub2, return_table = TRUE, contrast = TRUE)

  ## same schema:
  expect_equal(
    names(m2),
    c("name", "estimate", "se", "subgroup_proportion",
      "action", "subgroup", "type", "policy", "alpha", "threshold")
  )
  expect_equal(
    names(c2),
    c("name", "estimate", "se", "subgroup_proportion",
      "subgroup", "type", "policy", "alpha", "threshold")
  )

  ## means: 4 rows x 2 policies = 8; contrasts: 2 rows x 2 policies = 4.
  expect_equal(nrow(m2), 8L)
  expect_equal(nrow(c2), 4L)

  ## per-policy blocks in the expected order (sorted by threshold):
  expect_equal(m2$threshold, rep(c(50, 101), each = 4L))
  expect_equal(c2$threshold, rep(c(50, 101), each = 2L))
  expect_equal(m2$action, rep(c("2", "1", "2", "1"), 2L))
  expect_equal(m2$subgroup, rep(c(1, 1, 0, 0), 2L))
  expect_equal(c2$subgroup, rep(c(1, 0), 2L))

  ## the name column round-trips against the stored labels:
  expect_equal(m2$name, sub2$name)
  expect_equal(c2$name, sub2$contrast_name)

  ## subgroup_proportion: per policy, sub_indicator columns are ordered
  ## [d==a2, d==a1]. Broadcast per action row for means; identity for
  ## contrasts.
  sp2 <- colMeans(sub2$subgroup_indicator)
  expect_equal(m2$subgroup_proportion, rep(sp2, each = 2L))
  expect_equal(c2$subgroup_proportion, sp2)

  ## the specific values for this synthetic data:
  expect_equal(m2$subgroup_proportion,
               c(0.5, 0.5, 0.5, 0.5, 0, 0, 1, 1))
  expect_equal(c2$subgroup_proportion, c(0.5, 0.5, 0, 1))

  ## output_meta is absent when the threshold is not quantile-based:
  expect_null(sub2$output_meta)

  ## ------------------------------------------------------------------
  ## blip with a quantile probability threshold (dynamic threshold):
  ## ------------------------------------------------------------------
  pl3 <- policy_learn(
    type = "blip",
    control = control_blip(
      blip_models = polle:::q_degen(var = "z"),
      quantile_prob_threshold = c(0.25, 0.75)
    )
  )
  sub3 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl3,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )
  m3 <- summary(sub3, return_table = TRUE, contrast = FALSE)
  c3 <- summary(sub3, return_table = TRUE, contrast = TRUE)

  ## schema: `quantile_prob_threshold` replaces `threshold` in input_meta,
  ## and `threshold` moves to output_meta. The contrast view additionally
  ## drops the `action` column.
  expect_equal(
    names(m3),
    c("name", "estimate", "se", "subgroup_proportion",
      "action", "subgroup", "type", "policy", "alpha",
      "quantile_prob_threshold")
  )
  expect_equal(
    names(c3),
    c("name", "estimate", "se", "subgroup_proportion",
      "subgroup", "type", "policy", "alpha",
      "quantile_prob_threshold")
  )
  expect_false("threshold" %in% names(m3))
  expect_false("threshold" %in% names(c3))

  ## the output_meta captures the fold-realised numeric threshold(s):
  expect_true(data.table::is.data.table(sub3$output_meta))
  expect_equal(names(sub3$output_meta), "threshold")
  expect_equal(nrow(sub3$output_meta), 2L)

  ## policy names encode the quantile probability rather than the numeric
  ## threshold:
  expect_true(all(grepl("blip\\(q=", m3$name)))
  expect_true(all(grepl("blip\\(q=", c3$name)))
  expect_equal(m3$quantile_prob_threshold, rep(c(0.25, 0.75), each = 4L))
  expect_equal(c3$quantile_prob_threshold, rep(c(0.25, 0.75), each = 2L))

  ## estimates / se agree with the object accessors:
  expect_equal(m3$estimate, unname(sub3$coef))
  expect_equal(c3$estimate, unname(coef(sub3)))
  expect_equal(m3$se, unname(sqrt(diag(vcov(sub3, contrast = FALSE)))))
  expect_equal(c3$se, unname(sqrt(diag(vcov(sub3, contrast = TRUE)))))

  ## ------------------------------------------------------------------
  ## blip under cross-fitting (M > 1) with user-supplied thresholds:
  ## input_meta is static so the schema and columns are preserved.
  ## ------------------------------------------------------------------
  gf <- fit_g_functions(pd, g_models = g_glm(~1))
  set.seed(1)
  sub4 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl2,
    q_models = polle:::q_degen(var = "z"),
    g_functions = gf,
    cross_fit_type = "pooled",
    variance_type = "pooled",
    M = 2
  )
  m4 <- summary(sub4, return_table = TRUE, contrast = FALSE)
  c4 <- summary(sub4, return_table = TRUE, contrast = TRUE)
  expect_equal(names(m4), names(m2))
  expect_equal(names(c4), names(c2))
  expect_equal(nrow(m4), 8L)
  expect_equal(nrow(c4), 4L)
  expect_equal(m4$threshold, rep(c(50, 101), each = 4L))
  expect_equal(c4$threshold, rep(c(50, 101), each = 2L))
  expect_equal(m4$estimate, unname(sub4$coef))
  expect_equal(c4$estimate, unname(coef(sub4)))
  sp4 <- colMeans(sub4$subgroup_indicator)
  expect_equal(m4$subgroup_proportion, rep(sp4, each = 2L))
  expect_equal(c4$subgroup_proportion, sp4)

  ## ------------------------------------------------------------------
  ## blip with a user-supplied policy name:
  ## `type` stays "blip"; `policy` records the user's name; the composed
  ## `name` swaps the "blip" stem for the user's name.
  ## ------------------------------------------------------------------
  pl5 <- policy_learn(
    type = "blip",
    threshold = 50,
    control = control_blip(blip_models = polle:::q_degen(var = "z")),
    name = "cate"
  )
  sub5 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl5,
    q_models = polle:::q_degen(var = "z"),
    g_models = g_glm(~1)
  )
  m5 <- summary(sub5, return_table = TRUE, contrast = FALSE)
  c5 <- summary(sub5, return_table = TRUE, contrast = TRUE)
  expect_equal(names(m5), names(m1))
  expect_equal(names(c5), names(c1))
  expect_equal(m5$type, rep("blip", 4L))
  expect_equal(m5$policy, rep("cate", 4L))
  expect_equal(c5$type, rep("blip", 2L))
  expect_equal(c5$policy, rep("cate", 2L))
  expect_true(all(grepl("cate\\(eta=50", m5$name)))
  expect_true(all(grepl("cate\\(eta=50", c5$name)))
})

test_that("policy_eval target = 'subgroup' summary table has the expected schema for ptl", {
  d <- sim_single_stage(n = 2e2, seed = 1)
  pd <- policy_data(d, action = "A", covariates = c("Z", "L"), utility = "U")

  ## ------------------------------------------------------------------
  ## ptl with a single user-supplied threshold:
  ## ------------------------------------------------------------------
  pl1 <- policy_learn(
    type = "ptl",
    threshold = 0,
    control = control_ptl(policy_vars = c("Z", "L"), depth = 2)
  )
  sub1 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl1,
    q_models = q_glm(),
    g_models = g_glm()
  )
  m1 <- summary(sub1, return_table = TRUE, contrast = FALSE)
  c1 <- summary(sub1, return_table = TRUE, contrast = TRUE)

  ## schema: the means view has the fixed prefix + input_meta columns; the
  ## contrast view drops `action` (contrast rows do not correspond to a
  ## specific action).
  means_cols <- c("name", "estimate", "se", "subgroup_proportion",
                  "action", "subgroup",
                  "type", "policy",
                  "alpha", "threshold", "depth", "hybrid")
  contrast_cols <- c("name", "estimate", "se", "subgroup_proportion",
                     "subgroup",
                     "type", "policy",
                     "alpha", "threshold", "depth", "hybrid")
  expect_equal(names(m1), means_cols)
  expect_equal(names(c1), contrast_cols)

  ## row counts:
  expect_equal(nrow(m1), 4L)
  expect_equal(nrow(c1), 2L)

  ## values / round-trip:
  expect_equal(m1$name, sub1$name)
  expect_equal(c1$name, sub1$contrast_name)
  expect_equal(m1$estimate, unname(sub1$coef))
  expect_equal(m1$se, unname(sqrt(diag(vcov(sub1, contrast = FALSE)))))
  expect_equal(c1$estimate, unname(coef(sub1)))
  expect_equal(c1$se, unname(sqrt(diag(vcov(sub1, contrast = TRUE)))))

  expect_equal(m1$type, rep("ptl", 4L))
  ## default name defaults to type; policy mirrors it:
  expect_equal(m1$policy, rep("ptl", 4L))
  expect_equal(m1$threshold, rep(0, 4L))
  expect_equal(m1$depth, rep(2, 4L))
  expect_true(all(!m1$hybrid))
  expect_equal(m1$subgroup, c(1, 1, 0, 0))
  expect_equal(c1$subgroup, c(1, 0))
  expect_equal(c1$type, rep("ptl", 2L))
  expect_equal(c1$policy, rep("ptl", 2L))
  expect_equal(c1$threshold, rep(0, 2L))

  ## subgroup_proportion matches colMeans(subgroup_indicator):
  sp1 <- colMeans(sub1$subgroup_indicator)
  expect_equal(m1$subgroup_proportion, rep(sp1, each = 2L))
  expect_equal(c1$subgroup_proportion, sp1)

  ## no dynamic threshold with ptl:
  expect_null(sub1$output_meta)

  ## ------------------------------------------------------------------
  ## ptl with multiple user-supplied thresholds:
  ## ------------------------------------------------------------------
  pl2 <- policy_learn(
    type = "ptl",
    threshold = c(0, 0.5),
    control = control_ptl(policy_vars = c("Z", "L"), depth = 2)
  )
  sub2 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl2,
    q_models = q_glm(),
    g_models = g_glm()
  )
  m2 <- summary(sub2, return_table = TRUE, contrast = FALSE)
  c2 <- summary(sub2, return_table = TRUE, contrast = TRUE)

  expect_equal(names(m2), means_cols)
  expect_equal(names(c2), contrast_cols)
  expect_equal(nrow(m2), 8L)
  expect_equal(nrow(c2), 4L)
  expect_equal(m2$threshold, rep(c(0, 0.5), each = 4L))
  expect_equal(c2$threshold, rep(c(0, 0.5), each = 2L))
  expect_equal(m2$subgroup, rep(c(1, 1, 0, 0), 2L))
  expect_equal(c2$subgroup, rep(c(1, 0), 2L))
  expect_equal(m2$name, sub2$name)
  expect_equal(c2$name, sub2$contrast_name)
  expect_equal(m2$estimate, unname(sub2$coef))
  expect_equal(c2$estimate, unname(coef(sub2)))
  sp2 <- colMeans(sub2$subgroup_indicator)
  expect_equal(m2$subgroup_proportion, rep(sp2, each = 2L))
  expect_equal(c2$subgroup_proportion, sp2)
  expect_null(sub2$output_meta)

  ## ------------------------------------------------------------------
  ## ptl under cross-fitting (M > 1): input_meta is static, schema preserved.
  ## ------------------------------------------------------------------
  gf <- fit_g_functions(pd, g_models = g_glm())
  set.seed(1)
  sub3 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl2,
    q_models = q_glm(),
    g_functions = gf,
    cross_fit_type = "pooled",
    variance_type = "pooled",
    M = 2
  )
  m3 <- summary(sub3, return_table = TRUE, contrast = FALSE)
  c3 <- summary(sub3, return_table = TRUE, contrast = TRUE)
  expect_equal(names(m3), means_cols)
  expect_equal(names(c3), contrast_cols)
  expect_equal(nrow(m3), 8L)
  expect_equal(nrow(c3), 4L)
  expect_equal(m3$threshold, rep(c(0, 0.5), each = 4L))
  expect_equal(c3$threshold, rep(c(0, 0.5), each = 2L))
  expect_equal(m3$estimate, unname(sub3$coef))
  expect_equal(c3$estimate, unname(coef(sub3)))

  ## ------------------------------------------------------------------
  ## ptl with a user-supplied policy name:
  ## `type` stays "ptl"; `policy` records the user's name; the composed
  ## `name` swaps the "ptl" stem for the user's name.
  ## ------------------------------------------------------------------
  pl4 <- policy_learn(
    type = "ptl",
    threshold = 0,
    control = control_ptl(policy_vars = c("Z", "L"), depth = 2),
    name = "tree"
  )
  sub4 <- policy_eval(
    target = "subgroup",
    policy_data = pd,
    policy_learn = pl4,
    q_models = q_glm(),
    g_models = g_glm()
  )
  m4 <- summary(sub4, return_table = TRUE, contrast = FALSE)
  c4 <- summary(sub4, return_table = TRUE, contrast = TRUE)
  expect_equal(names(m4), means_cols)
  expect_equal(names(c4), contrast_cols)
  expect_equal(m4$type, rep("ptl", 4L))
  expect_equal(m4$policy, rep("tree", 4L))
  expect_equal(c4$type, rep("ptl", 2L))
  expect_equal(c4$policy, rep("tree", 2L))
  expect_true(all(grepl("tree\\(eta=", m4$name)))
  expect_true(all(grepl("tree\\(eta=", c4$name)))
})

test_that("policy_eval target = 'subgroup' has symmetrical outputs if a=A/B is switched", {

  catefun <- function(z) (z <= 32) * (-4 + (z - 20) * 1/3)
  sim_data <- function(n, labels = c("A", "B")) {
    ## baseline
    z <- runif(n = n, min = 20, max = 80)

    ## treatment
    a <- rbinom(n = n, size = 1, prob = 0.5)

    ymean <- a * catefun(z = z)
    y <- rnorm(n = n, sd = 2, mean = ymean)

    aout <- ifelse(a == 1, labels[1], labels[2])

    data.frame(z = z, a = aout, y = y)
  }

  set.seed(423)
  dAB <- sim_data(4e3)
  pdAB <- policy_data(dAB,
                    action = "a",
                    utility = "y",
                    covariates = c("z"))
  set.seed(423)
  dBA <- sim_data(4e3, labels = c("B", "A"))
  pdBA <- policy_data(dBA,
                    action = "a",
                    utility = "y",
                    covariates = c("z"))

  pl <- policy_learn(type = "blip",
                   control_blip(q_glm(~ .),
                                quantile_prob_threshold = c(0.1, 0.5, 0.9)))

  ##
  ## no cross-fitting
  ##

  ## subgroup policy eval
  peAB <- policy_eval(target = "subgroup",
                    policy_data = pdAB,
                    policy_learn = pl,
                    M = 1,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  peBA <- policy_eval(target = "subgroup",
                    policy_data = pdBA,
                    policy_learn = pl,
                    M = 1,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  expect_equal(sort(peAB$policy_object$threshold),
               sort(-peBA$policy_object$threshold))

  ## contrast = FALSE

  expect_equal(
    peAB$coef |> unname(),
    rev(peBA$coef) |> unname()
  )

  expect_equal(
    summary(peAB, contrast = FALSE)$coef |> unname(),
     peAB$coef |> unname()
  )

  expect_equal(
    summary(peBA, contrast = FALSE)$coef |> unname(),
     peBA$coef |> unname()
  )

  expect_equal(
    summary(peAB, contrast = FALSE, return_table = TRUE)$estimate |> unname(),
     peAB$coef |> unname()
  )

  expect_equal(
    summary(peBA, contrast = FALSE, return_table = TRUE)$estimate |> unname(),
     peBA$coef |> unname()
  )

  ## contrast = TRUE

  expect_equal(
    summary(peAB, contrast = TRUE)$coef |> unname(),
    rev(-summary(peBA, contrast = TRUE)$coef) |> unname()
  )

  expect_equal(
    summary(peAB, contrast = TRUE, return_table = TRUE)$estimate |> unname(),
    rev(-summary(peBA, contrast = TRUE, return_table = TRUE)$estimate) |> unname()
  )

  ##
  ## cross-fitting
  ##

  ## subgroup policy eval
  set.seed(4234)
  peAB <- policy_eval(target = "subgroup",
                    policy_data = pdAB,
                    policy_learn = pl,
                    M = 4,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  set.seed(4234)
  peBA <- policy_eval(target = "subgroup",
                    policy_data = pdBA,
                    policy_learn = pl,
                    M = 4,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  expect_equal(
    peAB$coef |> unname(),
    rev(peBA$coef) |> unname()
  )

})

test_that("policy_eval target = 'subgroup' almost no treatment effect VS targeted:cate", {

  library("data.table")
  catefun <- function(z) (20 + (z <= 32) * (-4 + (z - 20) * 1/3))
  sim_data <- function(n) {
    ## baseline
    z <- runif(n = n, min = 20, max = 80)

    ## treatment
    a <- rbinom(n = n, size = 1, prob = 0.5)

    y <- rnorm(n = n, sd = 10, mean = catefun(z = z) * a)

    data.frame(z = z, a = a, y = y)
  }

  pl <- policy_learn(type = "blip",
                   control_blip(q_glm(~ .),
                                quantile_prob_threshold = c(0.1, 0.5, 0.9)))

  ## sim policy data
  set.seed(423)
  d <- sim_data(400)
  pd <- policy_data(d,
                    action = "a",
                    utility = "y",
                    covariates = c("z"))

  ## subgroup policy eval
  set.seed(90234)
  pe <- policy_eval(target = "subgroup",
                    policy_data = pd,
                    policy_learn = pl,
                    M = 1,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  ## viz
  ## library("ggplot2")
  ## sumtab <- summary(pe, return_table = TRUE)
  ## sumtab$upper <- sumtab$estimate + 1.96 * sumtab$se
  ## sumtab$lower <- sumtab$estimate - 1.96 * sumtab$se

  ## ggplot(sumtab) +
  ##   geom_point(aes(x = quantile_prob_threshold, y = estimate, color = subgroup)) +
  ##   geom_errorbar(aes(x = quantile_prob_threshold, ymin = lower, ymax = upper, color = subgroup)) +
  ##   theme_bw()

  ## g1 <- ggplot(sumtab) +
  ##   geom_point(aes(x = subgroup_proportion, y = estimate)) +
  ##   geom_errorbar(aes(x = subgroup_proportion, ymin = lower, ymax = upper)) +
  ##   facet_wrap(~ subgroup) +
  ##   theme_bw()

  ## sumtab[, sp := ifelse(subgroup == 1, subgroup_proportion, 1 - subgroup_proportion)]

  ## g2 <- ggplot(sumtab) +
  ##   geom_point(aes(x = sp, y = estimate, color = subgroup)) +
  ##   geom_errorbar(aes(x = sp, ymin = lower, ymax = upper, color = subgroup)) +
  ##   theme_bw()

  ## require(gridExtra)
  ## grid.arrange(g1, g2, ncol=2)

  ## compare to targeted::cate
  subvar <- pe$subgroup_indicator[, c(1,3,5)]
  colnames(subvar) <- c("q1", "q5", "q9")
  d <- cbind(d, subvar)

  tar_est_q1 <- targeted::cate(data = d,
                               cate.model = ~ q1 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               second.order = FALSE)

  tar_est_q5 <- targeted::cate(data = d,
                               cate.model = ~ q5 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               second.order = FALSE)

  tar_est_q9 <- targeted::cate(data = d,
                               cate.model = ~ q9 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               second.order = FALSE)

  expect_equal(
    tar_est_q1$estimate$coef[c(3,4)][c("q1TRUE", "q1FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.1]$estimate |> unname()
  )
  expect_equal(
    tar_est_q5$estimate$coef[c(3,4)][c("q5TRUE", "q5FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.5]$estimate |> unname()
  )
  expect_equal(
    tar_est_q9$estimate$coef[c(3,4)][c("q9TRUE", "q9FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.9]$estimate |> unname()
  )

  ## viz policy checks: the policy is opposite the the optimal policy
  ## po <- pe$policy_object
  ## blip_coef <- coef(po$blip_functions$stage_1$blip_model$model)
  ## ref_model <- glm(y ~ a * z, data = d)

  ## ggplot(d) +
  ##   geom_point(aes(x = z, y = y, color = as.factor(a))) +
  ##   geom_function(fun = function(x) catefun(z = x), color = "blue") +
  ##   geom_abline(intercept = blip_coef[1], slope = blip_coef[2], color = "red") +
  ##   theme_bw()

  ## tmp <- cbind(get_policy(po)[[3]](pd), as.data.table(d))
  ## tmp[order(z)]
  ## ggplot(tmp) +
  ##   geom_histogram(aes(x = z)) +
  ##   facet_wrap(~ d) +
  ##   theme_bw()

})

test_that("policy_eval target = 'subgroup' almost no treatment effect with cross-fitting VS targeted:cate", {
  library("data.table")
  catefun <- function(z) (20 + (z <= 32) * (-4 + (z - 20) * 1/3))
  sim_data <- function(n) {
    ## baseline
    z <- runif(n = n, min = 20, max = 80)

    ## treatment
    a <- rbinom(n = n, size = 1, prob = 0.5)

    y <- rnorm(n = n, sd = 10, mean = catefun(z = z) * a)

    data.frame(z = z, a = a, y = y)
  }

  pl <- policy_learn(type = "blip",
                   control_blip(q_glm(~ .),
                                quantile_prob_threshold = c(0.1, 0.5, 0.9)))

  ## sim policy data
  set.seed(423)
  ## 423, by change display the opposite heterogenity
  ## 424 acts as expected
  d <- sim_data(400)
  pd <- policy_data(d,
                    action = "a",
                    utility = "y",
                    covariates = c("z"))

  ## subgroup policy eval
  set.seed(90234)
  pe <- policy_eval(target = "subgroup",
                    policy_data = pd,
                    policy_learn = pl,
                    M = 10,
                    g_models = g_glm(~1),
                    q_models = q_glm(~ A * z))

  ## viz q = 0.1 subgroups
  ## sg <- pe$subgroup_indicator[, c(1,2)]
  ## colnames(sg) <- c("high", "low")
  ## plot_data <- cbind(d, sg)
  ## g1 <- ggplot(plot_data) +
  ##   geom_histogram(aes(x = z)) +
  ##   facet_wrap(~ high) +
  ##   theme_bw()

  ## g2 <- ggplot(plot_data) +
  ##   geom_point(aes(x = z, y = y, color = a)) +
  ##   geom_smooth(aes(x = z, y = y, group= a))

  ## require(gridExtra)
  ## grid.arrange(g1, g2, ncol=2)

  ## ## viz
  ## plot_data <- d[d$a == 1,]
  ## ggplot(plot_data) +
  ##   geom_point(aes(x = z, y = y)) +
  ##   geom_smooth(aes(x = z, y = y), method='lm', formula= y~x) +
  ##   geom_function(fun = function(x) catefun(z = x), color = "blue") +
  ##   theme_b


  ## inspect
  ## lapply(pe$cross_fits, function(x) x$policy_object$blip_functions$stage_1$blip_model)

  ## viz
  ## library("ggplot2")
  ## sumtab <- summary(pe, return_table = TRUE)
  ## sumtab$upper <- sumtab$estimate + 1.96 * sumtab$se
  ## sumtab$lower <- sumtab$estimate - 1.96 * sumtab$se

  ## ggplot(sumtab) +
  ##   geom_point(aes(x = quantile_prob_threshold, y = estimate, color = subgroup)) +
  ##   geom_errorbar(aes(x = quantile_prob_threshold, ymin = lower, ymax = upper, color = subgroup)) +
  ##   theme_bw()

  ## g1 <- ggplot(sumtab) +
  ##   geom_point(aes(x = subgroup_proportion, y = estimate)) +
  ##   geom_errorbar(aes(x = subgroup_proportion, ymin = lower, ymax = upper)) +
  ##   facet_wrap(~ subgroup) +
  ##   theme_bw()

  ## sumtab[, sp := ifelse(subgroup == 1, subgroup_proportion, 1 - subgroup_proportion)]
  ## g2 <- ggplot(sumtab) +
  ##   geom_point(aes(x = sp, y = estimate, color = subgroup)) +
  ##   geom_errorbar(aes(x = sp, ymin = lower, ymax = upper, color = subgroup)) +
  ##   theme_bw()

  ## require(gridExtra)
  ## grid.arrange(g1, g2, ncol=2)

  ## ## test
  ## est <- estimate(pe)
  ## est[1] - est[2]

  ## compare to targeted::cate
  subvar <- pe$subgroup_indicator[, c(1,3,5)]
  colnames(subvar) <- c("q1", "q5", "q9")
  d <- cbind(d, subvar)

  set.seed(90234)
  tar_est_q1 <- targeted::cate(data = d,
                               cate.model = ~ q1 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               nfolds = 10,
                               second.order = FALSE)

  set.seed(90234)
  tar_est_q5 <- targeted::cate(data = d,
                               cate.model = ~ q5 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               nfolds = 10,
                               second.order = FALSE)

  set.seed(90234)
  tar_est_q9 <- targeted::cate(data = d,
                               cate.model = ~ q9 - 1,
                               response.model = y ~ a * z,
                               treatment.model = a ~ 1,
                               nfolds = 10,
                               second.order = FALSE)

  expect_equal(
    tar_est_q1$estimate$coef[c(3,4)][c("q1TRUE", "q1FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.1]$estimate |> unname()
  )
  expect_equal(
    tar_est_q5$estimate$coef[c(3,4)][c("q5TRUE", "q5FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.5]$estimate |> unname()
  )
  expect_equal(
    tar_est_q9$estimate$coef[c(3,4)][c("q9TRUE", "q9FALSE")] |> unname(),
    summary(pe, return_table = TRUE)[quantile_prob_threshold == 0.9]$estimate |> unname()
  )

  ## viz policy checks: the policy is opposite the the optimal policy
  ## po <- pe$policy_object
  ## blip_coef <- coef(po$blip_functions$stage_1$blip_model$model)
  ## ref_model <- glm(y ~ a * z, data = d)

  ## ggplot(d) +
  ##   geom_point(aes(x = z, y = y, color = as.factor(a))) +
  ##   geom_function(fun = function(x) catefun(z = x), color = "blue") +
  ##   geom_abline(intercept = blip_coef[1], slope = blip_coef[2], color = "red") +
  ##   theme_bw()

  ## tmp <- cbind(get_policy(po)[[3]](pd), as.data.table(d))
  ## tmp[order(z)]
  ## ggplot(tmp) +
  ##   geom_histogram(aes(x = z)) +
  ##   facet_wrap(~ d) +
  ##   theme_bw()

  ## policy_eval with repeated cross-fitting:
  ## set.seed(4342)
  ## perep <- policy_eval(target = "subgroup",
  ##                   policy_data = pd,
  ##                   policy_learn = pl,
  ##                   M = 10,
  ##                   nrep = 2,
  ##                   g_models = g_glm(~1),
  ##                   q_models = q_glm(~ A * z))
  ## summary(perep, contrast = TRUE)
})
