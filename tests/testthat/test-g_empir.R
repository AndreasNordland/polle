test_that("calculate_prop_table", {
  data <- data.table(
    A = c("yes", "no", "yes", "no", "yes", "yes"),
    B = c("a", "a", "a", "a", "b", "b"),
    BB = c("c","c","c","d","d", "d")
  )

  expect_equal(
    calculate_prop_table(data, formula = ~ 1)[["tab"]],
    data.table(A = c("no", "yes"), empir_prob = c(2/6, 4/6))
  )

  expect_equal(
    calculate_prop_table(data, formula = ~ B)[["tab"]],
    data.table(A = c("no", "yes", "yes"), B = c("a", "a", "b"), empir_prob = c(1/2, 1/2, 1))
  )

})

test_that("g_empir predictions in a single stage setting", {
  source(system.file("sim", "single_stage.R", package="polle"))

  n <- 1e2
  d <- sim_single_stage(n = n, seed = 1)
  set.seed(1)
  d$BB <- sample(x = c("group1", "group2"), prob = c(0.5, 0.5), replace = TRUE, size = n)
  d$BBB <- d$A

  pd <- policy_data(d,
                    action = "A",
                    covariates = c("Z", "L", "B", "BB", "BBB"),
                    utility = "U")

  # no groups
  g_functions <- fit_g_functions(pd,
                                 g_models = g_empir(),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)
  ref <- pd$stage_data[event == 0, list(p = .N / n) ,by = "A"][order(A),]$p
  ref <- matrix(rep(ref, times = n), ncol = 2, byrow = TRUE)
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_0", "g_1"), with = FALSE]))
  )

  # single grouping variable
  g_functions <- fit_g_functions(pd,
                                 g_models = g_empir(~B),
                                 full_history = FALSE)
  g_functions2 <- fit_g_functions(pd,
                                 g_models = g_glm(~B),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)
  pred2 <- predict(g_functions2, new_policy_data = pd)
  ref <- pd$stage_data[event == 0, list(p = .N) ,by = c("A", "B")]
  ref[, N_B := sum(p), by = "B"]
  ref[, p := p / N_B]
  ref[, N_B := NULL]
  H <- get_history(pd)$H
  ref <- cbind(
    merge(H, ref[A==0,], by = "B", all.x = TRUE)[order(id, stage),]$p,
    merge(H, ref[A==1,], by = "B", all.x = TRUE)[order(id, stage),]$p
  )
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_0", "g_1"), with = FALSE]))
  )
  expect_equal(
    unname(as.matrix(pred[, c("g_0", "g_1"), with = FALSE])),
    unname(as.matrix(pred2[, c("g_0", "g_1"), with = FALSE]))
  )

  # two grouping variables
  g_functions <- fit_g_functions(pd,
                                 g_models = g_empir(~B + BB),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)
  tmp <- cbind(pred, B = d$B, BB = d$BB) # incorrect
  ref <- pd$stage_data[event == 0, list(p = .N) ,by = c("A", "B", "BB")]
  ref[, N_group := sum(p), by = c("B", "BB")]
  ref[, p := p / N_group]
  ref[, N_group := NULL]
  ref <- cbind(
    merge(H, ref[A==0,], by = c("B", "BB"), all.x = TRUE)[order(id, stage),]$p,
    merge(H, ref[A==1,], by = c("B", "BB"), all.x = TRUE)[order(id, stage),]$p
  )
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_0", "g_1"), with = FALSE]))
  )

  # degenerate distribution
  g_functions <- fit_g_functions(pd,
                                 g_models = g_empir(~B + BBB),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)
  ref <- cbind(as.numeric(d$BBB == "0"), as.numeric(d$BBB == "1"))
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_0", "g_1"), with = FALSE]))
  )

})

test_that("g_empir predictions in a two stage setting", {
  source(system.file("sim", "two_stage_multi_actions.R", package="polle"))
  set.seed(1)
  d <- sim_two_stage_multi_actions(n = 1e2)
  pd <- policy_data(data = d,
                    action = c("A_1", "A_2"),
                    baseline = c("B", "BB"),
                    covariates = list(L = c("L_1", "L_2"),
                                      C = c("C_1", "C_2")),
                    utility = c("U_1", "U_2", "U_3"))
  n <- get_n(pd)

  # no groups
  g_functions <- fit_g_functions(pd,
                                 g_models = list(g_empir(), g_empir()),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)

  ref1 <- pd$stage_data[event == 0 & stage == 1, list(p = .N / n) ,by = c("A")][order(A),]$p
  ref2 <- pd$stage_data[event == 0 & stage == 2, list(p = .N / n) ,by = c("A")][order(A),]$p
  ref <- matrix(rep(c(0,ref1, ref2), times = n), ncol = 3, byrow = TRUE)
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_default", "g_no", "g_yes"), with = FALSE]))
  )

  # single grouping variable
  g_functions <- fit_g_functions(pd,
                                 g_models = list(g_empir(~BB), g_empir(~BB)),
                                 full_history = FALSE)
  pred <- predict(g_functions, new_policy_data = pd)
  ref <- merge(pd$stage_data, pd$baseline_data)[event == 0, list(p = .N) , by = c("stage","A","BB")]
  ref[, N_B := sum(p), by = c("stage", "BB")]
  ref[, p := p / N_B]
  ref[, N_B := NULL]
  H <- get_history(pd)$H
  ref <- cbind(
    merge(H, ref[A=="default",], by = c("stage","BB"), all.x = TRUE)[order(id, stage),]$p,
    merge(H, ref[A=="no",], by = c("stage","BB"), all.x = TRUE)[order(id, stage),]$p,
    merge(H, ref[A=="yes",], by = c("stage","BB"), all.x = TRUE)[order(id, stage),]$p
  )
  ref[is.na(ref)] <- 0
  expect_equal(
    ref,
    unname(as.matrix(pred[, c("g_default", "g_no", "g_yes"), with = FALSE]))
  )

})
