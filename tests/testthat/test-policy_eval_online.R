test_that("policy_eval_online() uses valid indexes for training and validation:", {

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
  p <- policy_def(function(p) p, name = "test")
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  set.seed(45234)
  pe <- policy_eval_online(
    policy_data = pd,
    policy = p,
    target = "value",
    g_functions = gf,
    q_models = q_degen(var = "z"),
    train_block_size = 60,
    M = 2
  )

  train_sequential_index <- pe$train_sequential_index
  validation_sequential_index <- pe$valid_sequential_index

  ## expecting accumulating training indexes:
  expect_true(
    all(train_sequential_index[[1]] %in% train_sequential_index[[2]])
  )

  ## expecting that the validation indexes are included in the training indexes:
  expect_true(
  all(sort(c(train_sequential_index[[1]], validation_sequential_index[[1]])) == train_sequential_index[[2]])
  )

  ## expecting that all the data is used:
  expect_true(
    all(sort(c(train_sequential_index[[2]], validation_sequential_index[[2]])) == 1:100)
  )

})

test_that("policy_eval_online has the expected output for target = 'value' in the single stage case:", {

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
  p <- policy_def(function(p) p, name = "test")
  gf <- fit_g_functions(pd, g_models = g_glm(~1))
  ref_Z <- (d$a == d$p) / 0.5 * (d$y - d$z) + d$z

  ## single policy

  set.seed(1)
  pe <- policy_eval_online(
    policy_data = pd,
    policy = p,
    target = "value",
    g_functions = gf,
    q_models = q_degen(var = "z"),
    train_block_size = 60,
    M = 2
  )

  train_sequential_index <- pe$train_sequential_index
  valid_sequential_index <- pe$valid_sequential_index

  ## standard deviation estimate for each training sample:
  train_sigma <- lapply(
    train_sequential_index,
    function(x){
      tmp <- ref_Z[x]
      mean((tmp - mean(tmp))^2)
    }
  ) |> unlist() |> sqrt()

  valid_count <- lapply(
    valid_sequential_index,
    length
  ) |> unlist()
  train_sigma_inverse_sum <- sum(valid_count / train_sigma)

  ## the doubly robust score evaluated on each of the validation samples:
  valid_score <- lapply(
    valid_sequential_index,
    function(x){
      sum(ref_Z[x])
    }
  ) |> unlist()

  ## reference online estimate:
  ref_coef <- sum(train_sigma^(-1) * valid_score) / train_sigma_inverse_sum
  names(ref_coef) <- "E[U(d)]: d=test"


  expect_equal(
    coef(pe),
    ref_coef
  )

  ## reference squared standard error estimate:
  ref_vcov <- (mean(train_sigma^(-1))^{-1}/sqrt(100-60))^2


  expect_equal(
    ref_vcov,
    vcov(pe)[[1]]
  )

  ## multiple policies

  pl <- policy_learn(
    type = "blip",
    threshold = c(25, 75),
    control = control_blip(blip_models = q_degen(var = "z"))
  )

  ## reference doubly robust score:
  ref_Z_25 <- (d$a == c(rep(1, 25), rep(2, 75))) / 0.5 * (d$y - d$z) + d$z
  ref_Z_75 <- (d$a == c(rep(1, 75), rep(2, 25))) / 0.5 * (d$y - d$z) + d$z

  set.seed(1)
  pe <- policy_eval_online(
    policy_data = pd,
    policy_learn = pl,
    target = "value",
    g_functions = gf,
    q_models = q_degen(var = "z"),
    train_block_size = 60,
    M = 2
  )

  train_sequential_index <- pe$train_sequential_index
  valid_sequential_index <- pe$valid_sequential_index

  ## standard deviation estimate for each training sample
  train_sigma <- lapply(
    train_sequential_index,
    function(x){
      tmp25 <- ref_Z_25[x]
      tmp75 <- ref_Z_75[x]
      out <- c(
        mean((tmp25 - mean(tmp25))^2),
        mean((tmp75 - mean(tmp75))^2)
      )
      out <- sqrt(out)
    }
  ) |> do.call(what = "rbind")

  valid_count <- lapply(
    valid_sequential_index,
    length
  ) |> unlist()

  ## sum of the inverse standard deviaiton estimates
  train_sigma_inverse_sum <-  1 / train_sigma *
    matrix(rep(valid_count,2), byrow = TRUE, ncol = length(valid_count))
  train_sigma_inverse_sum <- colSums(train_sigma_inverse_sum)

  ## the doubly robust score evaluated on each of the validation samples:
  valid_score <- lapply(
    valid_sequential_index,
    function(x){
      c(sum(ref_Z_25[x]),sum(ref_Z_75[x]))
    }
  ) |> do.call(what = "rbind")

  ## reference online estimate:
  ref_coef <- colSums(train_sigma^(-1) * valid_score) / train_sigma_inverse_sum
  names(ref_coef) <- c("E[U(d)]: d=blip(eta=25)", "E[U(d)]: d=blip(eta=75)")

  expect_equal(
    coef(pe),
    ref_coef
  )

  ## reference squared standard error estimate:
  ref_vcov <- (colMeans(train_sigma^(-1))^{-1}/sqrt(100-60))^2
  tmp <- matrix(ncol = length(ref_vcov), nrow = length(ref_vcov))
  diag(tmp) <- ref_vcov
  ref_vcov <- tmp

  expect_equal(
    ref_vcov,
    vcov(pe)
  )

})

test_that("policy_eval_sequential has the expected output in the single stage case for target = 'subgroup'", {

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
  p <- policy_def(function(p) p, name = "test")
  gf <- fit_g_functions(pd, g_models = g_glm(~1))

  ref_Z_1 <- (d$a == 1) / 0.5 * (d$y - d$z) + d$z
  ref_Z_2 <- (d$a == 2) / 0.5 * (d$y - d$z) + d$z
  ref_blip <- ref_Z_2 - ref_Z_1

  ## single policy

  pe <- policy_eval_online(
    policy_data = pd,
    policy = p,
    target = "subgroup",
    g_functions = gf,
    q_models = q_degen(var = "z"),
    train_block_size = 60,
    M = 2
  )

  train_sequential_index <- pe$train_sequential_index
  valid_sequential_index <- pe$valid_sequential_index

  ## standard deviation estimate for each training sample:
  train_sigma <- lapply(
    train_sequential_index,
    function(x){
      blip <- ref_blip[x]
      s1 <- (d$p == 1)[x]
      s2 <- (d$p == 2)[x]

      est_1 <- mean(blip[s1])
      est_2 <- mean(blip[s2])

      var_1 <- mean((s1 / mean(s1) * (blip - est_1))^2)
      var_2 <- mean((s2 / mean(s2) * (blip - est_2))^2)

      return(sqrt(c(var_1, var_2)))
    }
  ) |> do.call(what = "rbind")

  train_subgroup_prob <- lapply(
    train_sequential_index,
    function(x){
      s1 <- (d$p == 1)[x]
      s2 <- (d$p == 2)[x]
      return(c(mean(s1), mean(s2)))
    }
  ) |> do.call(what = "rbind")

  valid_count <- lapply(
    valid_sequential_index,
    length
  ) |> unlist()

  ## sum of the inverse standard deviaiton estimates
  train_sigma_inverse_sum <-  1 / train_sigma *
    matrix(rep(valid_count,2), byrow = TRUE, ncol = length(valid_count))
  train_sigma_inverse_sum <- colSums(train_sigma_inverse_sum)

  ## the doubly robust score evaluated on each of the validation samples:
  valid_score <- lapply(
    valid_sequential_index,
    function(x){
      blip <- ref_blip[x]
      s1 <- (d$p == 1)[x]
      s2 <- (d$p == 2)[x]

      return(
        c(
          sum(s1 * blip),
          sum(s2 * blip)
        )
      )
    }
  ) |> do.call(what = "rbind")
  valid_score <- valid_score / train_subgroup_prob

  ## reference online estimate:
  ref_coef <- colSums(train_sigma^(-1) * valid_score) / train_sigma_inverse_sum
  ref_coef <- rev(ref_coef)
  names(ref_coef) <- c("E[U(2)-U(1)|d=2]: d=test", "E[U(2)-U(1)|d=1]: d=test")

  expect_equal(
    coef(pe),
    ref_coef
  )

  ## reference squared standard error estimate:
  ref_vcov <- (colMeans(train_sigma^(-1))^{-1}/sqrt(100-60))^2
  tmp <- matrix(ncol = length(ref_vcov), nrow = length(ref_vcov))
  diag(tmp) <- rev(ref_vcov)
  ref_vcov <- tmp

  expect_equal(
    ref_vcov,
    vcov(pe)
  )

})
