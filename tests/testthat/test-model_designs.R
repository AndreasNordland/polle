test_that("get_design() and apply_design passes dots",{

  df <- data.table(x.1=c(1,2),y=c(3,5), "z:1" =c(3,6), "_w_" = c(1,2))

  des <- get_design(formula = y ~ ., data = df)

  expect_equal(
    colnames(des$x),
    c("x.1", "_z_1_", "__w__")
  )

  ades <- apply_design(design = des, data = df)

  expect_equal(
    colnames(ades),
    c("x.1", "_z_1_", "__w__")
  )

})
