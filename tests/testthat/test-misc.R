testthat::context("Utility functions")

testthat::test_that("IPW construction", {
  d <- c(0,1,1)
  g <- c(0.1,0.2,0.3)
  testthat::expect_equivalent(d/g, polle:::ipw_weight(d,g))
  testthat::expect_equivalent(d/(g*g), polle:::ipw_weight(cbind(d,d), cbind(g,g)))
  testthat::expect_error(polle:::ipw_weight(c(NA,1,1),g))
})

testthat::test_that("IPW", {

})

testthat::test_that("action_matrix", {
  A <- c(0,1,0,1)
  Aset <- c(0,1,2)
  res <- polle:::action_matrix(A, Aset)
  testthat::expect_true(ncol(res)==length(Aset))
  testthat::expect_true(nrow(res)==length(A))
  testthat::expect_true(all(!res[,3]))
  testthat::expect_true(all(res[,2]*1==A))
})
