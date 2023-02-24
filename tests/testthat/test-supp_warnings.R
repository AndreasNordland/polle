
library(testthat)

test_that("supp_warnings blocks specific warnings from specific functions",{

  expect_warning(
    supp_warnings(
      expr = {
        warning("warn", call. = FALSE)
      },
      mess = "warn",
      fun = "test"
    ),
    "warn"
  )

  test <- function(){
    warning("warn", call. = TRUE)
  }

  expect_no_warning(
    supp_warnings(
      expr = test(),
      mess = "warn",
      fun = "test"
    )
  )

  test <- function(){
    warning("warn2", call. = FALSE)
  }

  expect_warning(
    supp_warnings(
      expr = test(),
      mess = "warn",
      fun = "test"
    )
  )


  test2 <- function(){
    warning("warn2", call. = TRUE)
  }
  test <- function(){
    warning("warn", call. = TRUE)
    test2()

  }

  expect_no_warning(
    supp_warnings(
      expr = test(),
      mess = c("warn", "warn2"),
      fun = c("test", "test2")
    )
  )

  expect_warning(
    supp_warnings(
      expr = test(),
      mess = c("warn", "warn2"),
      fun = c("test", "test3")
    )
  )

})



