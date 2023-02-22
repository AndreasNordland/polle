
library(testthat)

test_that("supp_warnings blocks a specific warning from a specific function",{

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

})



