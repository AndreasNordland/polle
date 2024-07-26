test_that("get_element", {
    x <- list(ab = "test", b = NULL, c = NA, d = vector())

    expect_equal(
        get_element(x, "ab"),
        "test"
    )

    expect_equal(
        get_element(x, "b"),
        NULL
    )

    expect_equal(
        get_element(x, "c"),
        NA
    )

    expect_equal(
        get_element(x, "d"),
        vector()
    )

    expect_error(
        get_element(x, "a"),
        "named element does not exist"
    )

    expect_error(
        get_element(x, 1),
        "'name' must be a character string"
    )

    expect_equal(
        get_element(x, "a", check_name = FALSE),
        NULL
    )
})
