test_that("imputation method metadata is internally consistent", {
    specs <- imputation_method_specs()

    expect_named(specs, c("knn", "MinDet", "zero"))
    expect_equal(specs$knn$call_args, list(MARGIN = 1L))
    expect_equal(specs$MinDet$call_args, list(q = 0.01, MARGIN = 2L))
    expect_equal(specs$zero$call_args, list())

    expect_true(all(c("MinDet", "zero") %in% available_imputation_methods()))
    expect_type(assert_imputation_method_available("zero"), "list")
    expect_error(
        assert_imputation_method_available("unknown"),
        "Unknown imputation method"
    )
})
