test_that("log_transform_existing_sets transforms only selected assays", {
    qf <- make_test_qfeatures()

    logged <- log_transform_existing_sets(
        qf,
        i = "set1",
        base = 2,
        pc = 1
    )

    expect_equal(
        SummarizedExperiment::assay(logged[["set1"]]),
        log2(SummarizedExperiment::assay(qf[["set1"]]) + 1),
        tolerance = 1e-12
    )
    expect_equal(
        SummarizedExperiment::assay(logged[["set2"]]),
        SummarizedExperiment::assay(qf[["set2"]])
    )
})

test_that("log_transform_existing_sets validates selected assays", {
    qf <- make_test_qfeatures()

    expect_error(
        log_transform_existing_sets(qf, i = "missing"),
        "were not found"
    )
    expect_error(
        log_transform_existing_sets(qf, i = 3),
        "out-of-bounds"
    )
    expect_error(
        log_transform_existing_sets(qf, i = c(TRUE, FALSE, TRUE)),
        "does not match"
    )
    expect_error(
        log_transform_existing_sets(qf, i = c(TRUE, NA)),
        "NA values"
    )
    expect_error(
        log_transform_existing_sets(qf, i = list("set1")),
        "must be numeric, logical, or character"
    )
})
