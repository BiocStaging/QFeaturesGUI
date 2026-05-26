test_that("normalise_initial_sets supports valid selector types", {
    qf <- make_test_qfeatures()

    expect_equal(normalise_initial_sets(qf, c(2, 1, 2)), c(2L, 1L))
    expect_equal(normalise_initial_sets(qf, c(TRUE, FALSE)), 1L)
    expect_equal(normalise_initial_sets(qf, c("set2", "set1")), c(2L, 1L))
})

test_that("normalise_initial_sets rejects invalid selectors", {
    qf <- make_test_qfeatures()

    expect_error(
        normalise_initial_sets(qf, NULL),
        "select at least one assay"
    )
    expect_error(
        normalise_initial_sets(qf, logical(0)),
        "select at least one assay"
    )
    expect_error(
        normalise_initial_sets(qf, c(TRUE, FALSE, TRUE)),
        "does not match the number of assays"
    )
    expect_error(
        normalise_initial_sets(qf, 3),
        "out-of-bounds"
    )
    expect_error(
        normalise_initial_sets(qf, NA_real_)
    )
    expect_error(
        normalise_initial_sets(qf, "missing"),
        "not found"
    )
    expect_error(
        normalise_initial_sets(qf, list("set1")),
        "must be numeric, logical, or character"
    )
})

test_that("check_qfeatures validates objects and RDS paths", {
    qf <- make_test_qfeatures()

    expect_s4_class(check_qfeatures(qf), "QFeatures")

    path <- tempfile(fileext = ".rds")
    saveRDS(qf, path)
    expect_s4_class(check_qfeatures(path), "QFeatures")

    expect_error(check_qfeatures(), "argument is missing")
    expect_error(
        check_qfeatures(tempfile(fileext = ".rds")),
        "does not exist"
    )
    expect_error(
        check_qfeatures(data.frame(x = 1)),
        "must be a QFeatures object"
    )

    bad_path <- tempfile(fileext = ".rds")
    saveRDS(data.frame(x = 1), bad_path)
    expect_error(
        check_qfeatures(bad_path),
        "must be a QFeatures object"
    )
})

test_that("check_prefilled_steps maps valid workflow identifiers", {
    expect_equal(
        check_prefilled_steps(c("sampleFiltering", "normalisation", "join")),
        c("Sample Filtering", "Normalisation", "Join")
    )
    expect_equal(check_prefilled_steps(character()), character())
    expect_error(
        check_prefilled_steps(c("sampleFiltering", "badStep")),
        "Unknown workflow steps: badStep"
    )
})
