test_that("feature and sample removal metrics are computed from dimensions", {
    before <- make_test_qfeatures()
    after_features <- before
    after_features[["set1"]] <- after_features[["set1"]][1:2, ]
    after_features[["set2"]] <- after_features[["set2"]][1, , drop = FALSE]
    after_samples <- before[, c(TRUE, FALSE, TRUE), ]

    expect_equal(count_features_rows(before), 6L)
    expect_equal(number_removed(before, after_features, "features"), 3L)
    expect_equal(percent_removed(before, after_features, "features"), 50)
    expect_equal(number_removed(before, after_samples, "samples"), 1L)
    expect_equal(percent_removed(before, after_samples, "samples"), 33.3)
    expect_error(number_removed(before, after_samples, "bad_type"))
})

test_that("is_empty_set detects assays without rows or columns", {
    qf <- make_test_qfeatures()

    expect_true(is_empty_set(qf[["set1"]][0, ]))
    expect_true(is_empty_set(qf[["set1"]][, 0]))
    expect_false(is_empty_set(qf[["set1"]]))
})
