test_that("qfeatures_to_df summarises dimensions and strips GUI suffixes", {
    qf <- make_test_qfeatures()
    qf <- format_qfeatures(qf, c(1, 2))

    summary <- qfeatures_to_df(qf)

    expect_equal(summary$Name, c("set1", "set2"))
    expect_equal(summary$nFeatures, c(4, 2))
    expect_equal(summary$nSamples, c(3, 3))
    expect_equal(summary$nFeaturesMetadata, c(3, 3))
    expect_equal(summary$nSamplesMetadata, c(3, 3))
})

test_that("summarize_assays_to_df returns long assay data with annotations", {
    qf <- make_test_qfeatures()

    summary <- summarize_assays_to_df(
        qf,
        sample_column = "condition",
        feature_column = "feature_class"
    )

    expect_named(
        summary,
        c("PSM", "sample", "intensity", "sample_type", "feature_type"),
        ignore.order = TRUE
    )
    expect_equal(nrow(summary), 18L)

    row <- summary[summary$PSM == "f1" & summary$sample == "s1", ]
    expect_equal(row$intensity, 1)
    expect_equal(row$sample_type, "ctrl")
    expect_equal(row$feature_type, "pep")
})

test_that("annotation_cols reports row and column annotation names", {
    qf <- make_test_qfeatures()

    expect_equal(
        annotation_cols(qf, "rowData"),
        c("feature_class", "score", "protein")
    )
    expect_equal(
        annotation_cols(qf, "colData"),
        c("batch", "condition", "sample_score")
    )
    empty_qf <- suppressWarnings(suppressMessages(qf[, , FALSE]))
    expect_equal(annotation_cols(empty_qf, "rowData"), character())
})
