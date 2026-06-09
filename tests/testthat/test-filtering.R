test_that("apply_filter_operator supports missingness operators", {
    values <- c("a", NA, "b", NA)

    expect_equal(
        apply_filter_operator(values, "is_missing", NULL),
        c(FALSE, TRUE, FALSE, TRUE)
    )
    expect_equal(
        apply_filter_operator(values, "is_not_missing", NULL),
        c(TRUE, FALSE, TRUE, FALSE)
    )
    expect_equal(
        apply_filter_operator(values, "==", "a"),
        c(TRUE, NA, FALSE, NA)
    )
    expect_equal(
        apply_filter_operator(values, "!=", c("a", "c")),
        c(FALSE, FALSE, TRUE, FALSE)
    )
    expect_equal(
        apply_filter_operator(c(1, 2, NA), ">=", 2),
        c(FALSE, TRUE, NA)
    )
    expect_error(
        apply_filter_operator(values, "contains", "a"),
        "Unsupported filtering operator"
    )
})

test_that("sample_filtering applies missingness conditions to sample annotations", {
    qf <- make_test_qfeatures()
    SummarizedExperiment::colData(qf)$condition <- c("ctrl", NA, "case")

    missing_samples <- sample_filtering(
        qf,
        list(list(
            annotation = "condition",
            operator = "is_missing",
            value = NULL
        ))
    )
    present_samples <- sample_filtering(
        qf,
        list(list(
            annotation = "condition",
            operator = "is_not_missing",
            value = NULL
        ))
    )

    expect_equal(rownames(SummarizedExperiment::colData(missing_samples)), "s2")
    expect_equal(
        rownames(SummarizedExperiment::colData(present_samples)),
        c("s1", "s3")
    )
})

test_that("feature_filtering applies missingness conditions to feature annotations", {
    qf <- make_test_qfeatures()

    missing_features <- feature_filtering(
        qf,
        list(list(
            annotation = "score",
            operator = "is_missing",
            value = NULL
        ))
    )
    present_features <- feature_filtering(
        qf,
        list(list(
            annotation = "score",
            operator = "is_not_missing",
            value = NULL
        ))
    )

    expect_equal(rownames(missing_features[["set1"]]), "f3")
    expect_equal(rownames(missing_features[["set2"]]), character())
    expect_equal(
        rownames(present_features[["set1"]]),
        c("f1", "f2", "f4")
    )
    expect_equal(rownames(present_features[["set2"]]), c("g1", "g2"))
})

test_that("codeGeneratorFiltering emits is.na conditions", {
    qf <- make_test_qfeatures()
    feature_code <- paste(codeGeneratorFiltering(
        qf = qf,
        condition = list(
            list(
                annotation = "score",
                operator = "is_missing",
                value = NULL
            ),
            list(
                annotation = "protein",
                operator = "!=",
                value = "P1"
            )
        ),
        type = "features",
        step_number = 1
    ), collapse = "\n")
    sample_code <- paste(codeGeneratorFiltering(
        qf = qf,
        condition = list(
            list(
                annotation = "condition",
                operator = "is_not_missing",
                value = NULL
            ),
            list(
                annotation = "batch",
                operator = "!=",
                value = "A"
            ),
            list(
                annotation = "sample_score",
                operator = ">=",
                value = 2
            )
        ),
        type = "samples",
        step_number = 1
    ), collapse = "\n")

    expect_true(grepl(
        'is.na(rowData(se)[["score"]])',
        feature_code,
        fixed = TRUE
    ))
    expect_true(grepl(
        ' & !is.na(rowData(se)[["protein"]]) & !(rowData(se)[["protein"]] %in% c("P1"))',
        feature_code,
        fixed = TRUE
    ))
    expect_true(grepl(
        '!is.na(colData(se)[["condition"]])',
        sample_code,
        fixed = TRUE
    ))
    expect_true(grepl(
        ' & !is.na(colData(se)[["batch"]]) & !(colData(se)[["batch"]] %in% c("A"))',
        sample_code,
        fixed = TRUE
    ))
    expect_true(grepl(
        ' & colData(se)[["sample_score"]] >= c(2)',
        sample_code,
        fixed = TRUE
    ))
})

test_that("missingness plot values explain TRUE and FALSE labels", {
    values <- c("a", NA, "b")

    is_missing_values <- missingness_filter_plot_values(values, "is_missing")
    is_not_missing_values <- missingness_filter_plot_values(
        values,
        "is_not_missing"
    )

    expect_equal(
        levels(is_missing_values),
        c("Is not missing", "Is missing")
    )
    expect_equal(
        as.character(is_missing_values),
        c("Is not missing", "Is missing", "Is not missing")
    )
    expect_equal(
        levels(is_not_missing_values),
        c("Is missing", "Is not missing")
    )
    expect_equal(
        as.character(is_not_missing_values),
        c("Is not missing", "Is missing", "Is not missing")
    )
})

test_that("missingness plot renders when all values are FALSE", {
    annotation <- missingness_filter_plot_values(c("a", "b"), "is_missing")
    plot <- missingness_annotation_plot_wrapper(
        annotation = annotation,
        filtered_annotation = annotation[FALSE],
        assay_name = "All samples across sets",
        annotation_name = "Is missing (condition)"
    )
    built <- plotly::plotly_build(plot)

    expect_s3_class(plot, "plotly")
    expect_equal(
        as.vector(built$x$data[[1]]$x),
        c("Is not missing", "Is missing")
    )
    expect_equal(as.vector(built$x$data[[1]]$y), c(2L, 0L))
})
