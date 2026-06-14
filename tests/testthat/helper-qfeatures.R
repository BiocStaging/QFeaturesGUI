make_test_qfeatures <- function() {
    sample_data <- data.frame(
        batch = c("A", "A", "B"),
        condition = c("ctrl", "case", "ctrl"),
        sample_score = c(1, 2, 3),
        row.names = c("s1", "s2", "s3"),
        check.names = FALSE
    )

    mat1 <- matrix(
        seq_len(12),
        nrow = 4,
        byrow = TRUE,
        dimnames = list(
            c("f1", "f2", "f3", "f4"),
            rownames(sample_data)
        )
    )
    row_data1 <- data.frame(
        feature_class = c("pep", "pep", "prot", "prot"),
        score = c(0.2, 0.7, NA, 1.2),
        protein = c("P1", "P1", "P2", "P3"),
        row.names = rownames(mat1),
        check.names = FALSE
    )
    se1 <- SummarizedExperiment::SummarizedExperiment(
        assays = list(intensity = mat1),
        rowData = row_data1,
        colData = sample_data
    )

    mat2 <- matrix(
        c(10, 20, 30, 40, 50, 60),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(
            c("g1", "g2"),
            rownames(sample_data)
        )
    )
    row_data2 <- data.frame(
        feature_class = c("pep", "prot"),
        score = c(0.4, 0.8),
        protein = c("P4", "P5"),
        row.names = rownames(mat2),
        check.names = FALSE
    )
    se2 <- SummarizedExperiment::SummarizedExperiment(
        assays = list(intensity = mat2),
        rowData = row_data2,
        colData = sample_data
    )

    QFeatures::QFeatures(
        list(set1 = se1, set2 = se2),
        colData = sample_data
    )
}

normalise_test_data_frame <- function(df, sort_rows = TRUE) {
    df <- as.data.frame(df)
    df[] <- lapply(df, function(x) {
        if (is.factor(x)) {
            as.character(x)
        } else {
            x
        }
    })
    if (sort_rows && !is.null(rownames(df))) {
        df <- df[order(rownames(df)), , drop = FALSE]
    }
    df
}

normalise_test_sample_map <- function(object) {
    sample_map <- as.data.frame(MultiAssayExperiment::sampleMap(object))
    sample_map[] <- lapply(sample_map, as.character)
    sample_map <- sample_map[do.call(order, sample_map), , drop = FALSE]
    rownames(sample_map) <- NULL
    sample_map
}

expect_equal_data_frame_by_rowname <- function(object, expected) {
    object <- normalise_test_data_frame(object)
    expected <- normalise_test_data_frame(expected)
    testthat::expect_setequal(rownames(object), rownames(expected))
    testthat::expect_setequal(colnames(object), colnames(expected))
    object <- object[rownames(expected), colnames(expected), drop = FALSE]
    testthat::expect_equal(object, expected)
}

expect_equal_summarized_experiment_assays <- function(object, expected) {
    object_assays <- SummarizedExperiment::assays(object)
    expected_assays <- SummarizedExperiment::assays(expected)
    object_names <- names(object_assays)
    expected_names <- names(expected_assays)

    if (is.null(object_names) && is.null(expected_names)) {
        testthat::expect_equal(length(object_assays), length(expected_assays))
        for (i in seq_along(expected_assays)) {
            testthat::expect_equal(object_assays[[i]], expected_assays[[i]])
        }
        return(invisible(NULL))
    }

    if (is.null(object_names)) {
        object_names <- as.character(seq_along(object_assays))
    }
    if (is.null(expected_names)) {
        expected_names <- as.character(seq_along(expected_assays))
    }

    testthat::expect_setequal(object_names, expected_names)
    for (assay_name in expected_names) {
        object_index <- match(assay_name, object_names)
        expected_index <- match(assay_name, expected_names)
        testthat::expect_equal(
            object_assays[[object_index]],
            expected_assays[[expected_index]]
        )
    }
    invisible(NULL)
}

expect_qfeatures_equal <- function(object, expected) {
    testthat::expect_s4_class(object, "QFeatures")
    testthat::expect_s4_class(expected, "QFeatures")
    testthat::expect_equal(
        suppressMessages(QFeatures::getQFeaturesType(object)),
        suppressMessages(QFeatures::getQFeaturesType(expected))
    )
    testthat::expect_setequal(names(object), names(expected))
    expect_equal_data_frame_by_rowname(
        SummarizedExperiment::colData(object),
        SummarizedExperiment::colData(expected)
    )
    testthat::expect_equal(
        normalise_test_sample_map(object),
        normalise_test_sample_map(expected)
    )
    for (assay_name in sort(names(expected))) {
        object_assay <- object[[assay_name]]
        expected_assay <- expected[[assay_name]]

        expect_equal_summarized_experiment_assays(object_assay, expected_assay)
        expect_equal_data_frame_by_rowname(
            SummarizedExperiment::rowData(object_assay),
            SummarizedExperiment::rowData(expected_assay)
        )
        expect_equal_data_frame_by_rowname(
            SummarizedExperiment::colData(object_assay),
            SummarizedExperiment::colData(expected_assay)
        )
    }
}
