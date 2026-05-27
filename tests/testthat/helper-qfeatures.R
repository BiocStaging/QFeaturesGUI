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
