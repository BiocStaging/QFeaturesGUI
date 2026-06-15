library(shinytest2)

js_string <- function(value) {
    paste0('"', gsub('(["\\\\])', "\\\\\\1", value), '"')
}

wait_for_process_input <- function(app, id, timeout = 10000) {
    app$wait_for_js(
        sprintf(
            "(() => { const el = document.getElementById('%s'); return !!(el && window.jQuery && window.jQuery(el).data('shinyInputBinding')); })()",
            id
        ),
        timeout = timeout
    )
}

wait_for_process_step <- function(app, step_number, timeout = 30000) {
    selector <- sprintf("a[data-value=\"step_%s\"]", step_number)
    app$wait_for_js(
        sprintf("document.querySelector('%s') !== null", selector),
        timeout = timeout
    )
    app$click(selector = selector)
}

wait_for_process_input_value <- function(app, id, value, timeout = 30000) {
    app$wait_for_js(
        sprintf(
            "window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues[%s] === %s",
            js_string(id),
            js_string(value)
        ),
        timeout = timeout
    )
}

wait_for_process_output_number <- function(app, id, value, timeout = 30000) {
    app$wait_for_js(
        sprintf(
            "(() => { const el = document.getElementById(%s); if (!el) return false; const matches = (el.textContent || '').match(/-?\\d+(\\.\\d+)?/g) || []; return matches.includes(%s); })()",
            js_string(id),
            js_string(as.character(value))
        ),
        timeout = timeout
    )
}

download_process_qfeatures <- function(app) {
    app$click(selector = "a[data-value=\"summary_tab\"]")
    app$wait_for_js(
        "document.getElementById('summary_tab-download_qfeatures') !== null && document.getElementById('summary_tab-download_qfeatures').offsetParent !== null",
        timeout = 10000
    )
    app$wait_for_js(
        "document.getElementById('summary_tab-download_qfeatures').getAttribute('href') !== ''",
        timeout = 10000
    )

    download <- app$get_download("summary_tab-download_qfeatures")
    testthat::expect_true(file.exists(download))
    testthat::expect_setequal(
        utils::unzip(download, list = TRUE)$Name,
        c(
            "processQFeatures_QFeatures_object.rds",
            "processQFeatures_sessionInfo.html",
            "processQFeatures_script.R"
        )
    )

    extract_dir <- tempfile("processqfeatures-download-")
    dir.create(extract_dir)
    utils::unzip(
        download,
        files = "processQFeatures_QFeatures_object.rds",
        exdir = extract_dir
    )
    readRDS(file.path(extract_dir, "processQFeatures_QFeatures_object.rds"))
}

make_process_test_qfeatures <- function() {
    qf <- make_test_qfeatures()
    set1 <- qf[["set1"]]
    set2 <- qf[["set2"]]

    SummarizedExperiment::assay(set1)[1, 1] <- 0
    SummarizedExperiment::assay(set2)[2, 2] <- 0
    SummarizedExperiment::colData(set1)$condition[2] <- NA
    SummarizedExperiment::colData(set2)$condition[2] <- NA
    SummarizedExperiment::rowData(set1)$feature_class[3] <- NA
    SummarizedExperiment::rowData(set2)$feature_class[2] <- NA

    sample_data <- as.data.frame(SummarizedExperiment::colData(set1))
    suppressMessages(QFeatures::QFeatures(
        list(set1 = set1, set2 = set2),
        colData = sample_data
    ))
}

add_expected_process_assays <- function(qfeatures, processed_qfeatures,
    step_number, type) {
    expected <- qfeatures
    for (assay_name in names(processed_qfeatures)) {
        expected[[paste0(assay_name, "_", type, "_", step_number)]] <-
            processed_qfeatures[[assay_name]]
    }
    expected
}

test_that("{shinytest2} recording: processQFeatures", {
    testthat::skip_on_cran()

    data("inputTable", package = "QFeaturesGUI")
    data("sampleTable", package = "QFeaturesGUI")
    qf <- QFeatures::readQFeatures(
        assayData = inputTable,
        colData = sampleTable,
        runCol = "Raw.file",
        quantCols = NULL,
        removeEmptyCols = TRUE,
        verbose = FALSE
    )
    appObject <- QFeaturesGUI::processQFeatures(qf, prefilledSteps = c(
        "zeroToNA", "logTransform", "sampleFiltering", "featureFiltering", "missingValuesFeatures",
        "missingValuesSamples", "normalisation", "aggregation", "join", "aggregation"
    ))
    app <- AppDriver$new(appObject,
        name = "processQFeatures", height = 1619, width = 1080
    )
    on.exit(app$stop(), add = TRUE)

    app$wait_for_js("document.getElementById('zeroToNA_1_v1-export') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_1\"]")
    app$click("zeroToNA_1_v1-export")
    app$wait_for_js("document.getElementById('logTransform_2_v1-apply_log_transform') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_2\"]")
    app$set_inputs(`logTransform_2_v1-log_base` = "log2", wait_ = FALSE)
    app$set_inputs(`logTransform_2_v1-color` = "NULL", wait_ = FALSE)
    app$set_inputs(`logTransform_2_v1-pseudocount` = 0, wait_ = FALSE)
    app$click("logTransform_2_v1-apply_log_transform")
    app$click("logTransform_2_v1-export")
    app$wait_for_js("document.getElementById('sampleFiltering_3_v1-add_box') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_3\"]")
    app$click("sampleFiltering_3_v1-add_box")
    wait_for_process_input(app, "sampleFiltering_3_v1-filtering_1-annotation_selection")
    app$set_inputs(`sampleFiltering_3_v1-filtering_1-annotation_selection` = "SampleType")
    app$set_inputs(`sampleFiltering_3_v1-filtering_1-filter_operator` = "is_not_missing")
    app$click("sampleFiltering_3_v1-add_box")
    wait_for_process_input(app, "sampleFiltering_3_v1-filtering_2-annotation_selection")
    app$set_inputs(`sampleFiltering_3_v1-filtering_2-annotation_selection` = "SampleType")
    wait_for_process_input(app, "sampleFiltering_3_v1-filtering_2-filter_ui_samples")
    app$set_inputs(`sampleFiltering_3_v1-filtering_2-filter_ui_samples` = c(
        "Monocyte",
        "Macrophage"
    ), wait_ = FALSE)
    app$click("sampleFiltering_3_v1-apply_filters")
    app$click("sampleFiltering_3_v1-export")
    app$wait_for_js("document.getElementById('featureFiltering_4_v1-add_box') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_4\"]")
    app$click("featureFiltering_4_v1-add_box")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_1-annotation_selection")
    app$set_inputs(`featureFiltering_4_v1-filtering_1-annotation_selection` = "Potential.contaminant")
    app$set_inputs(`featureFiltering_4_v1-filtering_1-filter_operator` = "!=")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_1-filter_ui_features")
    app$set_inputs(`featureFiltering_4_v1-filtering_1-filter_ui_features` = "+")
    app$click("featureFiltering_4_v1-add_box")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_2-annotation_selection")
    app$set_inputs(`featureFiltering_4_v1-filtering_2-annotation_selection` = "Reverse")
    app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_operator` = "is_not_missing")
    app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_operator` = "!=")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_2-filter_ui_features")
    app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_ui_features` = "+")
    app$click("featureFiltering_4_v1-add_box")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_3-annotation_selection")
    app$set_inputs(`featureFiltering_4_v1-filtering_3-annotation_selection` = "Length")
    app$set_inputs(`featureFiltering_4_v1-filtering_3-filter_operator` = "<=")
    wait_for_process_input(app, "featureFiltering_4_v1-filtering_3-filter_ui_features")
    app$set_inputs(`featureFiltering_4_v1-filtering_3-filter_ui_features` = 15)
    app$click("featureFiltering_4_v1-apply_filters")
    app$click("featureFiltering_4_v1-export")
    app$wait_for_js("document.getElementById('missingValuesFeatures_5_v1-export') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_5\"]")
    app$set_inputs(`missingValuesFeatures_5_v1-threshold_features` = 0.75)
    app$click("missingValuesFeatures_5_v1-export")
    wait_for_process_step(app, 6)
    app$wait_for_js("document.getElementById('missingValuesSamples_6_v1-export') !== null", timeout = 30000)
    app$set_inputs(`missingValuesSamples_6_v1-threshold_samples` = 0.5)
    app$click("missingValuesSamples_6_v1-export")
    app$wait_for_js("document.getElementById('normalisation_7_v1-apply_normalisation') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_7\"]")
    app$set_inputs(`normalisation_7_v1-method` = "diff.median")
    app$click("normalisation_7_v1-apply_normalisation")
    app$click("normalisation_7_v1-export")
    app$wait_for_js("document.getElementById('aggregation_8_v1-aggregate') !== null", timeout = 10000)
    app$click(selector = "a[data-value=\"step_8\"]")
    app$set_inputs(`aggregation_8_v1-method` = "colMedians")
    app$set_inputs(`aggregation_8_v1-fcol` = "Modified.sequence")
    app$click("aggregation_8_v1-aggregate")
    app$set_inputs(`aggregation_8_v1-features` = "_(Acetyl (Protein N-term))ATNFLAHEK_")
    app$click("aggregation_8_v1-export")
    wait_for_process_step(app, 9)
    app$wait_for_js("document.getElementById('join_9_v1-export') !== null", timeout = 30000)
    app$set_inputs(`join_9_v1-feature_type` = "peptides")
    app$click("join_9_v1-export")
    wait_for_process_step(app, 10)
    app$wait_for_js("document.getElementById('aggregation_10_v1-aggregate') !== null", timeout = 30000)
    app$set_inputs(`aggregation_10_v1-method` = "colMedians")
    app$set_inputs(`aggregation_10_v1-fcol` = "Leading.razor.protein")
    app$click("aggregation_10_v1-aggregate")
    app$set_inputs(`aggregation_10_v1-features` = "P84090")
    app$click("aggregation_10_v1-export")
    app$click(selector = "a[data-value=\"summary_tab\"]")
    app$wait_for_js(
        "document.getElementById('summary_tab-download_qfeatures') !== null && document.getElementById('summary_tab-download_qfeatures').offsetParent !== null",
        timeout = 10000
    )
    app$wait_for_js(
        "document.getElementById('summary_tab-download_qfeatures').getAttribute('href') !== ''",
        timeout = 10000
    )
    download <- app$get_download("summary_tab-download_qfeatures")
    testthat::expect_true(file.exists(download))
    testthat::expect_setequal(
        utils::unzip(download, list = TRUE)$Name,
        c(
            "processQFeatures_QFeatures_object.rds",
            "processQFeatures_sessionInfo.html",
            "processQFeatures_script.R"
        )
    )
})

test_that("{shinytest2}: zeroToNA exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "zeroToNA"),
        name = "processQFeatures_zeroToNA",
        height = 900,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('zeroToNA_1_v1-export') !== null", timeout = 10000)
    app$click("zeroToNA_1_v1-export")

    processed <- QFeatures::zeroIsNA(qf, i = seq_along(qf))
    expected <- add_expected_process_assays(qf, processed, 1, "zero_to_na")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})

test_that("{shinytest2}: logTransform exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "logTransform"),
        name = "processQFeatures_logTransform",
        height = 900,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('logTransform_1_v1-apply_log_transform') !== null", timeout = 10000)
    app$set_inputs(`logTransform_1_v1-log_base` = "log2", wait_ = FALSE)
    app$set_inputs(`logTransform_1_v1-color` = "NULL", wait_ = FALSE)
    app$set_inputs(`logTransform_1_v1-pseudocount` = 1, wait_ = FALSE)
    app$click("logTransform_1_v1-apply_log_transform")
    app$click("logTransform_1_v1-export")

    processed <- qf
    for (assay_name in names(processed)) {
        processed[[assay_name]] <- QFeatures::logTransform(
            qf[[assay_name]],
            base = 2,
            pc = 1
        )
    }
    expected <- add_expected_process_assays(qf, processed, 1, "log_transform")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})

test_that("{shinytest2}: sampleFiltering exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "sampleFiltering"),
        name = "processQFeatures_sampleFiltering",
        height = 1000,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('sampleFiltering_1_v1-add_box') !== null", timeout = 10000)
    app$click("sampleFiltering_1_v1-add_box")
    wait_for_process_input(app, "sampleFiltering_1_v1-filtering_1-annotation_selection")
    app$set_inputs(`sampleFiltering_1_v1-filtering_1-annotation_selection` = "condition")
    wait_for_process_input_value(
        app,
        "sampleFiltering_1_v1-filtering_1-annotation_selection",
        "condition"
    )
    wait_for_process_input_value(
        app,
        "sampleFiltering_1_v1-filtering_1-filter_operator",
        "=="
    )
    app$set_inputs(`sampleFiltering_1_v1-filtering_1-filter_operator` = "is_not_missing")
    wait_for_process_input_value(
        app,
        "sampleFiltering_1_v1-filtering_1-filter_operator",
        "is_not_missing"
    )
    app$click("sampleFiltering_1_v1-apply_filters")
    wait_for_process_output_number(
        app,
        "sampleFiltering_1_v1-number_samples_removed",
        1
    )
    app$click("sampleFiltering_1_v1-export")

    processed <- qf[, !is.na(SummarizedExperiment::colData(qf)$condition), ]
    expected <- add_expected_process_assays(qf, processed, 1, "samples_filtering")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})

test_that("{shinytest2}: featureFiltering exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "featureFiltering"),
        name = "processQFeatures_featureFiltering",
        height = 1000,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('featureFiltering_1_v1-add_box') !== null", timeout = 10000)
    app$click("featureFiltering_1_v1-add_box")
    wait_for_process_input(app, "featureFiltering_1_v1-filtering_1-annotation_selection")
    app$set_inputs(`featureFiltering_1_v1-filtering_1-annotation_selection` = "feature_class")
    wait_for_process_input_value(
        app,
        "featureFiltering_1_v1-filtering_1-annotation_selection",
        "feature_class"
    )
    wait_for_process_input_value(
        app,
        "featureFiltering_1_v1-filtering_1-filter_operator",
        "=="
    )
    app$set_inputs(`featureFiltering_1_v1-filtering_1-filter_operator` = "is_not_missing")
    wait_for_process_input_value(
        app,
        "featureFiltering_1_v1-filtering_1-filter_operator",
        "is_not_missing"
    )
    app$click("featureFiltering_1_v1-apply_filters")
    wait_for_process_output_number(
        app,
        "featureFiltering_1_v1-number_features_removed",
        2
    )
    app$click("featureFiltering_1_v1-export")

    processed <- qf
    for (assay_name in names(processed)) {
        keep <- !is.na(SummarizedExperiment::rowData(qf[[assay_name]])$feature_class)
        processed[[assay_name]] <- qf[[assay_name]][keep, ]
    }
    expected <- add_expected_process_assays(qf, processed, 1, "features_filtering")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})

test_that("{shinytest2}: normalisation exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "normalisation"),
        name = "processQFeatures_normalisation",
        height = 900,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('normalisation_1_v1-apply_normalisation') !== null", timeout = 10000)
    app$set_inputs(`normalisation_1_v1-method` = "diff.median")
    app$click("normalisation_1_v1-apply_normalisation")
    app$click("normalisation_1_v1-export")

    processed <- qf
    for (assay_name in names(processed)) {
        processed[[assay_name]] <- QFeatures::normalize(
            qf[[assay_name]],
            method = "diff.median"
        )
    }
    expected <- add_expected_process_assays(qf, processed, 1, "normalisation")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})

test_that("{shinytest2}: aggregation exports the expected QFeatures object", {
    testthat::skip_on_cran()

    qf <- make_process_test_qfeatures()
    app <- AppDriver$new(
        QFeaturesGUI::processQFeatures(qf, prefilledSteps = "aggregation"),
        name = "processQFeatures_aggregation",
        height = 900,
        width = 1200
    )
    on.exit(app$stop(), add = TRUE)

    wait_for_process_step(app, 1)
    app$wait_for_js("document.getElementById('aggregation_1_v1-aggregate') !== null", timeout = 10000)
    app$set_inputs(`aggregation_1_v1-method` = "colMedians")
    app$set_inputs(`aggregation_1_v1-fcol` = "protein")
    app$click("aggregation_1_v1-aggregate")
    app$click("aggregation_1_v1-export")

    processed <- qf
    for (assay_name in names(processed)) {
        processed[[assay_name]] <- suppressMessages(QFeatures::aggregateFeatures(
            qf[[assay_name]],
            fun = matrixStats::colMedians,
            fcol = "protein",
            na.rm = TRUE
        ))
    }
    expected <- add_expected_process_assays(qf, processed, 1, "aggregation")
    exported <- download_process_qfeatures(app)

    expect_qfeatures_equal(object = exported, expected = expected)
})
