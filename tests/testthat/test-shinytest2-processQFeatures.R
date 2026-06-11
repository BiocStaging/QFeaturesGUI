library(shinytest2)

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
  appObject <- QFeaturesGUI::processQFeatures(qf, prefilledSteps = c("zeroToNA", "logTransform", "sampleFiltering", "featureFiltering", "missingValuesFeatures",
             "missingValuesSamples", "normalisation", "aggregation", "join", "aggregation"))
  app <- AppDriver$new(appObject, 
      name = "processQFeatures", height = 1619, width = 1080)
  wait_for_input <- function(id, timeout = 10000) {
    app$wait_for_js(
      sprintf(
        "(() => { const el = document.getElementById('%s'); return !!(el && window.jQuery && window.jQuery(el).data('shinyInputBinding')); })()",
        id
      ),
      timeout = timeout
    )
  }
  wait_for_step <- function(step_number, timeout = 30000) {
    selector <- sprintf("a[data-value=\"step_%s\"]", step_number)
    app$wait_for_js(
      sprintf("document.querySelector('%s') !== null", selector),
      timeout = timeout
    )
    app$click(selector = selector)
  }

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
  wait_for_input("sampleFiltering_3_v1-filtering_1-annotation_selection")
  app$set_inputs(`sampleFiltering_3_v1-filtering_1-annotation_selection` = "SampleType")
  app$set_inputs(`sampleFiltering_3_v1-filtering_1-filter_operator` = "is_not_missing")
  app$click("sampleFiltering_3_v1-add_box")
  wait_for_input("sampleFiltering_3_v1-filtering_2-annotation_selection")
  app$set_inputs(`sampleFiltering_3_v1-filtering_2-annotation_selection` = "SampleType")
  wait_for_input("sampleFiltering_3_v1-filtering_2-filter_ui_samples")
  app$set_inputs(`sampleFiltering_3_v1-filtering_2-filter_ui_samples` = c("Monocyte", 
      "Macrophage"), wait_ = FALSE)
  app$click("sampleFiltering_3_v1-apply_filters")
  app$click("sampleFiltering_3_v1-export")
  app$wait_for_js("document.getElementById('featureFiltering_4_v1-add_box') !== null", timeout = 10000)
  app$click(selector = "a[data-value=\"step_4\"]")
  app$click("featureFiltering_4_v1-add_box")
  wait_for_input("featureFiltering_4_v1-filtering_1-annotation_selection")
  app$set_inputs(`featureFiltering_4_v1-filtering_1-annotation_selection` = "Potential.contaminant")
  app$set_inputs(`featureFiltering_4_v1-filtering_1-filter_operator` = "!=")
  wait_for_input("featureFiltering_4_v1-filtering_1-filter_ui_features")
  app$set_inputs(`featureFiltering_4_v1-filtering_1-filter_ui_features` = "+")
  app$click("featureFiltering_4_v1-add_box")
  wait_for_input("featureFiltering_4_v1-filtering_2-annotation_selection")
  app$set_inputs(`featureFiltering_4_v1-filtering_2-annotation_selection` = "Reverse")
  app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_operator` = "is_not_missing")
  app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_operator` = "!=")
  wait_for_input("featureFiltering_4_v1-filtering_2-filter_ui_features")
  app$set_inputs(`featureFiltering_4_v1-filtering_2-filter_ui_features` = "+")
  app$click("featureFiltering_4_v1-add_box")
  wait_for_input("featureFiltering_4_v1-filtering_3-annotation_selection")
  app$set_inputs(`featureFiltering_4_v1-filtering_3-annotation_selection` = "Length")
  app$set_inputs(`featureFiltering_4_v1-filtering_3-filter_operator` = "<=")
  wait_for_input("featureFiltering_4_v1-filtering_3-filter_ui_features")
  app$set_inputs(`featureFiltering_4_v1-filtering_3-filter_ui_features` = 15)
  app$click("featureFiltering_4_v1-apply_filters")
  app$click("featureFiltering_4_v1-export")
  app$wait_for_js("document.getElementById('missingValuesFeatures_5_v1-export') !== null", timeout = 10000)
  app$click(selector = "a[data-value=\"step_5\"]")
  app$set_inputs(`missingValuesFeatures_5_v1-threshold_features` = 0.75)
  app$click("missingValuesFeatures_5_v1-export")
  app$wait_for_js("document.getElementById('missingValuesSamples_6_v1-export') !== null", timeout = 10000)
  app$click(selector = "a[data-value=\"step_6\"]")
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
  wait_for_step(9)
  app$wait_for_js("document.getElementById('join_9_v1-export') !== null", timeout = 30000)
  app$set_inputs(`join_9_v1-feature_type` = "peptides")
  app$click("join_9_v1-export")
  wait_for_step(10)
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
