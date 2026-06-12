library(shinytest2)

test_that("{shinytest2}: twoTable_importQFeatures", {
  testthat::skip_on_cran()

  data("inputTable", package = "QFeaturesGUI")
  data("sampleTable", package = "QFeaturesGUI")

  appObject <- importQFeatures(
    colData = sampleTable,
    assayData = inputTable
  )

  app <- AppDriver$new(
    appObject,
    name = "twoTable_importQFeatures",
    height = 1080,
    width = 1619
  )

  app$wait_for_idle()

  app$set_inputs(`readqfeatures-run_col` = "Raw.file")
  app$set_inputs(`readqfeatures-removeEmptyCols` = TRUE)
  app$set_inputs(`readqfeatures-singlecell` = TRUE)
  app$click("readqfeatures-convert")

  app$wait_for_idle()

  download <- app$get_download("readqfeatures-downloadQFeatures")
  testthat::expect_true(file.exists(download))
  testthat::expect_setequal(
    utils::unzip(download, list = TRUE)$Name,
    c(
      "importQFeatures_QFeatures_object.rds",
      "importQFeatures_sessionInfo.html",
      "importQFeatures_script.R"
    )
  )

  extract_dir <- tempfile("qfeatures-download-")
  dir.create(extract_dir)
  utils::unzip(
    download,
    files = c(
      "importQFeatures_QFeatures_object.rds",
      "importQFeatures_script.R"
    ),
    exdir = extract_dir
  )

  exported <- readRDS(file.path(
    extract_dir,
    "importQFeatures_QFeatures_object.rds"
  ))

  expected <- QFeatures::readQFeatures(
    assayData = inputTable,
    colData = sampleTable,
    runCol = "Raw.file",
    quantCols = NULL,
    removeEmptyCols = TRUE,
    verbose = FALSE
  )
  expected <- QFeatures::zeroIsNA(expected, i = seq_along(expected))
  for (i in seq_along(expected)) {
    expected[[i]] <- QFeatures::logTransform(expected[[i]], base = 2)
  }
  expected <- QFeatures::setQFeaturesType(expected, type = "scp")
  names(expected) <- paste0(names(expected), "_initial_import")

  expect_qfeatures_equal(object = exported, expected = expected)
  script_env <- new.env(parent = globalenv())
  script_env$dataFrame1 <- inputTable
  script_env$dataFrame2 <- sampleTable
  suppressPackageStartupMessages(source(
    file.path(extract_dir, "importQFeatures_script.R"),
    local = script_env
  ))

  testthat::expect_true(exists("qfeatures", envir = script_env, inherits = FALSE))
  script_qfeatures <- script_env$qfeatures

  expect_qfeatures_equal(object = script_qfeatures, expected = exported)
})

test_that("{shinytest2}: oneTable_importQFeatures", {
  testthat::skip_on_cran()

  data("inputTable", package = "QFeaturesGUI")

  appObject <- importQFeatures(
    assayData = inputTable
  )

  app <- AppDriver$new(
    appObject,
    name = "oneTable_importQFeatures",
    height = 1080,
    width = 1619
  )

  app$wait_for_idle()

  
  app$set_inputs(`readqfeatures-quant_cols` = c("Reporter.intensity.1", "Reporter.intensity.2", "Reporter.intensity.16", "Reporter.intensity.15", "Reporter.intensity.14", "Reporter.intensity.13", "Reporter.intensity.12", "Reporter.intensity.11", "Reporter.intensity.10", "Reporter.intensity.9", "Reporter.intensity.8", "Reporter.intensity.7", "Reporter.intensity.6", "Reporter.intensity.5", "Reporter.intensity.4", "Reporter.intensity.3"))
  app$set_inputs(`readqfeatures-run_col` = "Raw.file")
  app$set_inputs(`readqfeatures-zero_as_NA` = FALSE)
  app$set_inputs(`readqfeatures-logTransform` = FALSE)
  app$set_inputs(`readqfeatures-removeEmptyCols` = TRUE)
  app$click("readqfeatures-convert")

  app$wait_for_idle()

  download <- app$get_download("readqfeatures-downloadQFeatures")
  testthat::expect_true(file.exists(download))
  testthat::expect_setequal(
    utils::unzip(download, list = TRUE)$Name,
    c(
      "importQFeatures_QFeatures_object.rds",
      "importQFeatures_sessionInfo.html",
      "importQFeatures_script.R"
    )
  )

  extract_dir <- tempfile("qfeatures-download-")
  dir.create(extract_dir)
  utils::unzip(
    download,
    files = c(
      "importQFeatures_QFeatures_object.rds",
      "importQFeatures_script.R"
    ),
    exdir = extract_dir
  )

  exported <- readRDS(file.path(
    extract_dir,
    "importQFeatures_QFeatures_object.rds"
  ))

  expected <- QFeatures::readQFeatures(
    assayData = inputTable,
    runCol = "Raw.file",
    quantCols = c("Reporter.intensity.1", "Reporter.intensity.2", "Reporter.intensity.16", "Reporter.intensity.15", "Reporter.intensity.14", "Reporter.intensity.13", "Reporter.intensity.12", "Reporter.intensity.11", "Reporter.intensity.10", "Reporter.intensity.9", "Reporter.intensity.8", "Reporter.intensity.7", "Reporter.intensity.6", "Reporter.intensity.5", "Reporter.intensity.4", "Reporter.intensity.3"),
    removeEmptyCols = TRUE,
    verbose = FALSE
  )
  names(expected) <- paste0(names(expected), "_initial_import")

  expect_qfeatures_equal(object = exported, expected = expected)
  script_env <- new.env(parent = globalenv())
  script_env$dataFrame1 <- inputTable
  suppressPackageStartupMessages(source(
    file.path(extract_dir, "importQFeatures_script.R"),
    local = script_env
  ))

  testthat::expect_true(exists("qfeatures", envir = script_env, inherits = FALSE))
  script_qfeatures <- script_env$qfeatures

  expect_qfeatures_equal(object = script_qfeatures, expected = exported)
})
