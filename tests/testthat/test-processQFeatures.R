test_that("processQFeatures can be constructed without a QFeatures object", {
    app <- processQFeatures()

    expect_s3_class(app, "shiny.appobj")
})
