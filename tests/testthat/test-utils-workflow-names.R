test_that("QFeaturesGUI name helpers add, remove, and parse step suffixes", {
    qf <- make_test_qfeatures()
    formatted <- format_qfeatures(qf, c(1, 2))

    expect_equal(
        names(formatted),
        c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#0)")
    )
    expect_equal(
        remove_QFeaturesGUI(names(formatted)),
        c("set1", "set2")
    )
    expect_equal(
        qfeaturesgui_step_number(
            c("raw", "set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#12)_x")
        ),
        c(NA_integer_, 0L, 12L)
    )
})

test_that("page_assays_subset selects assays by fixed name pattern", {
    qf <- make_test_qfeatures()
    names(qf) <- c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#1)")

    initial <- page_assays_subset(qf, "_(QFeaturesGUI#0)")
    step1 <- page_assays_subset(qf, "_(QFeaturesGUI#1)")
    missing <- page_assays_subset(qf, "_(QFeaturesGUI#9)")

    expect_equal(names(initial), "set1_(QFeaturesGUI#0)")
    expect_equal(names(step1), "set2_(QFeaturesGUI#1)")
    expect_length(missing, 0)
})

test_that("downstream invalidation messages describe affected workflow steps", {
    expect_equal(downstream_invalidation_message(integer()), "")
    expect_match(
        downstream_invalidation_message(2L),
        "1 downstream saved step"
    )
    expect_match(
        downstream_invalidation_message(c(2L, 3L)),
        "2 downstream saved steps"
    )
})

test_that("upper_first capitalizes only the first character", {
    expect_equal(upper_first("protein"), "Protein")
    expect_equal(upper_first("protein group"), "Protein group")
})
