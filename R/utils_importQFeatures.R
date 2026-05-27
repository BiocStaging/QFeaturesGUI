# Import conversion helpers ----

#' Log transform selected QFeatures sets without creating new sets
#'
#' @param object `QFeatures` object to log transform
#' @param i sets to transform; numeric, logical, or character assay identifiers
#' @param base `numeric` base of the log transformation
#' @param pc `numeric` pseudocount to add to the data
#' @return `QFeatures` object with the selected sets replaced by their log
#'   transformed version
#' @rdname INTERNAL_log_transform_existing_sets
#' @keywords internal
#' @importFrom QFeatures logTransform
#'
log_transform_existing_sets <- function(object, i = seq_along(object), base = 2, pc = 0) {
    all_set_names <- names(object)
    if (is.character(i)) {
        set_names <- i
        missing_sets <- setdiff(set_names, all_set_names)
        if (length(missing_sets) > 0L) {
            stop(
                "The following QFeatures set(s) were not found: ",
                paste(missing_sets, collapse = ", "),
                call. = FALSE
            )
        }
    } else if (is.numeric(i)) {
        set_names <- all_set_names[i]
        if (anyNA(set_names)) {
            stop("`i` contains out-of-bounds QFeatures set indices.", call. = FALSE)
        }
    } else if (is.logical(i)) {
        if (length(i) != length(all_set_names)) {
            stop(
                "`i` is logical but its length (", length(i),
                ") does not match the number of QFeatures sets (",
                length(all_set_names), ").",
                call. = FALSE
            )
        }
        if (anyNA(i)) {
            stop("`i` contains NA values.", call. = FALSE)
        }
        set_names <- all_set_names[i]
    } else {
        stop("`i` must be numeric, logical, or character.", call. = FALSE)
    }

    for (name in set_names) {
        object[[name]] <- QFeatures::logTransform(
            object = object[[name]],
            base = base,
            pc = pc
        )
    }
    object
}
