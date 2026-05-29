#' Server Builder for the processQFeatures app
#'
#' @param qfeatures a `QFeatures` object given by the user
#' @param initial_sets index of the base sets of the QFeatures
#' @param initial_steps prefilled workflow steps
#' @param has_qfeatures `logical(1)` indicating whether the app was launched
#'   with an initial QFeatures object
#'
#' @return return the server function for the processQFeatures app.
#' @rdname INTERNAL_build_process_server
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#' @importFrom shiny observeEvent
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyalert shinyalert
#'
build_process_server <- function(qfeatures, initial_sets, initial_steps, has_qfeatures = TRUE) {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            id = character(),
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character()),
            stringsAsFactors = FALSE
        )
        .qf$qfeatures <- if (has_qfeatures) {
            format_qfeatures(qfeatures, initial_sets)
        } else {
            NULL
        }
        global_rv$workflow_config <- if (has_qfeatures) initial_steps else character(0)
        global_rv$code_lines <- list()
        global_rv$step_rvs <- list()
        server_exception_menu(input, output, session)
        server_sidebar(input, output, session)
        server_module_workflow_config("workflow_config")
        server_dynamic_workflow(input, output, session)
        server_module_summary_tab("summary_tab")

        uploaded_qfeatures <- shiny::reactiveVal(NULL)
        upload_message <- shiny::reactiveVal(NULL)

        output$startup_initial_sets_ui <- shiny::renderUI({
            uploaded <- uploaded_qfeatures()
            if (is.null(uploaded)) {
                return(shiny::p(
                    "Upload an .rds file containing a QFeatures object to",
                    "choose the initial sets."
                ))
            }

            shiny::selectizeInput(
                "startup_initial_sets",
                "Initial sets",
                choices = names(uploaded),
                selected = names(uploaded),
                multiple = TRUE,
                width = "100%",
                options = list(
                    plugins = list("remove_button"),
                    placeholder = "Choose one or more initial sets"
                )
            )
        })

        output$startup_upload_message <- shiny::renderUI({
            msg <- upload_message()
            if (is.null(msg)) {
                return(NULL)
            }
            shiny::tags$div(class = "text-danger", msg)
        })

        show_startup_upload_modal <- function() {
            shiny::showModal(shiny::modalDialog(
                title = "Load a QFeatures object",
                shiny::p(
                    "processQFeatures was started without a QFeatures object.",
                    "Upload an .rds file containing one, then choose which",
                    "sets should be used as the initial sets."
                ),
                shiny::fileInput(
                    "startup_qfeatures_rds",
                    "QFeatures RDS file",
                    accept = c(".rds", ".Rds", ".RDS")
                ),
                shiny::uiOutput("startup_initial_sets_ui"),
                shiny::uiOutput("startup_upload_message"),
                easyClose = FALSE,
                size = "l",
                footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    shiny::actionButton(
                        "startup_load_qfeatures",
                        "Load QFeatures",
                        class = "btn-primary"
                    )
                )
            ))
        }

        shiny::observeEvent(input$startup_qfeatures_rds, {
            uploaded_qfeatures(NULL)
            upload_message(NULL)

            uploaded <- tryCatch(
                check_qfeatures(input$startup_qfeatures_rds$datapath),
                error = function(e) e
            )
            if (inherits(uploaded, "error")) {
                upload_message(paste(
                    "Could not load QFeatures object:",
                    conditionMessage(uploaded)
                ))
                return(invisible(NULL))
            }

            uploaded_qfeatures(uploaded)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$startup_load_qfeatures, {
            uploaded <- uploaded_qfeatures()
            if (is.null(uploaded)) {
                upload_message(
                    "Upload a valid .rds file containing a QFeatures object."
                )
                return(invisible(NULL))
            }

            selected_sets <- input$startup_initial_sets
            initial_idx <- tryCatch(
                normalise_initial_sets(uploaded, selected_sets),
                error = function(e) e
            )
            if (inherits(initial_idx, "error")) {
                upload_message(conditionMessage(initial_idx))
                return(invisible(NULL))
            }

            workflow_steps <- input[["workflow_config-workflow_list"]]
            if (is.null(workflow_steps)) {
                workflow_steps <- initial_steps
            }

            .qf$qfeatures <- format_qfeatures(uploaded, initial_idx)
            global_rv$workflow_config <- workflow_steps
            global_rv$code_lines <- list()
            shiny::removeModal()

            n_sets <- length(initial_idx)
            n_steps <- length(workflow_steps)
            shinyalert(
                title = "QFeatures loaded",
                text = paste0(
                    "Loaded QFeatures with ", n_sets,
                    " initial set", if (n_sets != 1) "s" else "", ".",
                    if (n_steps > 0) {
                        paste0(
                            "\nWorkflow pre-configured with ", n_steps,
                            " step", if (n_steps != 1) "s" else "", "."
                        )
                    } else {
                        ""
                    }
                ),
                closeOnClickOutside = TRUE,
                type = "success",
                confirmButtonCol = "#3c8dbc"
            )
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$startup_show_upload, {
            show_startup_upload_modal()
        }, ignoreInit = TRUE)

        if (!has_qfeatures) {
            session$onFlushed(function() {
                show_startup_upload_modal()
            }, once = TRUE)
            return(invisible(NULL))
        }

        n_sets <- length(initial_sets)
        n_steps <- length(initial_steps)
        shinyalert(
            title = "App ready",
            text = paste0(
                "Loaded QFeatures with ", n_sets,
                " initial set", if (n_sets != 1) "s" else "", ".",
                if (n_steps > 0) {
                    paste0(
                        "\nWorkflow pre-configured with ", n_steps,
                        " step", if (n_steps != 1) "s" else "", "."
                    )
                } else {
                    ""
                }
            ),
            closeOnClickOutside = TRUE,
            type = "success",
            confirmButtonCol = "#3c8dbc"
        )
    }

    server
}
