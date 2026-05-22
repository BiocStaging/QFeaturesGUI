#' ui header builder
#'
#' @param title a string that refers to the title of the app
#'
#' @return the dashboardHeader object for the different apps
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shiny icon tags
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function(title) {
    online_documentation_button <- tags$li(
        class = "dropdown qfeatures-doc-menu",
        bs3Tooltip(
            trigger = tags$a(
                class = "qfeatures-doc-button",
                href = "https://rformassspectrometry.github.io/QFeaturesGUI/",
                target = "_blank",
                rel = "noopener noreferrer",
                `aria-label` = "Open the online documentation in a new page",
                icon("circle-question", class = "qfeatures-doc-button-icon"),
                tags$span(class = "sr-only", "Online Documentation")
            ),
            tooltipText = "Click to open the online documentation in a new page.",
            placement = "bottom"
        )
    )

    dashboardHeader(
        title = title,
        dropdownMenuOutput("exception_menu"),
        online_documentation_button
    )
}
