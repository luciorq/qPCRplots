#' @export
app_ui <- function() {
  #library(shinydashboard)
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "qPCRplots"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "deltaCT plots", tabName = "deltaCTplot", icon = shiny::icon("chart-bar")
        ),
        shinydashboard::menuItem("About", tabName = "about",
                                 icon = shiny::icon("info"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        # First tab content
        shinydashboard::tabItem(
          tabName = "deltaCTplot",
          shiny::fluidRow(
            shinydashboard::box(
              shiny::plotOutput("plot_deltaCT")
            ),
            shinydashboard::box(
              title = "Controls",
              shiny::fileInput(
                inputId = "file_deltaCT",
                "Choose Excel file",
                accept = c(".xls", ".xlsx")
              ),
              shiny::tags$hr(),
              shiny::checkboxInput("plot_1_notch", "Apply notch", FALSE),
              shiny::textInput(
                inputId = "threshold",
                label = "deltaCT detection threshold",
                value = 1e-7
              )
            )
          ),
        shiny::fluidRow()
        ),
        # Second tab content
        shinydashboard::tabItem(
          tabName = "about",
          shiny::h2("qPCRplots package"),
          shiny::p("App developed by")
        )
      )
    )
  )

}

