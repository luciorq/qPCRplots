#' qPCRplots Shiny UI
#'
#' This is the user-interface definition of a Shiny web application.
#' Define UI for application.
#'
#' @export
app_ui <- function() {
  #library(shinydashboard)
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "qPCRplots"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "deltaCT plots", tabName = "deltaCTplot", icon = shiny::icon("fa-chart-bar")
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
              shiny::checkboxInput("plot_1_log_transform", "Log10 transform", FALSE),
              shiny::selectInput(
                inputId = "plot_1_control_group",
                label = "Select control group:",
                choices =  c(" "), selected = " "
              ),
              shiny::textInput(
                inputId = "plot_1_threshold",
                label = "deltaCT detection threshold",
                value = "-7"
              ),
              shiny::sliderInput(
                inputId = "plot_1_point_size",
                label = "Dot size",
                min = 1, max = 10, value = 2,
                step = 1, width = "90%"
              ),
              shiny::sliderInput(
                inputId = "plot_1_text_size",
                label = "Text size",
                min = 8, max = 24, value = 16,
                step = 2, width = "90%"
              ),
            shiny::downloadButton('plot_1_download',"Download Figure")
            )
          ),
        shiny::fluidRow()
        ),
        # Second tab content
        shinydashboard::tabItem(
          tabName = "about",
          shiny::h2("qPCRplots package"),
          shiny::p("App developed by Lucio Rezende Queiroz"),
          shiny::em("Code available at: github.com/luciorq/qPCRplots")
        )
      )
    )
  )

}

