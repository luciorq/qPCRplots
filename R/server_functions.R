#' qPCRplots Shiny Server
#'
#' This is the server side logic of a Shiny web application.
#' Define server logic required by the app.
#'
#' @param input User inputs.
#' @param output Application outputs.
#' @param session Session status.
#' @export
app_server <- function(input, output, session) {

  updated_groups <- shiny::reactive({
    shiny::req(input$file_deltaCT)
    delta_ct_df <- readxl::read_excel(input$file_deltaCT$datapath)
    column_names <- names(delta_ct_df)
    delta_ct_df %>%
      dplyr::rename(group = !!as.name(column_names[1])) %>%
      dplyr::pull(group)
  })

  shiny::observe({
    shiny::updateSelectInput(
      session, inputId = "plot_1_control_group",
      choices = updated_groups(), selected = " ")
  })

  output$plot_deltaCT <- shiny::renderPlot({
    shiny::req(input$file_deltaCT)
    delta_ct_df <- readxl::read_excel(input$file_deltaCT$datapath)
    qPCRplots::plot_boxplot(
      data = delta_ct_df,
      control_group = input$plot_1_control_group,
      notch_value = input$plot_1_notch,
      cutoff = input$plot_1_threshold,
      log_transform = input$plot_1_log_transform,
      point_size = input$plot_1_point_size,
      text_size = input$plot_1_text_size
    )
  })

  output$plot_1_download <- shiny::downloadHandler(
    filename = function() { paste0('plot_1.pdf') },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = qPCRplots::plot_boxplot(
        data = readxl::read_excel(input$file_deltaCT$datapath),
        control_group = input$plot_1_control_group,
        notch_value = input$plot_1_notch,
        cutoff = input$plot_1_threshold,
        log_transform = input$plot_1_log_transform,
        point_size = input$plot_1_point_size,
        text_size = input$plot_1_text_size
      ), device = "pdf")
    }
  )

}
