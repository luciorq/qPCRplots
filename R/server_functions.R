#' @export
plot_boxplot <- function(data, notch_value = FALSE){
  # data <- delta_ct_df
  set.seed(42)
  if (is.null(data)) {
    plot_1 <- ggplot2::ggplot()
    return(plot_1)
  }
  column_names <- names(data)
  plot_data <- data %>%
    dplyr::rename(group = !!as.name(column_names[1])) %>%
    dplyr::rename(values = !!as.name(column_names[3])) %>%
    dplyr::mutate(values_boxplot = dplyr::if_else(values > -7, values, NA_real_))

  plot_1 <- plot_data %>%
    ggplot2::ggplot( ggplot2::aes(x = group)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(y = values_boxplot),
      notch = notch_value,
      outlier.colour = "white") +
    ggplot2::geom_jitter(
      ggplot2::aes(y = values)
    ) +
    ggpubr::theme_pubr()
  plot_1
}


#' @export
app_server <- function(input, output) {

  output$plot_deltaCT <- shiny::renderPlot({
    shiny::req(input$file_deltaCT)
    delta_ct_df <- readxl::read_excel(input$file_deltaCT$datapath)
    qPCRplots::plot_boxplot(delta_ct_df, input$plot_1_notch)
  })
}
