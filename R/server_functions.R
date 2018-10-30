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

#' Delta Ct Boxplot
#'
#' Plots boxplot and statistics from qPCR experiments.
#'
#' @param data Tibble or data frame with delta Ct values and
#'   sample groups info
#' @param control_group Sample group used to compare statistics.
#'   This group is used as the first group to plot.
#' @param notch_value If TRUE, make a notched box plot,
#'   instead of normal boxplot. Default = FALSE.
#' @param cutoff Detection threshold for qPCR experiment.
#' @param log_transform If TRUE,log10-transform the delta Ct values.
#'   Default = FALSE.
#' @param point_size Value for the size of plot elements.
#' @param text_size Value for the size of plot textual elements.
#' @export
plot_boxplot <- function(data,
                         control_group = NULL,
                         notch_value = FALSE,
                         cutoff = "-7",
                         log_transform = FALSE,
                         point_size = 2,
                         text_size = 16){
  # data <- delta_ct_df
  set.seed(42)
  if (is.null(data)) {
    plot_1 <- ggplot2::ggplot()
    return(plot_1)
  }
  text_size <- as.numeric(text_size)
  point_size <- as.numeric(point_size)
  cutoff <- as.numeric(cutoff)
  column_names <- names(data)
  plot_data <- data %>%
    dplyr::rename(group = !!as.name(column_names[1])) %>%
    dplyr::rename(values = !!as.name(column_names[3]))

  if (isTRUE(log_transform)) {
    plot_data <- plot_data %>%
      dplyr::mutate(values = base::log10(values))
  }

  plot_data <- plot_data %>%
    dplyr::mutate(values_boxplot = dplyr::if_else(values > cutoff, values, NA_real_)) %>%
    dplyr::mutate(values_undetected = dplyr::if_else(values <= cutoff, values, NA_real_))

  group_levels <- plot_data %>%
    dplyr::pull(group) %>%
    base::unique()
  if (is.null(control_group)) {
    control_group <- group_levels[1]
  }
  if (isTRUE(control_group == " ")) {
    control_group <- group_levels[1]
  }
  group_sans_control <- group_levels[!(group_levels %in% control_group)]
  if (isTRUE(length(group_levels) > 2)) {
    statistics_comparison <- group_sans_control %>%
      purrr::map(~{c(control_group, .x)}, control_group)
  } else {
    statistics_comparison <- list(c(control_group, group_sans_control))
  }

  group_length <- length(group_levels)
  if (isTRUE(group_length < 3)) {
   group_length <- 3
  }
  color_palette <- RColorBrewer::brewer.pal(n = group_length, name = "Set2")
  breaks_y <- base::pretty(plot_data$values)

  plot_data <- plot_data %>%
    dplyr::mutate(
      group = factor(group,
        levels = c(control_group,
                   group_levels[!(group_levels %in% control_group)]))
    )
  plot_1 <- plot_data %>%
    ggplot2::ggplot( ggplot2::aes(x = group, y = values_boxplot)) +
    ggplot2::geom_hline(
      yintercept = cutoff, alpha = 0.3
    ) +
    ggplot2::stat_boxplot(
      ggplot2::aes(y = values_boxplot, color = group),
      geom = "errorbar", width = point_size/45
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(y = values_boxplot, color = group), width = point_size/6,
      notch = notch_value,
      outlier.colour = "white"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = values_boxplot, color = group),
      size = point_size, alpha = 1,
      position = ggplot2::position_jitter(w = point_size/20, h = 0)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = values_undetected, color = group),
      shape = 21, fill = "white",  size = point_size, alpha = 1,
      position = ggplot2::position_jitter(w = point_size/20, h = 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = breaks_y, limits = base::range(breaks_y)
    ) +
    ggplot2::scale_color_manual(values = color_palette) +
    ggpubr::theme_pubr(base_size = text_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 45, hjust = 1, vjust = 1
    )) +
    ggplot2::xlab("") +
    ggplot2::ylab("RNA levels") +
    ggplot2::labs(color = "") +
  #plot_1 <- plot_1 +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      comparisons = statistics_comparison
    )

  plot_1
}
