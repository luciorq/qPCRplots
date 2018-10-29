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
      point_size = input$plot_1_point_size
    )
  })

  output$plot_1_download <- shiny::downloadHandler(
    filename = function() { paste0('plot_1.pdf') },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = qPCRplots::plot_boxplot(
        data = delta_ct_df,
        control_group = input$plot_1_control_group,
        notch_value = input$plot_1_notch,
        cutoff = input$plot_1_threshold,
        log_transform = input$plot_1_log_transform,
        point_size = input$plot_1_point_size
      ), device = "pdf")
    }
  )

}

#' @export
plot_boxplot <- function(data,
                         control_group = NULL,
                         notch_value = FALSE,
                         cutoff = "-7",
                         log_transform = FALSE,
                         point_size = 2){
  # data <- delta_ct_df
  set.seed(42)
  if (is.null(data)) {
    plot_1 <- ggplot2::ggplot()
    return(plot_1)
  }
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
  if (isTRUE(control_group == " ")) {
    control_group <- group_levels[1]
  }

  if (isTRUE(length(group_levels) > 2)) {
    statistics_comparison <- qPCRplots::group_comparisons(group_levels, control_group)
  } else {
    statistics_comparison <- list(c(control_group, group_levels[!(group_levels %in% control_group)]))
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
      geom = "errorbar", width = point_size/50
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(y = values_boxplot, color = group), width = point_size/8,
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
    ggpubr::theme_pubr() +
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

#' @export
# group_levels <- c("grupo2", "grupo1", "grupo3"); control_group <- "grupo3"
group_comparisons <- function(group_levels, control_group) {
  group_sans_control <- group_levels[!(group_levels %in% control_group)]
  purrr::map(group_sans_control, ~{c(control_group, .x)}, control_group)
}

