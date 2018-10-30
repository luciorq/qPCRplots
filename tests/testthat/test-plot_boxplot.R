testthat::context("test-plot_boxplot")

testthat::test_that("test if plot_boxplot() runs.", {
  set.seed(42)
  library(qPCRplots)
  df_1 <- readxl::read_excel(
    "data/example-two_groups.xlsx"
  )
  # df_2 <- readxl::read_excel("tests/testthat/data/example-three_groups.xlsx")
  df_2 <- readxl::read_excel(
    "data/example-three_groups.xlsx"
  )

  df_3 <- ggplot2::diamonds %>%
    dplyr::slice(1:300)  %>%
    dplyr::select(cut, carat, price)


  boxplot_1 <- qPCRplots::plot_boxplot(df_1)
  testthat::expect_equal(
    boxplot_1$mapping,
    structure(list(x = ~group, y = ~values_boxplot), class = "uneval")
  )
  boxplot_2 <- qPCRplots::plot_boxplot(df_2)

  testthat::expect_equal(length(boxplot_2$layers), 6)

  boxplot_3 <- qPCRplots::plot_boxplot(NULL)
  testthat::expect_equal(length(boxplot_3$layers), 0)

  boxplot_4 <- qPCRplots::plot_boxplot(df_2, log_transform = TRUE)

  #library(dplyr)
  #cutoff
  #df_2 %>%
  #  rename(values = `Log10 DNV1 Midgut`) %>%
  #  dplyr::mutate(values = dplyr::if_else(values <= cutoff, cutoff, values))
#
  df_3
  boxplot_5 <- qPCRplots::plot_boxplot(df_3, log_transform = TRUE, cutoff = 3 )

  boxplot_6 <- plot_boxplot(df_3, log_transform = TRUE, cutoff = 3 )
})
