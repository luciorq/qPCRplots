testthat::context("test-plot_boxplot")

testthat::test_that("test if plot_boxplot() runs.", {
  set.seed(42)
  library(qPCRplots)
  df_1 <- readxl::read_excel(
    "data/example-two_groups.xlsx"
  )
  df_2 <- readxl::read_excel(
    "data/example-three_groups.xlsx"
  )

  boxplot_1 <- qPCRplots::plot_boxplot(df_1)
  testthat::expect_equal(
    boxplot_1$mapping,
    structure(list(x = ~group, y = ~values_boxplot), class = "uneval")
  )
  boxplot_2 <- qPCRplots::plot_boxplot(df_2)

  testthat::expect_equal(length(boxplot_2$layers), 6)
})
