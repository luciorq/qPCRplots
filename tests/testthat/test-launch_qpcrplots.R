testthat::context("test-launch_qpcrplots")

testthat::test_that("Shiny App launch succesfull.", {

  a <- qPCRplots::app_server(NULL,NULL,NULL)
  testthat::expect_equal(class(a), c("shiny.render.function", "function"))

  b <- qPCRplots::app_ui()
  testthat::expect_equal(class(b), "shiny.tag")

  c <- qPCRplots::launch_qpcrplots()
  testthat::expect_equal(class(c), "shiny.appobj")

})
