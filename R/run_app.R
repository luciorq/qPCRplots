# From Dean Attali answer https://stackoverflow.com/a/49623819/8796710
#' @export
launch_qpcrplots <- function() {
  appDir <- system.file("qPCRplots-app", package = "qPCRplots")
  if (appDir == "") {
    stop("Could not find qPCRplots-app. Try re-installing `qPCRplots`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


# For custom shinyserver
# dir <- system.file("shiny-examples", "qPCRplots", package = "qPCRplots")
# setwd(dir)
# shiny::runApp(".")
