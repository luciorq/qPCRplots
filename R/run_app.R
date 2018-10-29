# From Dean Attali answer https://stackoverflow.com/a/49623819/8796710
#' @export
launch_qpcrplots <- function() {
  appDir <- system.file("qPCRplots", package = "qPCRplots")
  if (appDir == "") {
    stop("Could not find qPCRplots-app. Try re-installing `qPCRplots`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
