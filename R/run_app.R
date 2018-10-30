#' Launch qPCRplots Web App
#'
#' Launch qPCRplots Shiny Web Application Inspired and adapted from
#' Dean Attali answer https://stackoverflow.com/a/49623819/8796710
#' @export
launch_qpcrplots <- function() {
  appDir <- base::system.file("qPCRplots-app", package = "qPCRplots")
  if (appDir == "") {
    stop("Could not find qPCRplots-app. Try re-installing `qPCRplots`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
