#' Run shiny app
#'
#' @importFrom shiny runApp shinyOptions
#' 
#' @export
run_app <- function(user_name = "", data_location = "/My Notes/") {
  shinyOptions(
    wiz_user_name = user_name,
    wiz_data_location = data_location)
  appDir <- system.file("application", package = "wiztimebook")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `wiztimebook`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}