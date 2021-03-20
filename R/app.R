#' Run shiny app
#'
#' @importFrom shiny shinyApp runApp shinyOptions
#' 
#' @export
run_app <- function(user_name = "", data_location = "/My Notes/", start = NULL, end = NULL) {
  opts <- shinyOptions(
    wiz_user_name = user_name,
    wiz_data_location = data_location,
    wiz_start_day = start,
    wiz_end_day = end)
  app <- shinyApp(ui = createMainUI(start, end), server = mainServer, options = opts)
  shiny::runApp(app)
}