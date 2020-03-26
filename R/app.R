#' Run shiny app
#'
#' @importFrom shiny shinyApp runApp shinyOptions
#' 
#' @export
run_app <- function(user_name = "", data_location = "/My Notes/") {
  opts <- shinyOptions(
    wiz_user_name = user_name,
    wiz_data_location = data_location)
  shinyApp(ui = createMainUI(), server = mainServer, options = opts)
}