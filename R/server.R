#' Main server logic
#' 
#' @param input,output Arguments passed by \code{shiny::callModule} .
#' 
#' @importFrom lubridate today
#' @importFrom shiny reactive reactiveValues observeEvent getShinyOption
mainServer <- function(input, output, session) {
  user <- user_info(
    getShinyOption("wiz_user_name"),
    getShinyOption("wiz_data_location")
  )
  
  rv <- reactiveValues(
    records = user %>%
      query_records(today() - months(1), today())
  )
  
  # Update whole datatable when change date range
  observeEvent(input$query_range, {
    rv$records <- user %>%
      query_records(input$query_range[[1]], input$query_range[[2]])
  })
  
  # Render datatable page
  output$records_data_table <- DT::renderDataTable(
    rv$records, options = list(scrollX = TRUE, pageLength = 10)
  )
  
  df <- reactive({rv$records})
  callModule(classAnalysisPage, "class_analysis_page", df)
  callModule(summaryPage, "summary_page", df)
  callModule(eventAnalysisPage, "event_analysis_page", df)
}