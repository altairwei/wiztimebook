#' Main server logic
#' 
#' @param input,output Arguments passed by \code{shiny::callModule} .
mainServer <- function(input, output, session) {
  user <- user_info(
    shiny::getShinyOption("wiz_user_name"),
    shiny::getShinyOption("wiz_data_location")
  )
  
  start <- shiny::getShinyOption("wiz_start_day")
  end <- shiny::getShinyOption("wiz_end_day")
  if (!is.null(start) && !is.null(end)) {
    start_day <- lubridate::ymd(start)
    end_day <- lubridate::ymd(end)
  } else {
    start_day <- lubridate::today() - months(1)
    end_day <- lubridate::today()
  }

  rv <- shiny::reactiveValues(
    records = user %>%
      query_records(start_day, end_day)
  )
  
  # Update whole datatable when change date range
  shiny::observeEvent(input$query_range, {
    rv$records <- user %>%
      query_records(input$query_range[[1]], input$query_range[[2]])
  })
  
  # Render datatable page
  output$records_data_table <- DT::renderDataTable(
    rv$records, options = list(scrollX = TRUE, pageLength = 10)
  )
  
  df <- shiny::reactive(rv$records)
  shiny::callModule(classAnalysisPage, "class_analysis_page", df)
  shiny::callModule(summaryPage, "summary_page", df)
  shiny::callModule(eventAnalysisPage, "event_analysis_page", df)
  shiny::callModule(efficiencyAnalysisPage, "efficiency_analysis_page", df)
}