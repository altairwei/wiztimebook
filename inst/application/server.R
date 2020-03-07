library(shiny)
library(magrittr)
library(wiztimebook)
library(lubridate)
library(plotly)

function(input, output) {
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
  
  callModule(classAnalysisPage, "class_analysis_page", reactive({rv$records}))
}
