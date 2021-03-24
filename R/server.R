#' Main server logic
#' 
#' @param input,output Arguments passed by \code{shiny::callModule} .
mainServer <- function(input, output, session) {
  ###################################
  # Get application setup information
  ###################################

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

  ###################################
  # Begin application logic
  ###################################

  global_rv <- shiny::reactiveValues(
    WizExplorerApp = NULL,
    records = NULL
  )

  connect_to_wiznote(global_rv)
  
  # Update whole datatable when change date range
  shiny::observe({
    shiny::req(global_rv$WizExplorerApp)

    #global_rv$records <- user %>%
    #  query_records(input$query_range[[1]], input$query_range[[2]])

    #TODO: 提示用户选择数据文件夹，然后保存到用户数据库中。
    message("Retrieving time book records...")
    retrieve_records(
      global_rv$WizExplorerApp,
      user$data_location,
      input$query_range[[1]],
      input$query_range[[2]],
      function(data) {
        global_rv$records <- data
        message("Records retrieved.")
      }
    )
  })
  
  # Render datatable page
  output$records_data_table <- DT::renderDataTable(
    global_rv$records, options = list(scrollX = TRUE, pageLength = 10)
  )
  
  df <- shiny::reactive(global_rv$records)
  shiny::callModule(classAnalysisPage, "class_analysis_page", df)
  shiny::callModule(summaryPage, "summary_page", df)
  shiny::callModule(eventAnalysisPage, "event_analysis_page", df)
  shiny::callModule(efficiencyAnalysisPage, "efficiency_analysis_page", df)
}