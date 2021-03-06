#' Create Application whole UI.
#'
#' @importFrom lubridate today
#' @importFrom shiny dateRangeInput
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' dashboardBody sidebarMenu menuItem tabItems tabItem
createMainUI <- function(start_day, end_day) {
  dashboardPage(
    dashboardHeader(
      title = "Wiz Time Book"
    ),
    dashboardSidebar(
      sidebarMenu(
        dateRangeInput(
          inputId = "query_range",
          label = "Date to query:",
          start = lubridate::ymd(start_day),
          end = lubridate::ymd(end_day)
        ),
        menuItem("Data Table", tabName = "data_table_page"),
        menuItem("Summary", tabName = "summary_page"),
        menuItem("Class Analysis", tabName = "class_analysis_page"),
        menuItem("Event Analysis", tabName = "event_analysis_page"),
        menuItem("Efficiency Analysis", tabName = "efficiency_analysis_page")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "data_table_page",
          DT::dataTableOutput(outputId = "records_data_table")
        ),
        tabItem(
          tabName = "summary_page",
          summaryPageUI("summary_page")
        ),
        tabItem(
          tabName = "class_analysis_page",
          classAnalysisPageUI("class_analysis_page")
        ),
        tabItem(
          tabName = "event_analysis_page",
          eventAnalysisPageUI("event_analysis_page")
        ),
        tabItem(
          tabName = "efficiency_analysis_page",
          efficiencyAnalysisPageUI("efficiency_analysis_page")
        )
      )
    )
  )
}

