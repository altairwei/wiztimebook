library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)

source("R/classAnalysisPage.R")
source("R/summaryPage.R")
source("R/eventAnalysisPage.R")

dashboardPage(
  dashboardHeader(
    title = "Wiz Time Book"
  ),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput(
        inputId = "query_range",
        label = "Date to query:",
        start = today() - months(1),
        end = today()
      ),
      menuItem("Data Table", tabName = "data_table_page"),
      menuItem("Summary", tabName = "summary_page"),
      menuItem("Class Analysis", tabName = "class_analysis_page"),
      menuItem("Event Analysis", tabName = "event_analysis_page")
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
      )
    )
  )
)
