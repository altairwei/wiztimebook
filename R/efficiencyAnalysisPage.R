efficiencyAnalysisPageUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      box(width = 6, plotOutput(outputId = ns("activity_plot"))),
      box(width = 6, plotOutput(outputId = ns("duration_hist")))
      )
    )
}

efficiencyAnalysisPage <- function(input, output, session, df) {
  output$activity_plot <- shiny::renderPlot({
    df() %>% plot_class_duration()
  })

  output$duration_hist <- shiny::renderPlot({
    df() %>% plot_time_hist()
  })
}