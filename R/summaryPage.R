#' Create UI of summary page
#'
#' @param id Namespace for the page.
#' 
#' @importFrom shiny NS fluidRow plotOutput
#' @importFrom shinydashboard box
summaryPageUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 6, plotOutput(outputId = ns("treemap_plot")))
  )
}


#' Server logic of histogram page
#'
#' @param input,output,session Arguments passed by \code{shiny::callModule} .
#' @param df A data frame which used to plot histogram.
#' 
#' @importFrom dplyr group_by summarise first
#' @importFrom ggplot2 ggplot aes theme
#' @importFrom treemapify geom_treemap geom_treemap_subgroup_border
#' geom_treemap_text geom_treemap_subgroup_text
#' @importFrom shiny renderPlot
summaryPage <- function(input, output, session, df) {
  output$treemap_plot <- renderPlot({
    df() %>% plot_treemap()
  })
}


