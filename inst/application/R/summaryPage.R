library(shiny)
library(ggplot2)
library(treemapify)
library(dplyr)


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
summaryPage <- function(input, output, session, df) {
  output$treemap_plot <- renderPlot({
    df() %>%
      group_by(Event) %>%
      summarise(Time = sum(as.numeric(Time, "hours")), 
                Class = first(Class)) %>%
      ggplot(aes(area = as.numeric(Time, "hours"), 
                 fill = Event, subgroup = Class, label = Event)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "black", min.size = 0) +
      theme(legend.position = "none")
  })
}


