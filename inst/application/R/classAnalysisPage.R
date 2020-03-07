library(shiny)
library(shinydashboard)
library(shinyjqui)
library(wiztimebook)

#' UI of class analysis page
#'
#' @param id Namespace of this page.
#' 
#' @return Same to \code{shiny::fluidRow} function.
classAnalysisPageUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 8, plotOutput(outputId = ns("col_plot"))),
    box(title = "Controls", width = 4, 
        selectInput(inputId = ns("position_adj"),
                    label = "Choose position adjustment:",
                    choices = c("stack", "dodge", "dodge2", "fill", "identity"),
                    selected = "stack"),
        uiOutput(outputId = ns("class_candidates")),
        uiOutput(outputId = ns("tag_candidates"))
    )
  )
}

#' Server logic of histogram page
#'
#' @param input,output,session Arguments passed by \code{shiny::callModule} .
#' @param df A data frame which used to plot histogram.
classAnalysisPage <- function(input, output, session, df) {
  # Render control panel
  output$class_candidates <- renderUI({
    ns <- session$ns
    all_class = unique(df()$Class)
    return(sortableCheckboxGroupInput(
      inputId = ns("event_class"),
      label = "Choose class to show:",
      choices = all_class,
      selected = all_class,
      inline = FALSE
    ))
  })
  output$tag_candidates <- renderUI({
    ns <- session$ns
    all_tags <- df() %>%
      filter(Class %in% input$event_class) %>%
      .$Tag %>% unique()
    names(all_tags) <- ifelse(all_tags == "", "None", all_tags)
    return(checkboxGroupInput(
      inputId = ns("event_tag"),
      label = "Choose tag to show:",
      choices = all_tags,
      selected = all_tags,
      inline = FALSE
    ))
  })
  # Render event class page
  selected_cls <- reactive({
    input$event_class_order[input$event_class_order %in% input$event_class]
  })
  output$col_plot <- renderPlot({
    #TODO: combine sortable and checked event
    #TODO: 数据真正可用前，图形会被渲染多次。这会导致空数据集修改factor时发出警报。
    if (nrow(df()) > 0) {
      df <- df() %>%
        filter(Class %in% selected_cls()) %>%
        filter(Tag %in% input$event_tag) 
      if (nrow(df) > 0) {
        df %>%
          plot_class_time(cls = selected_cls(), position = input$position_adj)
      }
    }
  })
  #TODO: 不要重复
  # observeEvent(input$event_class_order, {
  #   output$col_plot <- renderPlot({
  #     if (nrow(df()) > 0) {
  #       df <- df() %>%
  #         filter(Tag %in% input$event_tag) 
  #       if (nrow(df) > 0) {
  #         df %>%
  #           plot_class_time(cls = input$event_class_order, position = input$position_adj)
  #       }
  #     }
  #   })
  # })
}