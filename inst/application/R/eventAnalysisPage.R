library(shiny)
library(shinydashboard)
library(stringr)

eventAnalysisPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             box(width = NULL, plotOutput(outputId = ns("event_plot"))),
             box(width = NULL, DT::dataTableOutput(outputId = ns("event_table")))
      ),
      column(width = 4,
             box(width = NULL, title = "Controls",
                 textInput(inputId = ns("name_filter"), label = "Search"),
                 uiOutput(outputId = ns("progress_candidates")))
      )

    )
  )
}


eventAnalysisPage <- function(input, output, session, df) {
  
  filtered <- reactive({
    if (input$name_filter != "") {
      df() %>%
        filter(str_detect(Event, input$name_filter))
    } else {
      tibble()
    }
  })
  
  output$progress_candidates <- renderUI({
    df_filtered <- filtered()
    ns <- session$ns
    all_prgs = unique(df_filtered$Progress)
    return(sortableCheckboxGroupInput(
      inputId = ns("event_progress"),
      label = "Choose progress to show:",
      choices = all_prgs,
      selected = all_prgs,
      inline = FALSE
    ))
  })
  
  selected_prgs <- reactive({
    input$event_progress_order[input$event_progress_order %in% input$event_progress]
  })
  
  output$event_plot <- renderPlot({
    df_filtered <- filtered()
    if (nrow(df_filtered) != 0) {
      df_filtered %>%
        filter(Progress %in% selected_prgs()) %>%
        mutate(Progress = fct_relevel(Progress, rev(selected_prgs()))) %>%
        ggplot(aes(x = Progress, y = as.numeric(Time, "hours"))) +
        geom_col() + ylab("Hours") + coord_flip()
    }
  })
  
  output$event_table <- DT::renderDataTable({
      df_filtered <- filtered()
      if (nrow(df_filtered) != 0) {
        df_filtered
      }
    }, width = "100%", options = list(scrollX = TRUE, pageLength = 5)
  )
}