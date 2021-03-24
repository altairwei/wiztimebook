library(shiny)
library(websocket)
library(wiztimebook)

ui <- fluidPage(
  textInput("location", "Location"),
  actionButton("query", "Query"),
  textOutput("display")
)

server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    WizExplorerApp = NULL,
    records = NULL
  )

  connect_to_wiznote(rv)

  observeEvent(input$query, {
    req(rv$WizExplorerApp)

    obj_app <- rv$WizExplorerApp
    obj_db <- obj_app$Database
    sql <- sprintf("DOCUMENT_LOCATION like '%s%%'", input$location)
    message(sql)
    sql <- iconv(sql, to="utf-8")
    message(sql)
    obj_db$DocumentsFromSQLWhere(sql, function(doc_list) {
      message("SQL executed.")
      fullfill <- logical(0)
      documents <- list()
      for (doc in doc_list) {
        message("~~~~")
        obj_app$DatabaseManager$CheckDocumentData(doc, function(ret) {
          message("Document data checked.")
          fullfill <<- c(fullfill, ret)
          if (length(fullfill) == length(doc_list)) {
            if (all(fullfill)) {
              rv$records <- doc_list %>% .$Title
              browser()
            } else {
              stop("Document download failed.")
            }
          }
        })
      }
    })
  })

  output$display <- renderText({
    req(rv$records)
    rv$records
  })
}

app <- shinyApp(ui, server)

runApp(app)
