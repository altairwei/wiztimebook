#' Connect to WizNote public APIs
#' @export
connect_to_wiznote <- function(global_rv) {
  baseUrl <- "ws://localhost:8848"
  message(paste0("Connecting to WebSocket server at ", baseUrl))

  socket <- websocket::WebSocket$new(baseUrl, autoConnect = FALSE)

  socket$onClose(function(event) {
    message("web channel closed")
  })

  socket$onError(function(event) {
    message(paste0("web channel error:", event$message))
  })

  socket$onOpen(function(event) {
    message("WebSocket connected, setting up QWebChannel.")
    rwebchannel::QWebChannel$new(socket, function(channel) {
      message("QWebChannel setup finished.")
      global_rv$WizExplorerApp <- channel$objects[["WizExplorerApp"]]
    })
  })

  socket$connect()
}