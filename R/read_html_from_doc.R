#' Get html text from WizNotePlus document zipfile
#' 
#' @param filename WizNote document zipfile name
#'
#' @return string. The html text of document zipfile.
#' 
#' @importFrom readr read_file
read_html_from_doc <- function(filename) {
  conn <- unz(filename, "index.html")
  read_file(conn)
}