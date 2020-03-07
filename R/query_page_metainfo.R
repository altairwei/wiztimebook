#' Find document meta information from WizNote database
#' 
#' `query_page_metainfo` will extract document meta information from WizNote
#' database with the given start to end date.
#' 
#' @param user_info An user information S3 object.
#' @param start,end Any input for `lubridate::ymd` function.
#' 
#' @return A tibble that contains meta informatin of WizNote document
#' 
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom lubridate ymd
#' @importFrom tidyr as_tibble
query_page_metainfo <- function(user_info, start, end) {
  start_date <- ymd(start)
  end_date <- ymd(end)
  
  # Connect to index.db
  con <- dbConnect(SQLite(), user_info$index_db)
  
  # Fetch document meta information
  res <- dbSendQuery(con, sprintf(
    "select DOCUMENT_GUID,DOCUMENT_TITLE,DOCUMENT_LOCATION from WIZ_DOCUMENT
      where DOCUMENT_LOCATION like '%s%%'
      and DOCUMENT_TITLE like '____-__-__'
      and DOCUMENT_TITLE >= '%s' and DOCUMENT_TITLE <= '%s'", 
    user_info$data_location, start_date, end_date))
  x <- dbFetch(res) %>% as_tibble()
  
  # Close connection
  dbClearResult(res)
  dbDisconnect(con)
  
  x
}