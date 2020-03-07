#' Extract records from given page document
#' 
#' `query_records` will produce tidy data from WizNote database.
#' 
#' @param user_info An user information S3 object.
#' @param date Any input for `lubridate::ymd` function. One page of time ledge
#'  per day.
#' 
#' @return A tidy tibble that contains event time records of your each day.
#' 
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom purrr map pmap reduce
#' @importFrom dplyr mutate bind_rows
#' 
#' @export
query_records <- function(user_info, start, end) {
  # Format inputs
  start_day <- ymd(start)
  end_day <- ymd(end)
  # Query meta information
  data_info_table <- query_page_metainfo(user_info, start_day, end_day)
  #TODO: Parse table from each document
  data_info_table %>%
    pmap(function(DOCUMENT_GUID, DOCUMENT_TITLE, DOCUMENT_LOCATION) {
      read_table_from_doc(user_info, DOCUMENT_GUID) %>%
        mutate(Date = ymd(DOCUMENT_TITLE), GUID = DOCUMENT_GUID)
    }) %>% map(tidy_records) %>% reduce(bind_rows)
}