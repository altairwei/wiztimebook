#' Organize records table
#' 
#' `tidy_records` is very verbose.
#' 
#' @param records A tibble.
#' @param date Any input for `lubridate::ymd` function.
#' 
#' @return A tidy tibble.
#' 
#' @importFrom dplyr filter mutate
#' @importFrom tidyr separate_rows separate
#' @importFrom lubridate ymd ymd_hm
#' @importFrom purrr pwalk
tidy_records <- function(records) {
  # Remove empty records
  records <-  records %>% filter(is_valid(Record))
  # Seperate records to each rows
  records <- records %>% separate_rows(Record, sep = ";\\s*") %>%
    separate(Record, into = c("Start", "End"), sep = ",\\s*")
  # Notify user to check the tabble again
  records <- records %>%
    pwalk(check_record_startend) %>% filter(is_valid(Start) & is_valid(End))
  # Compute time
  records <- records %>% mutate(
    Start = ymd_hm(paste(Date, Start)),
    End = ymd_hm(paste(Date, End))) %>%
    mutate(Time = End - Start) %>%
    pwalk(check_record_startend) %>%
    filter(!is.na(Start) & !is.na(End))
  # Check time
  records %>% pwalk(check_record_time)
}

is_valid <- function(x) {
  !is.na(x) & !is.null(x) & x != ""
}

check_record_startend <- function(Class, Tag, Event, Start, End, Time, Progress, Date, GUID) {
  if (!is_valid(as.character(Start)) | !is_valid(as.character(End))) {
    cat(sprintf(
      "The record is not valid, and will be removed: \n\t%s\t%s\t%s\t%s\t%s\n",
      Date, Class, Event, Start, End))
  }
}

#' Check time field
#' 
#' @importFrom readr parse_number
check_record_time <- function(Class, Tag, Event, Start, End, Time, Progress, Date, GUID) {
  if (is_valid(as.character(Time)) & (parse_number(as.character(Time)) < 0) ) {
    cat(sprintf(
      "The time of record may be wrong: \n\t%s\t%s\t%s\t%s\t%s\t%s\n",
      Date, Class, Event, Start, End, Time))
  }
}