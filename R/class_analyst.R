
#' List events of given event class.
#' 
#' @export
list_class_events <- function(user_info, cls, start, end) {
  print_time_table(user_info, start, end, "Class", cls, "Event")
}

list_event_progress <- function(user_info, event, start, end) {
  print_time_table(user_info, start, end, "Event", event, "Progress")
}

print_time_table <- function(user_info, start, end, what_to_filter, value_to_filter, what_to_arrange) {
  start_day <- lubridate::ymd(start)
  end_day <- lubridate::ymd(end)
  
  user_info %>%
    query_records(start_day, end_day) %>%
    dplyr::filter(!!as.name(what_to_filter) == value_to_filter) %>%
    dplyr::group_by(!!as.name(what_to_arrange)) %>%
    dplyr::summarise(count = n(), hours = as.numeric(sum(Time), "hours")) %>%
    dplyr::arrange(desc(hours))
}