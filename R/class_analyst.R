
#' List events of given event class.
#' 
#' @export
list_class_events <- function(df, cls) {
  print_time_table(df, "Class", cls, "Event")
}

#' @export
list_event_progress <- function(df, event = NULL) {
  if (is.null(event)) {
    df %>%
      dplyr::group_by(Progress) %>%
      dplyr::summarise(count = n(), hours = as.numeric(sum(Time), "hours")) %>%
      dplyr::arrange(desc(hours))
  } else {
    print_time_table(df, "Event", event, "Progress")
  }
}

#' @export
print_time_table <- function(df, what_to_filter, value_to_filter, what_to_arrange) { 
  df %>%
    dplyr::filter(!!as.name(what_to_filter) == value_to_filter) %>%
    dplyr::group_by(!!as.name(what_to_arrange)) %>%
    dplyr::summarise(count = n(), hours = as.numeric(sum(Time), "hours")) %>%
    dplyr::arrange(desc(hours))
}

#' @export
summary_event_class <- function(df) {
  df %>%
    dplyr::group_by(Class) %>%
    dplyr::summarise(hours = as.numeric(sum(Time), units="hours")) %>%
    dplyr::mutate(Percent = hours/sum(hours)) %>%
    dplyr::arrange(desc(hours))
}