#' Create a column plot based on the time cost of each event class. 
#' 
#' @param df A tidy tibble
#' @param cls String vector contains which classes you want to plot.
#' @param time_units Unit of time cost, such as 'hours', 'minutes' and so on.
#' @param ... Arguments will be passed to \code{ggplot2::geom_col} .
#' @return A ggplot object
#' 
#' @importFrom ggplot2 ggplot aes geom_col ylab
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @importFrom lubridate as.duration
#' @importFrom forcats fct_relevel
#' 
#' @export
plot_class_time <- function(df, cls = NULL, time_units = "hours", ...) {
  if (is.null(cls)) {
    cls = unique(df$Class)
  }
  df %>% mutate(Time = as.duration(Time),) %>%
    filter(Class %in% cls) %>%
    mutate(Class = fct_relevel(Class, cls)) %>%
    ggplot(aes(x = Date, y = as.numeric(Time, time_units), fill=Class)) +
    geom_col(...) + ylab(time_units)
}

#' Visualize event duration line during one day.
#'
#' @export
plot_class_duration <- function(df) {
  df %>%
    dplyr::mutate(Date = as.POSIXct(Date)) %>%
    ggplot2::ggplot(aes(Date, color=Class)) +
    ggplot2::scale_x_datetime() +
    ggplot2::scale_y_continuous(
      limits = c(6,24), breaks=seq(6, 24, 1),
      labels=stringr::str_pad(seq(6, 24, 1) %% 24, 2, pad="0")) + 
    ggplot2::geom_linerange(
      aes(ymin = Start - Date, ymax = End - Date), size = 3) +
    ggplot2::coord_flip() + ylab("Time (hours)") + 
    ggplot2::ggtitle("Activity During Day")
}

#' Visualize histogram of event during one day.
#'
#' @export
plot_time_hist <- function(df) {
  # TODO: 没有考虑短时间的大量事件记录
  df %>%
    dplyr::mutate(
      Start = as.POSIXct(round(Start, "hours")),
      End = as.POSIXct(round(End, "hours"))) %>%
    purrr::pmap_dfr(function(Class, Tag, Event, Start, End,
                             Time, Progress, Date, GUID) {
      seq(Start, End, "hour") %>% purrr::map_dfr(function(moment) {
        Moment = hms::as_hms(moment)
        tibble::tibble(Class, Tag, Event, Date, Moment, Progress)
      })
    }) %>%
    ggplot2::ggplot(aes(Moment, fill = Class)) +
      ggplot2::geom_histogram(position = "stack", bins = 24) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
}

#' @export
plot_treemap <- function(df) {
    df %>%
      dplyr::group_by(Event) %>%
      dplyr::summarise(Time = sum(as.numeric(Time, "hours")), 
                Class = dplyr::first(Class)) %>%
      ggplot2::ggplot(ggplot2::aes(area = as.numeric(Time, "hours"), 
                 fill = Event, subgroup = Class, label = Event)) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_subgroup_border() +
      treemapify::geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
      treemapify::geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "black", min.size = 0) +
      ggplot2::theme(legend.position = "none")
}