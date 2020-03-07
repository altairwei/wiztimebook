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