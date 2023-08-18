xgx_scale_time_units_ <- function(units_dataset, units_plot = NULL,
                                  breaks = NULL,
                                  labels = NULL, ...) {
  if (is.null(units_plot)) {
    units_plot <- units_dataset
  }

# convert units to lower case and remove s from the end ot have unambiguous name
  units_plot <- units_plot %>%
    tolower() %>%
    str_replace("s$", "")
  units_dataset <- units_dataset %>%
    tolower() %>%
    str_replace("s$", "")

  unit_names = c("second", "minute", "hour", "day", "week", "month", "year")
  if (!(units_dataset %in% unit_names)) {
    stop("units_dataset must be second, minute, hour, day, week, month, or year")
  }
  if (!(units_plot %in% unit_names)) {
    stop("units_dataset must be second, minute, hour, day, week, month, or year")
  }

  day_scale <- data.frame(
    second   = 1 / 24 / 60 / 60,
    minute = 1 / 24 / 60,
    hour = 1 / 24,
    day = 1,
    week = 7,
    month = 30.4375,
    year = 365.25
  )

  input_scale <- day_scale[[units_dataset]]
  output_scale <- day_scale[[units_plot]]
  scale_factor <- output_scale / input_scale

  if (is.null(breaks)) {
    breaks <- function(data_range) {
      xgx_breaks_time(data_range / scale_factor, units_plot) * scale_factor
    }
  }

  if (is.null(labels)) {
    labels <- function(breaks) {
      breaks / scale_factor
    }
  }

  xlabel_list <- data.frame(
    second = "Second",
    minute = "Minute",
    hour = "Hour",
    day = "Day",
    week = "Week",
    month = "Month",
    year = "Year"
  )
  xlabel <- paste0("Time (", xlabel_list[[units_plot]], "s)")
  return(list(breaks = breaks, labels = labels, xlabel = xlabel))
}

#' Convert time units for plotting
#'
#' \code{xgx_scale_x_time_units} converts x axis scale from one time unit
#' to another.
#' Supported units include hours, days, weeks, months, and years, which
#' can also be called using just the first letter (h, d, w, m, y).
#'
#' Note: \code{xgx_scale_x_time_units} only scales the plot axis, all other
#' specifications must be on the original scale of the dataset (e.g. breaks,
#' position, width)
#'
#' @param units_dataset units of the input dataset, must be specified by user
#' as "h", "d", "w", "m", or "y"
#' @param units_plot units of the plot, will be units of the dataset if empty
#' @inheritParams ggplot2::continuous_scale
#' @param ... other parameters for
#' \code{\link[ggplot2:scale_continuous]{scale_x_continuous}}
#'
#' @return ggplot2 compatible scale object
#'
#' @examples
#' data <- data.frame(x = 1:1000, y = rnorm(1000))
#' ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point() +
#'   xgx_scale_x_time_units(units_dataset = "hours", units_plot = "weeks")
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @export
xgx_scale_x_time_units <- function(units_dataset, units_plot = NULL,
                                   breaks = NULL,
                                   labels = NULL, ...) {
  # h = hours, d = days, w = weeks, m = months, y = years
  lst <- xgx_scale_time_units_(units_dataset, units_plot, breaks, labels, ...)
  return(list(
    ggplot2::scale_x_continuous(
      breaks = lst$breaks,
      labels = lst$labels, ...
    ),
    ggplot2::xlab(lst$xlabel)
  ))
}

#' @rdname xgx_scale_x_time_units
#' @export
xgx_scale_y_time_units <-
  function(units_dataset, units_plot = NULL, 
           breaks = NULL, 
           labels = NULL,
           ...) {
    lst <- xgx_scale_time_units_(units_dataset, units_plot, breaks, labels, ...)
    return(list(ggplot2::scale_y_continuous(
      breaks = lst$breaks,
      labels = lst$labels, ...
    ), 
    ggplot2::ylab(lst$xlabel)
    ))
  }