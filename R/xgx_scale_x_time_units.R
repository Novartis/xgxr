#' Convert time units for plotting
#' 
#' \code{xgx_scale_x_time_units} converts x axis scale from one time unit 
#' to another.  
#' Supported units include hours, days, weeks, months, and years, which 
#' can also be called using just the first letter (h, d, w, m, y).
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
#'   
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

  if (is.null(units_plot)) {
    units_plot <- units_dataset
  }

  # allows for user to write out longer string for units
  units_plot <- units_plot %>% tolower() %>% substr(1, 1)
  units_dataset <- units_dataset  %>% tolower() %>% substr(1, 1)

  if (!(units_dataset %in% c("h", "d", "w", "m", "y"))) {
    stop("units_dataset must be hours, days, weeks, months, or years")
  }
  if (!(units_plot %in% c("h", "d", "w", "m", "y"))) {
    stop("units_plot must be hours, days, weeks, months, or years")
  }

  day_scale <- data.frame(h = 1 / 24,
                          d = 1,
                          w = 7,
                          m = 30.4375,
                          y = 365.25)

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

  xlabel_list <- data.frame(h = "Hour",
                            d = "Day",
                            w = "Week",
                            m = "Month",
                            y = "Year")
  xlabel <- paste0("Time (", xlabel_list[[units_plot]], "s)")

  return(list(
    ggplot2::scale_x_continuous(breaks = breaks,
                                labels = labels, ...),
    ggplot2::xlab(xlabel)))
}
