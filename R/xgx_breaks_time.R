#' Sets the default breaks for a time axis
#' 
#' \code{xgx_breaks_time} sets the default breaks for a time axis, 
#' given the units of the data and the units of the plot.
#' It is inspired by scales::extended_breaks
#'
#' for the extended breaks function, weights is a set of 4 weights for
#' \enumerate{
#' \item simplicity - how early in the Q order are you
#' \item coverage - labelings that don't extend outside the data:
#' range(data) / range(labels)
#' \item density (previously granularity) - how close to the number of ticks 
#' do you get (default is 5)
#' \item legibility - has to do with fontsize and formatting to prevent 
#' label overlap
#' }
#' 
#' @references Talbot, Justin, Sharon Lin, and Pat Hanrahan.
#' "An extension of Wilkinson’s algorithm for positioning tick labels on axes."
#' IEEE Transactions on visualization and 
#' computer graphics 16.6 (2010): 1036-1043.
#' 
#' @param data_range range of the data
#' @param units_plot units to use in the plot
#' @param number_breaks number of breaks to aim for (default is 5)
#' 
#' @return numeric vector of breaks
#' 
#' @examples
#' xgx_breaks_time(c(0, 5), "hour")
#' xgx_breaks_time(c(0, 6), "hour")
#' xgx_breaks_time(c(-3, 5), "hour")
#' xgx_breaks_time(c(0, 24), "hour")
#' xgx_breaks_time(c(0, 12), "hour")
#' xgx_breaks_time(c(1, 4), "day")
#' xgx_breaks_time(c(1, 12), "day")
#' xgx_breaks_time(c(1, 14), "day")
#' xgx_breaks_time(c(1, 50), "day")
#' xgx_breaks_time(c(1000, 3000), "day")
#' xgx_breaks_time(c(-21, 100), "day")
#' xgx_breaks_time(c(-1, 10), "week")
#' 
#' @importFrom labeling extended
#' @export
xgx_breaks_time <-  function(data_range, units_plot, number_breaks = 5) {
  data_min <- min(data_range)
  data_max <- max(data_range)
  data_span <- data_max - data_min
  number_breaks <- 5 # number of breaks to aim for
  preferred_increment_default <- c(1, 5, 2, 4, 3, 1)
  weights_default <- c(0.25, 0.2, 0.5, 0.05)
  weights_simple <- c(1, 0.2, 0.5, 0.05)

  if (units_plot %in% c("hour", "month") && data_span >= 48) {
    preferred_increment <- c(24, 12, 6, 3)
    weights <- weights_simple
  } else if (units_plot %in% c("hour", "month") && data_span >= 24) {
    preferred_increment <- c(3, 12, 6, 2)
    weights <- weights_simple
  } else if (units_plot %in% c("hour", "month") && data_span < 24) {
    preferred_increment <- c(6, 3, 2, 1)
    weights <- weights_simple
  } else if (units_plot == "day" && data_span >= 720) {
    preferred_increment <- c(360, 180)
    weights <- weights_simple
  } else if (units_plot == "day" && data_span >= 360) {
    preferred_increment <- c(180, 120, 60)
    weights <- weights_simple
  } else if (units_plot == "day" && data_span >= 90) {
    preferred_increment <- c(30, 7)
    weights <- weights_simple
  }
  else if (units_plot == "day" && data_span >= 12) {
    preferred_increment <- c(7, 14, 28)
    weights <- weights_simple
  } else {
    preferred_increment <- preferred_increment_default
    weights <- weights_default
  }

  breaks <- labeling::extended(data_min, data_max, m = number_breaks, 
                               Q = preferred_increment, w = weights)
  return(breaks)
}
