#' Sets the default breaks for a time axis
#' 
#' \code{xgx_breaks_time} sets the default breaks for a time axis, 
#' given the units of the data and the units of the plot.
#' It is inspired by scales::extended_breaks
#'
#' for the extended breaks function, 
#' Q is a set of nice increments
#' w is a set of 4 weights for
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
#'
#' @return numeric vector of breaks
#' 
#' @examples
#' xgx_breaks_time(c(0, 5), "h")
#' xgx_breaks_time(c(0, 6), "h")
#' xgx_breaks_time(c(-3, 5), "h")
#' xgx_breaks_time(c(0, 24), "h")
#' xgx_breaks_time(c(0, 12), "h")
#' xgx_breaks_time(c(1, 4), "d")
#' xgx_breaks_time(c(1, 12), "d")
#' xgx_breaks_time(c(1, 14), "d")
#' xgx_breaks_time(c(1, 50), "d")
#' xgx_breaks_time(c(1000, 3000), "d")
#' xgx_breaks_time(c(-21, 100), "d")
#' xgx_breaks_time(c(-1, 10), "w")
#' 
#' @importFrom labeling extended
#' @export
xgx_breaks_time <-  function(data_range, units_plot) {
  dmin <- min(data_range)
  dmax <- max(data_range)
  dspan <- dmax - dmin
  # number of breaks to aim for
  m <- 5
  # default Q (spacing)
  q_default <- c(1, 5, 2, 4, 3, 1)
  w_default <- c(0.25, 0.2, 0.5, 0.05)
  w_simple <- c(1, 0.2, 0.5, 0.05)

  if (units_plot %in% c("h", "m") && dspan >= 48) {
    q <- c(24, 12, 6, 3)
    w <- w_simple
  } else if (units_plot %in% c("h", "m") && dspan >= 24) {
    q <- c(3, 12, 6, 2)
    w <- w_simple
  } else if (units_plot %in% c("h", "m") && dspan < 24) {
    q <- c(6, 3, 2, 1)
    w <- w_simple
  } else if (units_plot == "d" && dspan >= 12) {
    q <- c(7, 14, 28)
    w <- w_simple
  } else {
    q <- q_default
    w <- w_default
  }

  breaks <- labeling::extended(dmin, dmax, m, Q = q, w = w)
  return(breaks)
}
