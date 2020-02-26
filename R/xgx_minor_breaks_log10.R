#' Sets the default minor_breaks for log10 scales
#'
#' \code{xgx_minor_breaks_log10} sets nice minor_breaks for log10 scale.
#'
#'
#' @param data_range range of the data
#'
#' @return numeric vector of breaks
#'
#' @examples
#' xgx_minor_breaks_log10(c(1, 1000))
#' xgx_minor_breaks_log10(c(0.001, 100))
#' xgx_minor_breaks_log10(c(1e-4, 1e4))
#' xgx_minor_breaks_log10(c(1e-9, 1e9))
#' xgx_minor_breaks_log10(c(1, 2))
#' xgx_minor_breaks_log10(c(1, 5))
#' xgx_minor_breaks_log10(c(1, 10))
#' xgx_minor_breaks_log10(c(1, 100))
#' xgx_minor_breaks_log10(c(1, 1.01))
#' xgx_minor_breaks_log10(c(1, 1.0001))
#' print(xgx_minor_breaks_log10(c(1, 1.000001)), digits = 10)
#'
#' @importFrom labeling extended
#' @export
xgx_minor_breaks_log10 <-  function(data_range) {
  units <- NULL
  class_units <- NULL
  if (inherits(data_range, "units")) {
    units <- attr(data_range, "units")
    attr(data_range, "units") <- NULL
    class_units <- class(data_range)
    class(data_range) <- NULL
  }
  r1 <- range(log10(data_range))
  r <-  r1
  r[1] <-  floor(r[1])
  r[2] <-  ceiling(r[2]) + 1
  minor_breaks <- c()
  for (i in seq(r[1], r[2])) {
    minor_breaks <-  c(minor_breaks, seq(2 * 10^(i - 1), 10^i - 10^(i - 1),
                             by = 10^(i - 1)))
  }
  minor_breaks <-  minor_breaks[minor_breaks <= 10^r1[2]]
  minor_breaks <-  minor_breaks[minor_breaks >= 10^r1[1]]
  if (!is.null(units)) {
    attr(minor_breaks, "units") <- units
    class(minor_breaks) <- class_units
  }
  return(minor_breaks)
}
