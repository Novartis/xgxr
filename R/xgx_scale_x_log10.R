#' log10 scales the x axis with a "pretty" set of breaks
#'
#' \code{xgx_scale_x_log10} is similar to
#' \code{\link[ggplot2:scale_continuous]{scale_x_log10}}.
#' But it uses what we believe to be a nicer spacing and set of tick marks
#' it can be used the same as
#' \code{\link[ggplot2:scale_continuous]{scale_x_log10}}
#'
#' @param breaks major breaks, default is a function defined here
#' @param minor_breaks minor breaks, default is a function defined here
#' @param labels function for setting the labels, defined here
#' @param ... other arguments passed to
#' \code{\link[ggplot2:scale_continuous]{scale_x_log10}}
#'
#' @return ggplot2 compatible scale object
#'
#' @examples
#' conc <- 10^(seq(-3, 3, by = 0.1))
#' ec50 <- 1
#' data <- data.frame(concentration  = conc,
#'                    bound_receptor = 1 * conc / (conc + ec50))
#' ggplot2::ggplot(data, ggplot2::aes(x = concentration, y = bound_receptor)) +
#' ggplot2::geom_point() +
#'   ggplot2::geom_line() +
#'   xgx_scale_x_log10() +
#'   xgx_scale_y_reverselog10()
#'
#' @importFrom ggplot2 scale_x_log10
#' @export
xgx_scale_x_log10 <-  function(breaks = xgx_breaks_log10,
                               minor_breaks = NULL,
                               labels = xgx_labels_log10,
                               ...) {
  if (is.null(minor_breaks)) {
    minor_breaks <- function(x) xgx_minor_breaks_log10(x)
  }
  if (requireNamespace("ggforce", quietly = TRUE)) {
    ret <- try(ggforce::scale_x_unit(..., breaks = breaks,
                                     minor_breaks = minor_breaks,
                                     labels = labels,
                                     trans = xgx_log10_trans_unit()),
               silent=TRUE)
  } else {
    ret <- structure(0, class="try-error")
  }
  if (inherits(ret, "try-error")) {
    ret <- try(list(ggplot2::scale_x_log10(..., breaks = breaks,
                                           minor_breaks = minor_breaks,
                                           labels = labels)),
               silent=TRUE)
  }
  if  (inherits(ret, "try-error")) {
    return(ggplot2::scale_x_log10(...))
  } else {
    return(ret)
  }

}
