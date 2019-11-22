#' Reverselog transform for the y scale.  
#' 
#' \code{xgx_scale_y_reverselog10} is designed to be used with data 
#' that approaches 100%.
#' A common example is receptor occupancy in drug development.  
#' It is used when you want even spacing between 90, 99, 99.9, etc.
#' 
#' @param labels.  if NULL, then the default is to use scales::percent()
#' 
#' @param accuracy.  if NULL, then use the the default as specified by scales::percent()
#' to round to the hundredths place, set accuracy 0.01
#' 
#' @param ... other parameters passed to 
#' \code{\link[ggplot2:scale_continuous]{scale_y_continuous}}
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
#' @importFrom scales trans_new
#' @importFrom scales percent_format
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_y_continuous
#' @export
xgx_scale_y_reverselog10 <- function(labels = NULL, accuracy = NULL, ...) {
  reverselog <- scales::trans_new(
    name      = "reverselog",
    transform = function(x) -log10(1 - x),
    inverse   = function(x) 1 - 10^-x,
    breaks    = function(x) c(0, c(100 - 10^(-100:1))) / 100)

  if (is.null(labels)) {
    labels = scales::percent_format(accuracy = accuracy)
  }
  
  ggplot2::scale_y_continuous(trans = reverselog,
                              labels = labels, ...)
}
