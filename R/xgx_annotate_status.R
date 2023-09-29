#' Create a status (e.g. DRAFT) annotation layer
#' 
#' \code{xgx_annotate_status} adds a status (e.g. DRAFT) annotation layer 
#' to a plot. 
#' The text of the annotation can be customized, the default is "DRAFT". 
#' The color, location, size, fontface, transparency of the annotation can 
#' also be customized.
#' 
#' The default value of x = Inf works in many instances, but not some special
#' data types like Dates.  In that case, you will need to specify x
#'
#' @param status the text to 
#' @param x x location, default Inf (right most point)
#' @param y y location, default Inf (up most point)
#' @param color font color, default "grey"
#' @param hjust horizontal justification, default 1.2
#' @param vjust vertical justification, default 1.2
#' @param fontsize font size to use, default 7
#' @param fontface font style to use, default "bold"
#' @param alpha transparency, default is 0.5
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}
#'
#' @return ggplot layer
#'
#' @examples
#' data <- data.frame(x = 1:1000, y = rnorm(1000))
#' ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) + 
#'   ggplot2::geom_point() +
#'   xgx_annotate_status("DRAFT")
#'   
#' # for dates
#'   data <- data.frame(x = as.Date(c("2015-01-01", "2016-01-01")), y = c(1,2))
#'   xmax <- max(na.omit(dummy_data$x))
#'   ggplot2::ggplot(data, aes(x = x, y = y)) + geom_point() + xgx_annotate_status("DRAFT", x = xmax)
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 annotate
#' @export
xgx_annotate_status <- function(status = "DRAFT",
                                x = Inf, y = Inf, color = "grey",
                                hjust = 1.2, vjust = 1.2,
                                fontsize = 7, fontface = "bold",
                                alpha = 0.5, ...) {
  ggplot2::annotate("text", x = x, y = y,
                    label = status, color = color,
                    hjust = hjust, vjust = vjust,
                    cex = fontsize, fontface = fontface,
                    alpha = alpha, ...)
}
