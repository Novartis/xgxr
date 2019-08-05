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
    minor_breaks <- function(x) {
      r1 <- range(log10(x))
      r <-  r1
      r[1] <-  floor(r[1])
      r[2] <-  ceiling(r[2]) + 1
      breaks <- c()
      for (i in seq(r[1], r[2])) {
        breaks <-  c(breaks, seq(2 * 10^(i - 1), 10^i - 10^(i - 1),
                                 by = 10^(i - 1)))
      }
      breaks <-  breaks[breaks <= 10^r1[2]]
      breaks <-  breaks[breaks >= 10^r1[1]]
      return(breaks)
    }
  }

  ret <- try(list(ggplot2::scale_x_log10(..., breaks = breaks,
                                         minor_breaks = minor_breaks,
                                         labels = labels)),
             silent = TRUE)
  if  (inherits(ret, "try-error")) {
    return(ggplot2::scale_x_log10(...))
  } else {
    return(ret)
  }
}
