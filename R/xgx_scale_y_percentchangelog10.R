#' percentchangelog10 transform for the y scale.  
#' 
#' \code{xgx_scale_y_percentchangelog10} is designed to be used with 
#' percent change (PCHG) from baseline data (on a scale of -1 to +Inf)
#' Common examples include % weight loss, % reduction in LDL, % change in tumor diameter.  
#' It is used when you have a wide range of data on a percent change scale, 
#' especially data close to -100%, and/or several fold increase from baseline.
#' 
#' @param breaks if NULL, then default is to use a variant of 2^(labeling::extended(log2(PCHG + 1))) - 1, where PCHG represents the range of the data
#' @param minor_breaks if NULL, then default is to use nicely spaced log10(PCHG + 1) minor breaks
#' @param labels  if NULL, then the default is to use scales::percent_format()
#' @param accuracy accuracy to use with scales::percent_format(), if NULL, then the default is set to 1
#' @param n_breaks number of desired breaks, if NULL, then the default is set to 7
#' @param ... other parameters passed to 
#' \code{\link[ggplot2:scale_continuous]{scale_y_continuous}}
#' 
#' @return ggplot2 compatible scale object
#' 
#' @examples 
#' dat1 <- data.frame(x = rnorm(100), PCHG = exp(rnorm(100)) - 1)
#' 
#' ggplot2::ggplot(dat1, ggplot2::aes(x = x, y = PCHG)) +
#'   ggplot2::geom_point() +
#'   xgx_theme() +
#'   xgx_scale_y_percentchangelog10()
#'  
#' @importFrom scales trans_new
#' @importFrom scales percent_format
#' @importFrom labeling extended
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom dplyr mutate
#' @export
xgx_scale_y_percentchangelog10 <- function(breaks = NULL,
                                           minor_breaks = NULL,
                                           labels = NULL, 
                                           accuracy = 1, 
                                           n_breaks = 7,
                                           ...) {
  
  if (is.null(breaks)){
    breaks <-  function(data_range) {
      r <- range(log2(data_range + 1))
      breaks <- 2^(labeling::extended(r[1], r[2], m = n_breaks, Q = c(1,2,4,8))) - 1
      return(breaks)
    }
    
  }
  
  if (is.null(minor_breaks)) {
    minor_breaks <-  function(x) xgx_minor_breaks_log10(x + 1) - 1
  }
  
  percentchangelog <- scales::trans_new(
    name      = "percentchangelog",
    transform = function(x) log10(x + 1),
    inverse = function(x) 10^(x) - 1)
  
  if (is.null(labels)) {
    labels = scales::percent_format(accuracy = accuracy)
  }
  
  ggplot2::scale_y_continuous(trans = percentchangelog,
                              labels = labels,
                              minor_breaks = minor_breaks,
                              breaks = breaks, ...)
}
