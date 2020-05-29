#  Function for computing confidence intervals
#'
#' \code{conf_int} returns a dataframe with mean +/- confidence intervals
#'
#' @param conf_level The percentile for the confidence interval (should fall 
#' between 0 and 1). The default is 0.95, which corresponds to a 95 percent 
#' confidence interval.
#' @param distribution The distribution which the data follow, used for 
#' calculating confidence intervals. The options are "normal", "lognormal", 
#' and "binomial". The "normal" option will use the Student t Distribution 
#' to calculate confidence intervals, the "lognormal" option will transform 
#' data to the log space first. The "binomial" option will use the
#' \code{\link[binom:binom.confint]{binom.exact}} function to calculate the
#' confidence 
#' intervals. Note: binomial data must be numeric and contain only 1's and 0's.
#'
#' @return data.frame
#'
#' @examples
#' # default settings for normally distributed data, 95% confidence interval,  
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60),
#'                    group = rep(1:3, 20))
#' conf_int(data$y)
#'   
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats na.omit
#' @importFrom stats qt
#' @importFrom stats var
#' @importFrom binom binom.exact
#' @export
conf_int = function(y, conf_level, distribution) {
  
  if (!(conf_level > 0.5 && conf_level < 1)) {
    stop("conf_level should be greater than 0.5 and less than 1")
  }
  
  percentile_value <- conf_level + (1 - conf_level) / 2
  
  y <- stats::na.omit(y)
  
  if (distribution == "normal") {
    conf_int_out <- data.frame(
      y = mean(y),
      ymin = mean(y) - stats::qt(percentile_value,
                                 length(y)) * sqrt(stats::var(y) / length(y)),
      ymax = mean(y) + stats::qt(percentile_value,
                                 length(y)) * sqrt(stats::var(y) / length(y))
    )
  } else if (distribution == "lognormal") {
    yy <- log(y)
    conf_int_out <- data.frame(
      y = exp(mean(yy)),
      ymin = exp(mean(yy) - stats::qt(percentile_value, length(yy)) * sqrt(stats::var(yy) / length(yy))),
      ymax = exp(mean(yy) + stats::qt(percentile_value, length(yy)) * sqrt(stats::var(yy) / length(yy)))
    )
  } else if (distribution == "binomial") {
    conf_int_out <- data.frame(
      y = mean(y),
      ymin = binom::binom.exact(sum(y), length(y),
                                conf.level = percentile_value)$lower,
      ymax = binom::binom.exact(sum(y), length(y),
                                conf.level = percentile_value)$upper)
  } else {
    stop("distribution must be either normal, lognormal, or binomial")
  }
  return(conf_int_out)
}