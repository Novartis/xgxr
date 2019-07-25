#' Plot data with mean and confidence intervals
#'
#' @inheritParams xgx_stat_ci
#' @return ggplot2 plot layer
#'
#' @examples
#' library(ggplot2)  
#' data = data.frame(x = rep(c(1,2,3),each=20),
#' y = rep(c(1,2,3),each=20) + rnorm(60))
#' ggplot(data,aes(x=x,y=y)) + 
#'  xgx_geom_ci(conf_level = .95)
#'  
#' @export
xgx_geom_ci = function(mapping = NULL, data = NULL, conf_level=.95, distribution = "normal", 
                       geom = list("point","line","errorbar"), 
                       position = "identity", 
                       ..., fun.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  return(list(xgx_stat_ci(conf_level = conf_level, ...)
))
}
