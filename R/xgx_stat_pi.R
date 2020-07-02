#' Plot data with median and percent intervals
#'
#' \code{xgx_stat_pi} returns a ggplot layer plotting median +/- percent 
#' intervals
#' 
#'
#' @param mapping Set of aesthetic mappings created by `aes` or `aes_`. 
#' If specified and `inherit.aes = TRUE` (the default), it is combined with the 
#' default mapping at the top level of the plot. You must supply mapping if 
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' 
#' If NULL, the default, the data is inherited from the plot data as specified 
#' in the call to ggplot.
#' 
#' A data.frame, or other object, will override the plot data. All objects 
#' will be fortified to produce a data frame. See fortify for which variables 
#' will be created.
#' 
#' A function will be called with a single argument, the plot data. The return 
#' value must be a data.frame., and will be used as the layer data.
#' @param percent_level The upper or lower percentile for the percent interval (should fall 
#' between 0 and 1). The default is 0.95, which corresponds  
#' to (0.05, 0.95) interval. Supplying 0.05 would give the same result

#' @param geom Use to override the default geom. Can be a list of multiple 
#' geoms, e.g. list("line","ribbon"), which is the default.
#' @param position Position adjustment, either as a string, or the result of 
#' a call to a position adjustment function.
#' @param fun.args Optional additional arguments passed on to the functions.
#' @param na.rm If FALSE, the default, missing values are removed with a 
#' warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? 
#' NA, the default, includes if any aesthetics are mapped. FALSE never 
#' includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather 
#' than combining with them. This is most useful for helper functions that 
#' define both data and aesthetics and shouldn't inherit behaviour from the 
#' default plot specification, e.g. borders.
#' @param ... other arguments passed on to layer. These are often aesthetics, 
#' used to set an aesthetic to a fixed value, like color = "red" or size = 3. 
#' They may also be parameters to the paired geom/stat.
#'
#' @return ggplot2 plot layer
#'
#' @examples
#' # default settings for normally distributed data, (5%,95%) interval,
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) +
#'   xgx_stat_pi(percent_level = 0.95)
#' 
#' # try different geom 
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_pi(percent_level = 0.95, geom = list("errorbar", "point", "line"))
#'  
#' # including multiple groups in same plot
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_pi(percent_level = 0.95, 
#'               ggplot2::aes(color = factor(group), fill = factor(group)),
#'               position = ggplot2::position_dodge(width = 0.5))
#'               
#' # including multiple percent intervals in same plot              
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) +
#'   xgx_stat_pi(percent_level = 0.90) + 
#'   xgx_stat_pi(percent_level = 0.80) + 
#'   xgx_stat_pi(percent_level = 0.70) + 
#'   xgx_stat_pi(percent_level = 0.60)      
#'  
#' @importFrom stats rnorm
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 position_dodge
#' @export
xgx_stat_pi <- function(mapping = NULL, data = NULL, percent_level = 0.95,
                        geom = list("line", "ribbon"),
                        position = "identity",
                        bins = NULL,
                        breaks = NULL,
                        fun.args = list(),
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {
  if (!(percent_level >= 0 && percent_level <= 1)) {
    stop("percent_level should be greater or equal 0 and less or equal 1")
  }
  
  percent_int <- function(y, percent_level) {
    percentile_value <- max(percent_level, 1 - percent_level)
    
    y <- stats::na.omit(y)
    
      percent_int_out <- data.frame(
        y = median(y),
        ymin = quantile(y, 1 - percentile_value),
        ymax = quantile(y, percentile_value)
      )
  }
  
  ret <- xgx_stat_ci(mapping = mapping,
                     data = data,
                     conf_level = NULL,
                     distribution = "normal",
                     bins = bins,
                     breaks = breaks,
                     geom = geom,
                     position = position,
                     fun.args = fun.args,
                     fun.data = function(y) percent_int(y, percent_level),
                     na.rm = na.rm,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes,
                     ...)
  
  # ret <- list()
  # for (igeom in geom) {
  #   temp <- ggplot2::stat_summary(mapping = mapping, data = data,
  #                                 geom = igeom, position = position, ...,
  #                                 fun.args = list(), na.rm = na.rm,
  #                                 show.legend = show.legend,
  #                                 inherit.aes = inherit.aes,
  #                                 fun.data = function(y) percent_int(y, percent_level)
  #   )
  #   
  #   if (igeom == "point") {
  #     if (is.null(temp$aes_params$size)) temp$aes_params$size <- 2
  #   }
  #   else if (igeom == "line") {
  #     if (is.null(temp$aes_params$size)) temp$aes_params$size <- 1
  #   }
  #   else if (igeom == "errorbar") {
  #     if (is.null(temp$aes_params$size)) temp$aes_params$size <- 1
  #     if (is.null(temp$geom_params$width)) {
  #       temp$geom_params$width <- 0
  #     }
  #   }
  #   else if (igeom == "ribbon") {
  #     if(is.null(temp$aes_params$alpha)) temp$aes_params$alpha <- 0.25
  #   }
  #   else if (igeom == "pointrange") {
  #     if(is.null(temp$aes_params$size)) temp$aes_params$size <- 1
  #     temp$geom$geom_params$fatten <- 2
  #   }
  #   
  #   
  #   ret[[paste0("geom_", igeom)]] <- temp
  # }
  
  return(ret)
}
