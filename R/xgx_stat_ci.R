#' Plot data with mean and confidence intervals
#'
#' \code{xgx_stat_ci} returns a ggplot layer plotting mean +/- confidence 
#' intervals
#' 
#' This function can be used to generate mean +/- confidence interval plots 
#' for different distributions, 
#' and multiple geoms with a single function call.
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
#' @param geom Use to override the default geom. Can be a list of multiple 
#' geoms, e.g. list("point","line","errorbar"), which is the default.
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
#' # default settings for normally distributed data, 95% confidence interval,  
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95)
#' 
#' # try different geom 
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95, geom = list("ribbon", "point", "line"))
#'  
#' # plotting lognormally distributed data
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = 10^(rep(c(1, 2, 3), each = 20) + stats::rnorm(60)),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95, distribution = "lognormal")
#'   
#' # note: you DO NOT need to use both distribution = "lognormal"
#' # and scale_y_log10()
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95) + xgx_scale_y_log10()
#'  
#' # plotting binomial data
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = stats::rbinom(60, 1, rep(c(0.2, 0.6, 0.8),
#'                    each = 20)),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95, distribution = "binomial")
#'  
#' # including multiple groups in same plot
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_stat_ci(conf_level = 0.95, distribution = "binomial", 
#'               ggplot2::aes(color = factor(group)),
#'               position = ggplot2::position_dodge(width = 0.5))
#'  
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats na.omit
#' @importFrom stats qt
#' @importFrom stats var
#' @importFrom binom binom.exact
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 position_dodge
#' @export
xgx_stat_ci <- function(mapping = NULL,
                        data = NULL,
                        conf_level = 0.95,
                        distribution = "normal",
                        bins = NULL,
                        breaks = NULL,
                        geom = list("point", "line", "errorbar"),
                        position = "identity",
                        fun.args = list(),
                        fun.data = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {

  lays <- list()

  # Confidence intervals via `xgx_conf_int` is the default function
  if (is.null(fun.data)) {
    fun.data <- function(y) xgx_conf_int(y = y,conf_level = conf_level,
                                         distribution = distribution)
  }

  # Default parameters
  gg_params = list(
    fun.args = fun.args,
    fun.data = fun.data,
    na.rm = na.rm,
    ...)

  # Non-binned
  if (is.null(bins) & is.null(breaks)) {
    ggproto_stat <- StatSummary
  }
  # Binned
  else {
    # Ordinal binned
    if (distribution %in% c("ordinal", "binomial", "multinomial")) {
      ggproto_stat <- StatSummaryBinOrdinal

      gg_params = append(gg_params, list(conf_level = conf_level,
                                      distribution = distribution,
                                      bins = bins,
                                      breaks = breaks))
    }

    # Continuous binned
    else {
      ggproto_stat <- StatSummaryBinQuant
      gg_params = append(gg_params, list(bins = bins,
                                         breaks = breaks))
    }
  }

  for (igeom in geom) {
    lay = layer(
      stat = ggproto_stat,
      data = data,
      mapping = mapping,
      geom = igeom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = gg_params
    )

    # Adjust aes to default xgx preference
    if (igeom == "point") {
      if (is.null(lay$aes_params$size)) lay$aes_params$size <- 2
    }
    else if (igeom == "line") {
      if (is.null(lay$aes_params$size)) lay$aes_params$size <- 1
    }
    else if (igeom == "errorbar") {
      if (is.null(lay$aes_params$size)) lay$aes_params$size <- 1
      if (is.null(lay$geom_params$width)) lay$geom_params$width <- 0
    }
    else if (igeom == "ribbon") {
      if(is.null(lay$aes_params$alpha)) lay$aes_params$alpha <- 0.25
    }
    else if (igeom == "pointrange") {
      if(is.null(lay$aes_params$size)){
        lay$aes_params$size <- 1
        lay$geom$geom_params$fatten <- 2
      }
    }

    lays[[paste0("geom_", igeom)]] <- lay  
  }
  
  return(lays)
}







#' Stat ggproto object for creating ggplot layers of binned confidence intervals
#' for probabiliities of classes in ordinal data
#'
#' 
#' \code{StatSummaryBinOrdinal} returns a ggproto object for plotting mean +/- confidence bins
#' 
#'
#' @return ggplot2 ggproto object
#'
#' @export
StatSummaryBinOrdinal <- ggplot2::ggproto("StatSummaryBinOrdinal", ggplot2::Stat,

     required_aes = c("x"),
     # default_aes = aes(fill = ..y..),
     
     compute_group = function(data, scales, conf_level, distribution, bins, breaks,
                              fun.data = NULL,
                              fun.args = list()) {
       return(data)
     },

     setup_data = function(data, params) {

       # Calculate percentages for each category across each bin
       # Get median x value for each bin
       median_x <- data %>% mutate(quantile_index = dplyr::ntile(data$x, params$bins)) %>%
         group_by(quantile_index) %>%
         summarize(x = median(x))
       
       # Get the number of each category in each bin 
       counts <- data %>% mutate(quantile_index = dplyr::ntile(data$x, params$bins)) %>%
         group_by(quantile_index, colour, PANEL, group) %>%
         summarize(count = length(x))

       # Combine the x and y data
       data <- merge(median_x, counts, by = "quantile_index", all = TRUE)

       # Now calculate the confidence intervals for the multinomial data
       data <- data %>% group_by(quantile_index) %>%
         mutate(x = median(x),
                y=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$est,
                ymin=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$lwr.ci,
                ymax=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$upr.ci) %>%
         ungroup() %>% group_by(group)
       return(data)
     }
)

#' Stat ggproto object for binning by quantile for xgx_stat_ci
#'
#' Source:
#'     https://github.com/tidyverse/ggplot2/blob/351eb41623397dea20ed0059df62a4a5974d88cb/R/stat-summary-bin.R
#' 
#' \code{StatSummaryBinQuant} returns a ggproto object for plotting mean +/- confidence bins
#' 
#'
#' @return ggplot2 ggproto object
#'
#' @export
StatSummaryBinQuant <- ggproto("StatSummaryBinQuant", Stat,
       required_aes = c("x", "y"),
       
       extra_params = c("na.rm", "orientation"),
       setup_params = function(data, params) {
         params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
         params
       },

       compute_group = function(data, scales,
                                fun.data = NULL,
                                fun = NULL,
                                fun.max = NULL,
                                fun.min = NULL,
                                fun.args = list(),
                                bins = NULL,
                                binwidth = NULL,
                                breaks = NULL,
                                origin = NULL,
                                right = FALSE,
                                na.rm = FALSE,
                                flipped_aes = FALSE) {
         # data <- flip_data(data, flipped_aes)
         fun <- ggplot2:::make_summary_fun(fun.data, fun, fun.max, fun.min, fun.args)

         # Use breaks if available instead of bins
         if (!is.null(breaks)) {
           breaks <- breaks
         }
         else {
           # Calculate breaks from number of bins
           breaks <- quantile(data$x,probs = seq(0, 1, 1/bins))
         }
         
         data$bin <- cut(data$x, breaks, include.lowest = TRUE, labels = FALSE)
         out <- ggplot2:::dapply(data, "bin", fun)
         
         locs <- ggplot2:::bin_loc(breaks, out$bin)
         out$x <- locs$mid
         return(out)
       }
)
