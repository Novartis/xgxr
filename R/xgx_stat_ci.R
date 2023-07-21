
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
#' @param bins number of bins to cut up the x data, cuts data into quantiles.
#' @param breaks breaks to cut up the x data, if this option is used, bins is ignored
#' @param geom Use to override the default geom. Can be a list of multiple 
#' geoms, e.g. list("point","line","errorbar"), which is the default.
#' @param position Position adjustment, either as a string, or the result of 
#' a call to a position adjustment function.
#' @param fun.args Optional additional arguments passed on to the functions.
#' @param fun.data A function that is given the complete data and should return 
#' a data frame with variables ymin, y, and ymax.
#' @param na.rm If FALSE, the default, missing values are removed with a 
#' warning. If TRUE, missing values are silently removed.
#' @param orientation The orientation of the layer, passed on to ggplot2::stat_summary. 
#' Only implemented for ggplot2 v.3.3.0 and later. The default ("x") summarizes y values over
#' x values (same behavior as ggplot2 v.3.2.1 or earlier). Setting \code{orientation = "y"} will 
#' summarize x values over y values, which may be useful in some situations where you want to flip
#' the axes, e.g. to create forest plots. Setting \code{orientation = NA} will try to automatically
#' determine the orientation from the aesthetic mapping (this is more stable for ggplot2 v.3.3.2
#' compared to v.3.3.0).
#' See \code{\link[ggplot2:stat_summary]{stat_summary}} (v.3.3.0 or greater) for more information. 
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
#' # plotting ordinal or multinomial data
#' set.seed(12345) 
#' data = data.frame(x = 120*exp(stats::rnorm(100,0,1)), 
#'               response = sample(c("Mild","Moderate","Severe"), 100, replace = TRUE),
#'               covariate = sample(c("Male","Female"), 100, replace = TRUE))
#'   
#' xgx_plot(data = data) + 
#'   xgx_stat_ci(mapping = ggplot2::aes(x = x, response = response, colour = covariate), 
#'               distribution = "ordinal", bins = 4) + 
#'   ggplot2::scale_y_continuous(labels = scales::percent_format()) + ggplot2::facet_wrap(~response)
#' 
#' xgx_plot(data = data) + 
#'   xgx_stat_ci(mapping = ggplot2::aes(x = x, response = response, colour = response), 
#'               distribution = "ordinal", bins = 4) + 
#'   ggplot2::scale_y_continuous(labels = scales::percent_format()) + ggplot2::facet_wrap(~covariate)
#' 
#' # Example plotting categorical vs categorical data
#' set.seed(12345)
#' data = data.frame(x = 120*exp(stats::rnorm(100,0,1)),
#'                   response = sample(c("Trt1", "Trt2", "Trt3"), 100, replace = TRUE),
#'                   covariate = factor(
#'                     sample(c("White","Black","Asian","Other"), 100, replace = TRUE),
#'                                      levels = c("White", "Black", "Asian", "Other")))
#' 
#' xgx_plot(data = data) +
#'   xgx_stat_ci(mapping = ggplot2::aes(x = response, response = covariate),
#'               distribution = "ordinal") +
#'   xgx_stat_ci(mapping = ggplot2::aes(x = 1, response = covariate), geom = "hline",
#'               distribution = "ordinal") +
#'   ggplot2::scale_y_continuous(labels = scales::percent_format()) + 
#'   ggplot2::facet_wrap(~covariate) + 
#'   ggplot2::xlab("Treatment group") + 
#'   ggplot2::ylab("Percent of subjects by category")
#' 
#' # Same example with orientation flipped (only works for ggplot2 v.3.3.0 or later)
#' # only run if ggplot2 v.3.3.0 or later
#' ggplot2_geq_v3.3.0 <- utils::compareVersion(
#'   as.character(utils::packageVersion("ggplot2")), '3.3.0') >= 0
#' 
#' if(ggplot2_geq_v3.3.0){
#' 
#' xgx_plot(data = data) +
#' xgx_stat_ci(mapping = ggplot2::aes(y = response, response = covariate), orientation = "y",
#'             distribution = "ordinal") +
#'   xgx_stat_ci(mapping = ggplot2::aes(y = 1, response = covariate), orientation = "y", 
#'               geom = "vline", distribution = "ordinal") +
#'   ggplot2::scale_x_continuous(labels = scales::percent_format()) +
#'   ggplot2::facet_wrap(~covariate) +
#'   ggplot2::ylab("Treatment group") +
#'   ggplot2::xlab("Percent of subjects by category")
#'   
#' }
#' 
#'  
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats na.omit
#' @importFrom stats qt
#' @importFrom stats var
#' @importFrom binom binom.exact
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 StatSummary
#' 
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
                        orientation = "x",
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
  
  # Compare to ggplot2 version 3.3.0
  # If less than 3.3.0, then don't include orientation option
  ggplot2_geq_v3.3.0 <- utils::compareVersion(as.character(utils::packageVersion("ggplot2")), '3.3.0') >= 0
  
  if(ggplot2_geq_v3.3.0){
    gg_params$orientation = orientation
  }else{
    if(!(orientation %in% "x")){
      warning('orientation other than "x" not supported for ggplot2 versions less than 3.3.0')
    }
  }

  # Ordinal, binned or not binned
  if(distribution %in% c("ordinal", "multinomial")){
    ggproto_stat <- StatSummaryOrdinal
    
    gg_params = append(gg_params, list(conf_level = conf_level,
                                       distribution = distribution,
                                       bins = bins,
                                       breaks = breaks))
    
  }else{
    # Continuous Non-binned
    if (is.null(bins) & is.null(breaks)) {
      ggproto_stat <- ggplot2::StatSummary
    }

    # Continuous binned
    else {
      ggproto_stat <- StatSummaryBinQuant
      gg_params = append(gg_params, list(bins = bins,
                                         breaks = breaks))
    }
  }

  for (igeom in geom) {
    lay = ggplot2::layer(
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

#  Function for computing confidence intervals
#'
#' \code{xgx_conf_int} returns a dataframe with mean +/- confidence intervals
#'
#' @param y data to compute confidence interval of
#' @param conf_level The percentile for the confidence interval (should fall 
#' between 0 and 1). The default is 0.95, which corresponds to a 95 percent 
#' confidence interval.
#' @param distribution The distribution which the data follow, used for 
#' calculating confidence intervals. The options are "normal", "lognormal", 
#' and "binomial". The "normal" option will use the Student t Distribution 
#' to calculate confidence intervals, the "lognormal" option will transform 
#' data to the log space first. The "binomial" option will use the
#' \code{\link[binom:binom.confint]{binom.confint}} function to calculate the
#' confidence 
#' intervals. Note: binomial data must be numeric and contain only 1's and 0's.
#' The "multinomial" or "ordinal" options will use DescTools::MultinomCI 
#' @param ci_method Method to pass to binom.confint or MultinomCI. Defaults are 
#' "exact" and "goodman", respectively.
#'
#'
#' @return data.frame
#'
#' @examples
#' # default settings for normally distributed data, 95% confidence interval,  
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60),
#'                    group = rep(1:3, 20))
#' xgx_conf_int(data$y)
#'   
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats na.omit
#' @importFrom stats qt
#' @importFrom stats var
#' @importFrom binom binom.confint
#' @importFrom DescTools MultinomCI
#' @export
xgx_conf_int = function(y, conf_level = 0.95, distribution = "normal", ci_method = NULL) {
  
  if (!(conf_level > 0.5 && conf_level < 1)) {
    stop("conf_level should be greater than 0.5 and less than 1")
  }
  
  percentile_value <- conf_level + (1 - conf_level) / 2
  
  y <- stats::na.omit(y)

  if (distribution == "normal") {
    if(!is.null(ci_method)){ warning("Ignoring ci_method for normal CI calculation")}
    
    mu <- mean(y)
    qtt <- stats::qt(percentile_value, length(y) - 1)
    s_v = sqrt(stats::var(y) / (length(y) - 1))

    conf_int_out <- data.frame(
      y = mu,
      ymin = mu - qtt * s_v,
      ymax = mu + qtt * s_v
    )
    
    if(length(y) == 1){
      conf_int_out <- data.frame(
        y = mu,
        ymin = -Inf,
        ymax = Inf
      )
      warning(paste("Infinite CI warning:","One of the bins of y contains only 1 value.", 
              "Confidence interval is infinite for that bin,", 
              "but is only displayed to the edge of the plotting area.", 
              "Interpret with caution.", sep = "\n  "))
    }
    
  } else if (distribution == "lognormal") {
    if(!is.null(ci_method)){ warning("Ignoring ci_method for lognormal CI calculation")}
    
    yy <- log(y)
    mu <- mean(yy)
    qtt <- stats::qt(percentile_value, (length(yy)-1) )
    s_v <- sqrt(stats::var(yy) / (length(yy)-1))

    # e^mu = median value - http://jse.amstat.org/v13n1/olsson.html
    conf_int_out <- data.frame(
      y = exp(mu),
      ymin = exp(mu - qtt * s_v),
      ymax = exp(mu + qtt * s_v)
    )
    
    if(length(y) == 1){
      conf_int_out <- data.frame(
        y = exp(mu),
        ymin = 0,
        ymax = Inf
      )
      warning(paste("Infinite CI warning:","One of the bins of y contains only 1 value.", 
                    "Confidence interval is infinite for that bin,", 
                    "but is only displayed to the edge of the plotting area.", 
                    "Interpret with caution.", sep = "\n  "))
    }
    
  } else if (distribution == "binomial") {
    
    if(is.null(ci_method)){ 
      ci_method = "exact" 
      }
    stats <- binom::binom.confint(sum(y), length(y), 
                                conf.level = conf_level,
                                methods = ci_method)

    conf_int_out <- data.frame(
      y = mean(y),
      ymin = stats$lower,
      ymax = stats$upper)
    
  } else if (distribution %in% c("multinomial", "ordinal")) {

    if(is.null(ci_method)){ 
      ci_method = "goodman" 
    }
    
    # Assuming `y` is a not yet collapsed to the number of counts per category
    count <- table(y)  #as.data.frame(table(y))$Freq
    stats <- as.data.frame(DescTools::MultinomCI(count, conf.level = conf_level, method = ci_method))

    conf_int_out <- data.frame(
      y = stats$est,
      ymin = stats$lwr.ci,
      ymax = stats$upr.ci)
    
  } else {
    stop("distribution must be either normal, lognormal, binomial,
         or multinomial/ordinal.")
  }
  return(conf_int_out)
}

#' Stat ggproto object for creating ggplot layers of binned confidence intervals
#' for probabiliities of classes in ordinal data
#'
#' \code{StatSummaryOrdinal} returns a ggproto object for plotting mean +/- confidence intervals
#' for ordinal data. It also allows for binning values on the independent axis.
#' 
#'
#' @return ggplot2 ggproto object
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom ggplot2 aes
#' @export
StatSummaryOrdinal <- ggplot2::ggproto("StatSummaryOrdinal", ggplot2::Stat,
                                          
     required_aes = c("x", "response"),
     
     extra_params = c("na.rm", "orientation"),
                                          
     compute_group = function(data, scales, conf_level, distribution, bins, breaks,
                              fun.data = NULL,
                              fun.args = list()) {
       return(data)
     },
     
     setup_params = function(self, data, params) {
       params$flipped_aes <- has_flipped_aes(data, params)
       
       required_aes <- self$required_aes
       
       if(params$flipped_aes){
         required_aes <- switch_orientation(self$required_aes)
       }
       
       # check required aesthetics
       ggplot2:::check_required_aesthetics(
         required_aes,
         c(names(data), names(params)),
         ggplot2:::snake_class(self)
       )
       
       # Make sure required_aes consists of the used set of aesthetics in case of
       # "|" notation in self$required_aes
       required_aes <- intersect(
         names(data),
         unlist(strsplit(required_aes, "|", fixed = TRUE))
       )
       
       # aes_to_group are the aesthetics that are different from response,
       # it's assumed that these should split the data into groups for calculating CI,
       # e.g. coloring by a covariate
       #
       # aes_not_to_group are aesthetics that are identical to response,
       # it's assumed that these are only for applyng aesthetics to the end result, 
       # e.g. coloring by response category
       params$aes_to_group <- c()
       params$aes_not_to_group <- c()
       
       # go through PANEL, colour, fill, linetype, shape
       if( (data %>% subset(, c(response, PANEL)) %>% unique() %>% dim)[1] == length(unique(data$response) )){
         params$aes_not_to_group <- c(params$aes_not_to_group, "PANEL")
       }else{
         params$aes_to_group <- c(params$aes_to_group, "PANEL")
       }
       
       if(is.null(data$colour)){
         
       }else if((data %>% subset(, c(response, colour)) %>% unique() %>% dim)[1] == length(unique(data$response))){
         params$aes_not_to_group <- c(params$aes_not_to_group, "colour")
       }else{
         params$aes_to_group <- c(params$aes_to_group, "colour")
       }
       
       if(is.null(data$linetype)){
         
       }else if((data %>% subset(, c(response, linetype)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
         params$aes_not_to_group <- c(params$aes_not_to_group, "linetype")
       }else{
         params$aes_to_group <- c(params$aes_to_group, "linetype")
       }
       
       if(is.null(data$fill)){
         
       }else if((data %>% subset(, c(response, fill)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
         params$aes_not_to_group <- c(params$aes_not_to_group, "fill")
       }else{
         params$aes_to_group <- c(params$aes_to_group, "fill")
       }
       
       if(is.null(data$shape)){
         
       }else if((data %>% subset(, c(response, shape)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
         params$aes_not_to_group <- c(params$aes_not_to_group, "shape")
       }else{
         params$aes_to_group <- c(params$aes_to_group, "shape")
       }
       
       if(length(params$aes_not_to_group) == 0){
         warning("In xgx_stat_ci: \n  No aesthetics defined to differentiate response groups.\n  Suggest to add color = response, linetype = response, or similar to aes() mapping.",
                 call. = FALSE)
       }else{
         message(paste0("In xgx_stat_ci: \n  The following aesthetics are identical to response: ", 
                        paste0(params$aes_not_to_group, collapse = ", "), 
                        "\n  These will be used for differentiating response groups in the resulting plot."))         
       }
       
       if(length(params$aes_to_group) > 0){
         message(paste0("In xgx_stat_ci: \n  The following aesthetics are different from response: ", 
                        paste0(params$aes_to_group, collapse = ", "), 
                        "\n  These will be used to divide the data into different groups before calculating summary statistics on the response."))
       }
       
       if("mapped_discrete" %in% attr(data$x, "class") & (!is.null(params$breaks) | !is.null(params$bins))){
         message("In xgx_stat_ci: \n ignoring bins or breaks supplied with discrete x values")
         params$breaks <- NULL
         params$bins <- NULL
       }
       
       params
     },

     setup_data = function(self, data, params) {
       
       data <- flip_data(data, params$flipped_aes)
       
       # Define new grouping variable for which to split the data computation 
       # (excludes aesthetics that are identical to the Response variable)
       if(is.null(params$aes_to_group)){
         data <- data %>% mutate(group2 = 1)
       }else{
         groups <- unique(data %>% subset(, params$aes_to_group))
         groups <- groups %>%
           mutate(group2 = 1:dim(groups)[1])
         
         data <- data %>% merge(groups)
       }
       
       if(is.null(params$breaks)){
         if(is.null(params$bins)){
           data <- data %>% mutate(x_bin = x)
           median_x <- data %>% 
             subset(,c(x_bin, group2, x)) %>% 
             unique() %>%  
             ungroup() %>% group_by(x_bin, group2)
             
         }else{

           # Calculate percentages for each category across each bin
           data <- data %>% mutate(x_bin = dplyr::ntile(data$x, params$bins))
         }
       
       }else{
         data <- data %>% mutate(x_bin = cut(data$x, params$breaks))
       }
       
       if(!is.null(params$breaks) | !is.null(params$bins)){
         # Get median x value for each bin
         median_x <- data %>% ungroup() %>%
           group_by(x_bin, group2) %>%
           summarize(x = median(x), .groups = "keep")
       }
       
       # Get the number of each category in each bin 
       counts <- data %>% ungroup() %>%
         group_by(x_bin, group2, response, .drop = FALSE) %>%
         summarize(count = length(x), .groups = "keep") %>% 
         merge(data %>% subset(,-c(x, x_bin)), 
               by = c("response","group2")) %>% 
         unique()
       
       # Combine the x and y data
       data <- merge(median_x, counts, by = c("x_bin", "group2"), all = TRUE)
       
       # Now calculate the confidence intervals for the multinomial data
       data <- data %>% group_by(x_bin, group2) %>%
         mutate(x = median(x),
                y=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$est,
                ymin=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$lwr.ci,
                ymax=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$upr.ci) %>%
         ungroup() %>% group_by(group, group2)
       
       # if you want to use geom hline, then need yintercept defined
         data <- data %>% mutate(yintercept = y)
         
         data <- flip_data(data, params$flipped_aes)
         
       return(data)
     },
     
     compute_layer = function(self, data, params, layout) {
       data
     },
     
     compute_panel = function(self, data, scales, ...) {
       data
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
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom ggplot2 aes
#' @export
StatSummaryBinQuant <- ggplot2::ggproto("StatSummaryBinQuant", ggplot2::Stat,
                               required_aes = c("x", "y"),
                               
                               extra_params = c("na.rm", "orientation"),
                               setup_params = function(data, params) {
                                 # gg_util_url <- "https://raw.githubusercontent.com/tidyverse/ggplot2/7e5ff921c50fb0beb203b115397ea33fee410a54/R/utilities.r"
                                 # eval(text = RCurl::getURL(gg_util_url, ssl.verifypeer = FALSE))
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
                                   if (any(duplicated(breaks)))
                                     stop(paste("The data cannot be uniquely divided into", bins, "bins, probably because a single value is repeated many times in the dataset.\n",
                                                "It is recommended to specify the breaks directly instead of the number of bin"))
                                 }
                                 
                                 data$bin <- cut(data$x, breaks, include.lowest = TRUE, labels = FALSE)
                                 out <- ggplot2:::dapply(data, "bin", fun)
                                 
                                 locs <- ggplot2:::bin_loc(breaks, out$bin)
                                 out$x <- locs$mid
                                 return(out)
                               }
)

#
# From ggplot2::ggplot_global
# Environment that holds various global variables and settings for ggplot,
# such as the current theme. It is not exported and should not be directly
# manipulated by other packages.
ggplot_global <- new.env(parent = emptyenv())

# The current theme. Defined here only as placeholder, and defined properly
# in file "theme-current.R". This setup avoids circular dependencies among
# the various source files.
ggplot_global$theme_current <- list()

# Element tree for the theme elements. Defined here only as placeholder, and
# defined properly in file "theme-elements.r".
ggplot_global$element_tree <- list()

# List of all aesthetics known to ggplot
# (In the future, .all_aesthetics should be removed in favor
# of direct assignment to ggplot_global$all_aesthetics, see below.)
.all_aesthetics <- c(
  "adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
  "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
  "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z"
)

ggplot_global$all_aesthetics <- .all_aesthetics

# Aesthetic aliases
# (In the future, .base_to_ggplot should be removed in favor
# of direct assignment to ggplot_global$base_to_ggplot, see below.)
.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

ggplot_global$base_to_ggplot <- .base_to_ggplot

ggplot_global$x_aes <- c("x", "xmin", "xmax", "xend", "xintercept",
                         "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")

ggplot_global$y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
                         "ymax_final", "lower", "middle", "upper", "y0")

#
#
# From ggplot2::utilites github
#
#
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

is_mapped_discrete <- function(x) inherits(x, "mapped_discrete")

has_flipped_aes <- function(data, params = list(), main_is_orthogonal = NA,
                            range_is_orthogonal = NA, group_has_equal = FALSE,
                            ambiguous = FALSE, main_is_continuous = FALSE,
                            main_is_optional = FALSE) {
  # Is orientation already encoded in data?
  if (!is.null(data$flipped_aes)) {
    not_na <- which(!is.na(data$flipped_aes))
    if (length(not_na) != 0) {
      return(data$flipped_aes[[not_na[1L]]])
    }
  }
  
  # Is orientation requested in the params
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }
  
  x <- data$x %||% params$x
  y <- data$y %||% params$y
  xmin <- data$xmin %||% params$xmin
  ymin <- data$ymin %||% params$ymin
  xmax <- data$xmax %||% params$xmax
  ymax <- data$ymax %||% params$ymax
  
  # Does a single x or y aesthetic corespond to a specific orientation
  if (!is.na(main_is_orthogonal) && xor(is.null(x), is.null(y))) {
    return(is.null(y) == main_is_orthogonal)
  }
  
  has_x <- !is.null(x)
  has_y <- !is.null(y)
  
  # Does a provided range indicate an orientation
  if (!is.na(range_is_orthogonal)) {
    if (!is.null(ymin) || !is.null(ymax)) {
      return(!range_is_orthogonal)
    }
    if (!is.null(xmin) || !is.null(xmax)) {
      return(range_is_orthogonal)
    }
  }
  
  # If ambiguous orientation = NA will give FALSE
  if (ambiguous && (is.null(params$orientation) || is.na(params$orientation))) {
    return(FALSE)
  }
  
  # Is there a single actual discrete position
  y_is_discrete <- is_mapped_discrete(y)
  x_is_discrete <- is_mapped_discrete(x)
  if (xor(y_is_discrete, x_is_discrete)) {
    return(y_is_discrete != main_is_continuous)
  }
  
  # Does each group have a single x or y value
  if (group_has_equal) {
    if (has_x) {
      if (length(x) == 1) return(FALSE)
      x_groups <- vapply(split(data$x, data$group), function(x) length(unique(x)), integer(1))
      if (all(x_groups == 1)) {
        return(FALSE)
      }
    }
    if (has_y) {
      if (length(y) == 1) return(TRUE)
      y_groups <- vapply(split(data$y, data$group), function(x) length(unique(x)), integer(1))
      if (all(y_groups == 1)) {
        return(TRUE)
      }
    }
  }
  
  # default to no
  FALSE
}


flip_data <- function(data, flip = NULL) {
  flip <- flip %||% any(data$flipped_aes) %||% FALSE
  if (isTRUE(flip)) {
    names(data) <- switch_orientation(names(data))
  }
  data
}


flipped_names <- function(flip = FALSE) {
  x_aes <- ggplot_global$x_aes
  y_aes <- ggplot_global$y_aes
  if (flip) {
    ret <- as.list(c(y_aes, x_aes))
  } else {
    ret <- as.list(c(x_aes, y_aes))
  }
  names(ret) <- c(x_aes, y_aes)
  ret
}

switch_orientation <- function(aesthetics) {
  ggplot_global <- list2env(
    list(x_aes = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
    y_aes = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "ylower", "ymiddle", "yupper", "y0")))
  
  # We should have these as globals somewhere
  x <- ggplot_global$x_aes
  y <- ggplot_global$y_aes
  x_aes <- match(aesthetics, x)
  x_aes_pos <- which(!is.na(x_aes))
  y_aes <- match(aesthetics, y)
  y_aes_pos <- which(!is.na(y_aes))
  if (length(x_aes_pos) > 0) {
    aesthetics[x_aes_pos] <- y[x_aes[x_aes_pos]]
  }
  if (length(y_aes_pos) > 0) {
    aesthetics[y_aes_pos] <- x[y_aes[y_aes_pos]]
  }
  aesthetics
}
                         
