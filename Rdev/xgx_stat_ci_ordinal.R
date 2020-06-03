#' Plot data with mean and confidence intervals
#'
#' \code{xgx_ci_summary} returns a ggplot layer plotting mean +/- confidence 
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
#'   xgx_summary_ci(conf_level = 0.95)
#' 
#' # try different geom 
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_summary_ci(conf_level = 0.95, geom = list("ribbon", "point", "line"))
#'  
#' # plotting lognormally distributed data
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = 10^(rep(c(1, 2, 3), each = 20) + stats::rnorm(60)),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_summary_ci(conf_level = 0.95, distribution = "lognormal")
#'   
#' # note: you DO NOT need to use both distribution = "lognormal"
#' # and scale_y_log10()
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_summary_ci(conf_level = 0.95) + xgx_scale_y_log10()
#'  
#' # plotting binomial data
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = stats::rbinom(60, 1, rep(c(0.2, 0.6, 0.8),
#'                    each = 20)),
#'                    group = rep(1:3, 20))
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_summary_ci(conf_level = 0.95, distribution = "binomial")
#'  
#' # including multiple groups in same plot
#' xgx_plot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_summary_ci(conf_level = 0.95, distribution = "binomial", 
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
#' @importFrom ggplot2 position_dodeg
#' @export
xgx_stat_ci <- function(
                    data = NULL,
                    mapping = NULL,
                    geom = list("point", "line", "errorbar"),
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA, 
                    inherit.aes = TRUE,
                    
                    conf_level = 0.95,
                    distribution = "normal",
                    bins = NULL,
                    ...) {
  

  
  
  lays <- list()
  
  if (distribution == "ordinal") {
    # data$y = as.double(data$y)
    # lays[["scale_y_continuous()"]] <- scale_y_continuous()
    lays[["scale_y_continuous()"]] <- coord_trans(limy = c(0.0,1.0))
    lays[["scale_y_continuous()"]] <- remove_y_scales("discrete")
    # lays[["scale_y_continuous()"]] <- scale_y_continuous(labels = scales::percent)
  }
  
  for (igeom in geom) {
    if (is.null(bins)) {
      lay = layer(
        stat = StatSummary,
        data = data,
        mapping = mapping,
        geom = igeom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          fun.args = list(),
          fun.data = function(y) xgx_conf_int(y, conf_level, distribution),
          na.rm = na.rm,
          ...)
      )
    }
    else {
      lay = layer(
        stat = StatCI,
        data = data,
        mapping = mapping,
        geom = igeom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          conf_level = conf_level,
          distribution = distribution,
          bins = bins,
          na.rm = na.rm,
          ...)
      )
    }

    # TODO:
    #   - This would be cleaner with a dictionary
    #   - Move this to aes_default in ggproto?
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
#'
#' 
#' \code{StatCI} returns a ggproto object for plotting mean +/- confidence bins
#' 
#'
#' @return ggplot2 ggproto object
#'
#' @export
StatCI <- ggplot2::ggproto("StatCI", ggplot2::Stat,
                  
    required_aes = c("x","y"),
    default_aes = aes(fill = ..y..),
    
    compute_group = function(data, scales, conf_level, distribution, bins) {
    
      if (distribution == "ordinal") {
        # print(scales)
        # print(scales$y)
        print(scales$y$is_discrete)
        # scales$y$is_discerete <- FALSE
        # scales$y <- continuous_scale(limits = c(0,1))
        # print(scales$y$is_discrete)
        stat_data <- data
        # print(data)
        # stat_data <- data %>% group_by(quantile_index) %>%
        #                       summarize(colour = unique(colour),
        #                                 x = median(x),
        #                                 ymin = DescTools::MultinomCI(y, conf_level)$ymin,
        #                                 ymax = DescTools::MultinomCI(y, conf_level)$ymax,
        #                                 y = DescTools::MultinomCI(y, conf_level)$y) %>%
        #                      ungroup()
        # print(stat_data)
        # # print(stat_data)
        # # # Hacky solution - treat each group as binomial
        # # temp_distribution = "binomial"
        # # # # The `y` values should be scaled to the number of ordinal variables
        # # # num_categories = max(data$y)
        # # # # For some reason it needs this + the mutate...
        # # # ytemp = data$y / num_categories
        # # 
        # # stat_data <- data %>% mutate(quantile_index = dplyr::ntile(data$x, bins),
        # #                              ytemp = data$y / num_categories) %>%
        # #                       group_by(quantile_index) %>%
        # #                       summarize(x = median(x),
        # #                                 ymin = xgx_conf_int(ytemp, conf_level, temp_distribution)$ymin,
        # #                                 ymax = xgx_conf_int(ytemp, conf_level, temp_distribution)$ymax,
        # #                                 y = xgx_conf_int(ytemp, conf_level, temp_distribution)$ys) %>%
        # #                       subset(select = -c(ytemp)) %>%
        # #                       ungroup()
      }
      else {
        stat_data <- data %>% mutate(quantile_index = dplyr::ntile(data$x, bins)) %>%
                              group_by(quantile_index) %>%
                              summarize(x = median(x),
                                        ymin = xgx_conf_int(y, conf_level, distribution)$ymin,
                                        ymax = xgx_conf_int(y, conf_level, distribution)$ymax,
                                        y = xgx_conf_int(y, conf_level, distribution)$y) %>%
                              ungroup()
      }

      return(stat_data)
    },
    
    setup_data = function(data, params){
      # print(scales)
      # print(data)
      # print(data)
      data$y <- as.double(data$y)
      if (params$distribution == "ordinal"){
        # print(data)
        median_x <- data %>% mutate(quantile_index = dplyr::ntile(data$x, params$bins)) %>%
                            group_by(quantile_index) %>%
                            summarize(x = median(x))
        # print(median_x)
        counts <- data %>% mutate(quantile_index = dplyr::ntile(data$x, params$bins)) %>%
                          group_by(quantile_index, y) %>%
                          summarize(count = length(y),
                                    colour = unique(colour),
                                    fill = unique(fill),
                                    PANEL = unique(PANEL),
                                    group = unique(group))
        # print(counts)
        data <- merge(median_x, counts, by = "quantile_index", all = TRUE)
        # print(data)
        data <- data %>% group_by(quantile_index) %>%
                  mutate(
                    # colour = unique(colour),
                    # fill = unique(fill),
                    # group = unique(group),
                            x = median(x),
                            y=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$est,
                            ymin=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$lwr.ci,
                            ymax=as.data.frame(DescTools::MultinomCI(count, params$conf_level))$upr.ci) %>%
                  ungroup() %>% group_by(group)
        # print(data)

        # print(data)

        # group_by(quantile_index) %>%
        #   summarize(x = median(x),
        #             y=(DescTools::MultinomCI(count, params$conf_level))["est"],
        #             ymin=(DescTools::MultinomCI(count, params$conf_level))["lwr.ci"],
        #             ymax=(DescTools::MultinomCI(count, params$conf_level))["upr.ci"]) %>%
          # ungroup()
        # print(data)
        # categories = unique(data$y)
        # num_categories = length(categories)
        # print(data)
        # # Make categorical data into binary of classes by pivoting / melting
        # data <- data %>% cbind(dummies::dummy.data.frame(data, names = "y", all = FALSE)) %>%
        #                       pivot_longer(cols = paste("y", categories, sep = ""),
        #                                    names_to = "dummy_group",
        #                                    values_to = "dummy_y",
        #                                    names_repair = "minimal") %>%
        #                       # Remove original "group" and "y"
        #                       subset(select = -c(group, y)) %>%
        #                       # Remove "y" from values like "y1, y2, ..."
        #                       # Replace "y" and "group" column
        #                       mutate(group = as.integer(stringr::str_replace(dummy_group, "y", "")),
        #                              y = dummy_y * num_categories) %>%
        #                       subset(select = -c(dummy_group, dummy_y))
      }
      # print(data)
      return(data)
    }
)


remove_y_scales <- function(x, scale_type) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$scales[[1]])[1] == scale_type
                     })
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}