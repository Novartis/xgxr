#' Wrapper for stat_smooth that also can deal with class (ordinal, multinomial, or binary variables)
#' \code{xgx_stat_smooth} Smooths categorical or continuous data
#'
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
#' @param conf_level The percentile for the confidence interval (should fall 
#' between 0 and 1). The default is 0.95, which corresponds to a 95 percent 
#' confidence interval.
#' @param geom Use to override the default geom. Can be a list of multiple 
#' geoms, e.g. list("point","line","errorbar"), which is the default.
#' @param position Position adjustment, either as a string, or the result of 
#' a call to a position adjustment function.
#' 
#' @param method method (function) to use, eg. lm, glm, gam, loess, rlm. For datasets with n < 1000 default is loess. For datasets with 1000 or more observations defaults to gam.
#' Example: `"polr"` for ordinal data. If this is left as `NULL`, then a typical `StatSmooth` is applied
#' @param formula formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#' @param se display confidence interval around smooth? (TRUE by default, see level to control)
#' @param fullrange should the fit span the full range of the plot, or just the data
#' @param n number of points to evaluate smoother at
#' @param method.args Optional additional arguments passed on to the method.
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
#'
#'
#' @importFrom magrittr "%>%"
#' @export
xgx_stat_smooth <- function(mapping = NULL,
                        data = NULL,
                        conf_level = 0.95,
                        
                        geom = "smooth",
                        position = "identity",
                        ...,
                        method = NULL,
                        formula = NULL,
                        se = TRUE,
                        n = 80,
                        span = 0.75,
                        fullrange = FALSE,
                        level = 0.95,
                        method.args = list(),
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  lays <- list()

  # Assume OLS / LM / nls / nlsLM / glm etc. model
  ggproto_stat <- StatSmooth

  # Class Model
  if (is.null(method)){ }
  else{
    if (method %in% c("polr")) {
      ggproto_stat <- StatSmoothOrdinal
    }
  }
  
  # Default parameters
  gg_params = list(method = method,
                  formula = formula,
                  se = se,
                  n = n,
                  fullrange = fullrange,
                  level = level,
                  na.rm = na.rm,
                  orientation = orientation,
                  method.args = method.args,
                  span = span)
  

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
    
    lays[[paste0("geom_", igeom)]] <- lay  
  }
  
  return(lays)
}


predictdf.polr <- function(model, xseq, se, level){
  pred <- predict(model, newdata = data.frame(x = xseq), type = "probs") %>%
    data.frame() %>%
    mutate( x = xseq)
  pred.df <- pivot_longer(data = pred, cols = -x, names_to = "response", values_to = "y")
}



#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSmoothOrdinal <- ggproto("StatSmoothOrdinal", Stat,
        setup_params = function(data, params) {
         # params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
         msg <- character()
         
         if (is.null(params$formula)) {
           params$formula <- response ~ x
           msg <- c(msg, paste0("formula '", deparse(params$formula), "'"))
         }
         
         if (length(msg) > 0) {
           message("`geom_smooth()` using ", paste0(msg, collapse = " and "))
         }
         
         params
        },
        
        setup_data = function(data, params) {
         data
        },
        
        extra_params = c("na.rm", "orientation"),
        
        compute_panel = function(data, scales, method = NULL, formula = NULL,
                                se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                xseq = NULL, level = 0.95, method.args = list(),
                                na.rm = FALSE) {
         
         if (length(unique(data$x)) < 2) {
           # Not enough data to perform fit
           return(new_data_frame())
         }
         
         if (is.null(data$weight)) data$weight <- 1
         
         if (is.null(xseq)) {
           if (is.integer(data$x)) {
             if (fullrange) {
               xseq <- scales$x$dimension()
             } else {
               xseq <- sort(unique(data$x))
             }
           } else {
             if (fullrange) {
               range <- scales$x$dimension()
             } else {
               range <- range(data$x, na.rm = TRUE)
             }
             xseq <- seq(range[1], range[2], length.out = n)
           }
         }
         
         if (is.character(method)) {
           if (identical(method, "polr")) {
             method <- MASS::polr
           } else {
             method <- match.fun(method)
           }
         }
         
         base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
         
         n_bootstrap = 200
         iter_failed = 0
         prediction = NULL
         for (i in 1:n_bootstrap) {
           new_pred <- tryCatch ({
             # Boostrap by resampling entire dataset
             #   (prediction + residual doesn't work with ordinal data)
             data_boot <- sample_n(tbl = data,
                                   size = nrow(data),
                                   replace = TRUE)
             base.args <- list(quote(formula), data = quote(data_boot), weights = quote(weight))
             model_boot <- do.call(method, c(base.args, method.args))
             # Extract Bootstrapped Predictions
             predictdf.polr(model_boot, xseq, se, level)
           }, warning = function(w) {
             "There was a problem in the sampling."
           }
           )
           
           
           if (is.character(new_pred)) {
             iter_failed <- 1 + iter_failed
             next
           }
           
           if (is.null(prediction)) {
             prediction <- new_pred
           }
           else {
             prediction <- rbind(prediction, new_pred)
           }
         }
         
         prediction <- prediction %>%
           group_by(x, response) %>%
           summarize(ymin = quantile(na.omit(y), 0.05),
                     ymax = quantile(na.omit(y), 0.95),
                     y = median(y)) %>%
           ungroup()
         
         prediction <- merge(prediction, data %>% subset(,-c(x), by = response))
         
        },
        
        required_aes = c("x","response")
)