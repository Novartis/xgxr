#' @importFrom brms brm
#' @importFrom brms 
#' @export
xgx_ordinal_regression_plot <- function(data = NULL,
                                        formula = "y ~ x",
                                        levels = NULL,
                                        nbins = 6,
                                        n_bootstrap = 100,
                                        n_resolution = 30,
                                        ci = 0.95) {

  pred_df <- xgx_ordinal_bootstrap_predictions(data = data,
                                               formula = formula,
                                               n_bootstrap = n_bootstrap,
                                               n_resolution = n_resolution,
                                               y_as_probability = TRUE)

  gg <- ggplot(pred_df,
               aes_string(x = "x",
                          y = "y",
                          color = "name",
                          fill = "name"))

  # Plot confidence interval ribbon for fitted draw predictions
  gg <- gg + stat_summary(geom = "ribbon")

  # Plot confidence intervals for observed data binned by quantiles
  gg <- gg + xgx_stat_ci(data = data,
                         aes(x = x,
                             color = class_for_probability),
                         bins = nbins,
                         distribution = "ordinal")

  # Clean up remaining aesthetics of plot
  gg <- gg + xgx_scale_x_log10()
  gg <- gg + labs(x = all.vars(formula)[1],
                  y = "Probability of being within Severity Group",
                  title = "Ordinal Regression Predictions vs. Observed Data",
                  fill = "Predictions",
                  color = "Observed Data")
  gg <- gg + guides(fill = guide_legend(override.aes = list(linetype = 0, shape = "")),
                    color = guide_legend(override.aes = list(linetype = 0, alpha = .3)))
  gg <- gg + xgx_annotate_status(status)
  gg <- gg + xgx_theme()
  return(gg)
}



xgx_ordinal_bootstrap_predictions <- function(data,
                                              formula = "y ~ x",
                                              n_bootstrap = 100,
                                              n_resolution = 50,
                                              y_as_probability = TRUE) {
    # Formula can be string or NSE
    if (class(formula) == "character"){
      formula <- as.formula(formula)
    }

    # Check formula is of form y ~ x for plotting
    dependent_variable <- all.vars(formula)[1]
    independent_variables <- tail(all.vars(formula), -1)
    ind_var <- independent_variables[[1]]
    if (length(independent_variables) > 1) {
      warning(glue::glue("Dealing with plotting >1 independent variable is not handled yet.
              The function will proceed using a simplified {dependent_variable} ~ {ind_var} formula"))
    }
    
    # Convert labels to `y` and `x``
    data <- data %>% #[all.vars(formula)]
                  mutate(x = data[, ind_var],
                         y = data[, dependent_variable])# %>%
                  # select(y, x)
    formula = "y ~ x"

    # Ensure dependent variable is ordinal (ordered factor)
    if (is.null(levels)) {
      warning("In order to determine the ordering,
               it is best to provide the `levels` parameter")
      levels = unique(as.vector(data$y))
    }
    data <- data %>% mutate(y = factor(data$y,
                                       levels = levels,
                                       ordered = TRUE))

    # Bootstrap for CIs
    pred_df <- NULL
    
    # Ordinal Regression Model
    model <- MASS::polr(formula = formula,
                        data = data,
                        Hess = TRUE,
                        method = "probit")
    
    data$predictions <- unlist(predict(model, type = "probs"))
    
    max_x  <- max(data$x)
    min_x  <- min(data$x)
    new_x <- seq(min_x, max_x, by = (max_x - min_x)/n_resolution)
    new_data <- data.frame(x = new_x)
    for(i in 1:n_bootstrap){
      # Boostrap by resampling entire dataset
      #   (prediction + residual doesn't work with ordinal data)
      data_boot <- sample_n(tbl = data,
                            size = nrow(data),
                            replace = TRUE)
  
      model_boot <- MASS::polr(formula = formula,
                               data = data_boot,
                               Hess = TRUE,
                               method = "probit")

      # Extract Bootstrapped Predictions
      new_preds <- data.frame(x = new_x,
                              y = unlist(predict(model_boot,
                                                 type="probs",
                                                 newdata = new_data)))
      # Add them to `pred_df``
      if (is.null(pred_df)) {
        pred_df <- new_preds
      }
      else {
        pred_df <- rbind(pred_df, new_preds)
      }
    }

    # Convert Probabilities to Classes
    names(pred_df) <- c("x", levels)
    pred_df$predicted_class <- levels[apply(pred_df[levels],1,which.max)]
    pred_df <- pivot_longer(data = pred_df,
                            cols = levels,
                            names_to = "class_for_probability",
                            values_to = "probability_of_class")
    
    # Convert to desired continuous or ordinal output for y value
    if (y_as_probability) {
      pred_df <- pred_df %>%
                    mutate(y = probability_of_class) %>%
                    select(-c(probability_of_class))
    }
    else {
      pred_df <- pred_df %>%
        mutate(y = predicted_class) %>%
        select(-c(predicted_class))
    }

    return(pred_df)
}



predictdf.polr <- function(model, xseq, se, level){
  pred <- predict(model, newdata = data.frame(x = xseq), type = "probs") %>%
    data.frame() %>%
    mutate( x = xseq)
  pred.df <- pivot_longer(data = pred, cols = -x, names_to = "response", values_to = "y")
}


#' @param method Smoothing method (function) to use, accepts either
#'   `NULL` or a character vector, e.g. `"lm"`, `"glm"`, `"gam"`, `"loess"`
#'   or a function, e.g. `MASS::rlm` or `mgcv::gam`, `stats::lm`, or `stats::loess`.
#'   `"auto"` is also accepted for backwards compatibility.  It is equivalent to
#'   `NULL`.
#'
#'   For `method = NULL` the smoothing method is chosen based on the
#'   size of the largest group (across all panels). [stats::loess()] is
#'   used for less than 1,000 observations; otherwise [mgcv::gam()] is
#'   used with `formula = y ~ s(x, bs = "cs")` with `method = "REML"`. Somewhat anecdotally,
#'   `loess` gives a better appearance, but is \eqn{O(N^{2})}{O(N^2)} in memory,
#'   so does not work for larger datasets.
#'
#'   If you have fewer than 1,000 observations but want to use the same `gam()`
#'   model that `method = NULL` would use, then set
#'   `method = "gam", formula = y ~ s(x, bs = "cs")`.
#' @param formula Formula to use in smoothing function, eg. `y ~ x`,
#'   `y ~ poly(x, 2)`, `y ~ log(x)`. `NULL` by default, in which case
#'   `method = NULL` implies `formula = y ~ x` when there are fewer than 1,000
#'   observations and `formula = y ~ s(x, bs = "cs")` otherwise.
#' @param se Display confidence interval around smooth? (`TRUE` by default, see
#'   `level` to control.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param span Controls the amount of smoothing for the default loess smoother.
#'   Smaller numbers produce wigglier lines, larger numbers produce smoother
#'   lines.
#' @param n Number of points at which to evaluate smoother.
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#' @section Computed variables:
#' \describe{
#'   \item{y}{predicted value}
#'   \item{ymin}{lower pointwise confidence interval around the mean}
#'   \item{ymax}{upper pointwise confidence interval around the mean}
#'   \item{se}{standard error}
#' }
#' 
#' @examples 
#' 
#' dummy_data <- data.frame(x = rep(seq(1,9),2), 
#'                    y = factor(c("Yes", "Yes", "Maybe","Yes", "Maybe","No", "Maybe","No","No",
#'                                  "No", "No", "Maybe","No", "Maybe","Yes", "Maybe","Yes","Yes"), levels = c("Yes","Maybe","No")),
#'                    covariate = c(rep("yes_no",9), rep("no_yes",9)))
#' 
#' ggplot() + stat_smooth_ordinal(data = dummy_data,
#'                  aes(x = x, response = y, colour = y), 
#'                  method = "polr", 
#'                  formula = response ~ x, se = TRUE) + 
#'                  facet_wrap(~covariate)
#' 
#'  ggplot() + stat_smooth_ordinal(data = dummy_data,
#'                  aes(x = x, response = y, linetype = y), 
#'                  method = "polr", 
#'                  formula = response ~ x, se = TRUE) + 
#'                  facet_wrap(~covariate)
#' 
#' 
#' @export
#' @rdname geom_smooth
stat_smooth_ordinal <- function(mapping = NULL, data = NULL,
                                geom = "smooth", position = "identity",
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
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothOrdinal,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      span = span,
      ...
    )
  )
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
