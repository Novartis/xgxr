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

  gg <- ggplot(data = pred_df,
               aes_string(x = "x",
                          y = "y",
                          color = "class_for_probability",
                          fill = "class_for_probability"))
  
  # Plot confidence interval ribbon for fitted draw predictions
  gg <- gg + stat_smooth_ordinal(aes(response = class_for_probability))

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
                                              levels = NULL,
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
                               method = "logit")

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



