#' @importFrom brms brm
#' @importFrom brms 
#' @export
xgx_ordinal_regression_plot <- function(data = NULL,
                                        formula = "dependent_variable ~ independent_variable",
                                        levels = NULL,
                                        nbins = 6,
                                        colors = c("red", "green", "blue"),
                                        ci = 0.95) {

  # Prepare variables and ensure correct datatypes
  if (class(formula) == "character"){
    formula <- as.formula(formula)
  }

  independent_variables <- tail(all.vars(formula), -1)
  dependent_variable <- all.vars(formula)[1]

  # Ensure Ordinal Values for dependent variable
  if (!lapply(data, is.ordered)[[dependent_variable]]) {
    if (is.null(levels)) {
      warning("In order to determine the ordering, it is best to provide the `levels` parameter")
      levels = unique(as.vector(data[,dependent_variable]))
    }
  }
  data <- data %>% mutate(Response = factor(data[, dependent_variable],
                                            levels = levels,
                                            ordered = TRUE))


  # Bootstrapping script from Fariba/Andy
  B <- 100
  pred_df <- NULL
  # Ordinal Regression Model
  model <- MASS::polr(formula = formula,
                      data = data,
                      Hess = TRUE,
                      method = "probit")
  
  data$Pred <- unlist(predict(model, type = "probs"))

  for(i in 1:B){
    # Boostrap by resampling entire dataset
    #   (prediction + residual doesn't work with ordinal data)
    data_boot <- sample_n(tbl = data,
                          size = nrow(data),
                          replace = TRUE)
    
    model_boot <- MASS::polr(formula = formula,
                             data = data,
                             Hess = TRUE, method = "probit")
    # extract predictions
    new_preds <- data.frame(x = data_boot$CONC,
                            y = unlist(predict(model_boot, type="probs")))
    if (is.null(pred_df)) {
      pred_df <- new_preds
    }
    else {
      pred_df <- rbind(pred_df, new_preds)
    }
  }
  
  response_classes = paste0("y.", levels)
  pred_df$pred <- response_classes[apply(pred_df[response_classes],1,which.max)]
  pred_df <- pivot_longer(data = pred_df, cols = response_classes)
 

  gg <- ggplot(pred_df,
               aes_string(x = "x",
                          y = "value",
                          color = "name",
                          fill = "name"))

  # Plot confidence interval ribbon for fitted draw predictions
  gg <- gg + stat_summary(geom = "ribbon")

  # Plot confidence intervals for observed data binned by quantiles
  # gg <- gg + xgx_stat_ci(data = data,
  #                        aes(x=independent_variables[1],
  #                            color = Response),
  #                        bins = nbins,
  #                        distribution = "ordinal")

  # Clean up remaining aesthetics of plot
  gg <- gg + xgx_scale_x_log10()
  gg <- gg + labs(x = independent_variables[1],
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
