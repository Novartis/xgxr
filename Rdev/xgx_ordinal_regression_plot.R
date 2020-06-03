#' @importFrom brms brm
#' @importFrom brms 
#' @export
xgx_ordinal_regression_plot <- function(data = NULL,
                                        formula = "dependent_variable ~ independent_variable",
                                        levels = NULL,
                                        nbins = 6,
                                        colors = c("red", "green", "blue"),
                                        ci = 0.975) {

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
      levels = unique(data[,dependent_variable])
    }
  }
  data <- data %>% mutate(Response = factor(data[, dependent_variable],
                                            levels = levels,
                                            ordered = TRUE))

  # Ordinal Regression Model
  model <-  brms::brm(formula = formula,
                      family = brms::cumulative("logit", threshold="flexible"),
                      data = data,
                      refresh = 0)

  # Bootstrap with fitted_draws for CI
  n_steps = 100
  pframe <- list()
  for (var_ix in 1:length(independent_variables)) {
    mi = min(data[[independent_variables[var_ix]]])
    ma = max(data[[independent_variables[var_ix]]])
    seqs <- seq(mi, ma, abs(ma-mi)/n_steps)

    pframe[[var_ix]] <- seqs
  }
  pframe <- data.frame(do.call("cbind", pframe))
  pframe <- setNames(pframe, independent_variables)

  predictions <- tidybayes::fitted_draws(model = model,
                                         newdata = pframe,
                                         n = 20,
                                         category ="Response",
                                         value = "Value") %>%
                            ungroup() %>%
                            select(independent_variables, Response, Value)
  
  
  
  
  # Dataframe with renamed columns for alignment
  pred_probabilities <- predict(model)%>%
                                as.data.frame %>%
                                setNames(levels(predictions$Response))
  pred_labels = colnames(pred_probabilities)[apply(pred_probabilities, 1, which.max)]

  # Store all data and predictions together
  ord_reg_prediction_data <- data %>%
                            cbind(predict(model)) %>%
                            mutate(predicted_label = factor(pred_labels,
                                                              levels = levels(data$Response),
                                                              ordered = TRUE)) %>%
                            # Add one-hot vectors
                            cbind(dummies::dummy.data.frame(data, 
                                                            names = "Response",
                                                            all = FALSE))
                          
  print(ord_reg_prediction_data)
  gg <- ggplot(predictions,
               aes_string(x = independent_variables[1], y = "Value",
                   color = "Response",
                   fill = "Response"))

  # Plot confidence interval ribbon for fitted draw predictions
  gg <- gg + xgx_geom_pi(percent_level = 0.975, geom = "ribbon")
  
  # Plot confidence intervals for observed data binned by quantiles
  response_cols = paste("Response", levels(predictions$Response), sep = "")
  for (ix in 1:length(response_cols)){
    color <- colors[[ix]]
    response_col <- response_cols[[ix]]
    
    breaks <- quantile(x = ord_reg_prediction_data[[independent_variables[1]]], seq(0,1,1/nbins))
    gg <- gg + stat_summary_bin(data = ord_reg_prediction_data,
                                show.legend = TRUE,
                                fun.data = binom_ci,
                                aes_string(x = independent_variables[1],
                                           y = response_col),
                                color = color,
                                # shape = "square",
                                fill = color,
                                breaks = breaks)
  }
  
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