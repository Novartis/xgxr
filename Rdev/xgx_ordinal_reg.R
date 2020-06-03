#' @importFrom brms brm
#' @importFrom brms 
#' @export
xgx_ordinal_regression_plot <- function(data = NULL,
                                        mapping = ggplot2::aes(),
                                        ...,
                                        environment = parent.frame(),
                                        formula = "dependent_variable ~ independent_variable",
                                        levels = NULL,
                                        ci = 0.975,
                                        n_sampled_pts = 1000) {

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
    
    data[,dependent_variable] <- factor(data[,dependent_variable],
                                      levels = levels,
                                      ordered = TRUE)
  }
  
  # Ordinal Regression Model
  model <-  brms::brm(formula = formula,
                      family = brms::cumulative("logit", threshold="flexible"),
                      data = data,
                      refresh = 0)

  # Bootstrap with fitted_draws for CI
  pframe <- list()
  for (var_ix in 1:length(independent_variables)) {
    mi = min(data[[independent_variables[var_ix]]])
    ma = max(data[[independent_variables[var_ix]]])
    seqs <- seq(mi, ma, abs(ma-mi)/n_sampled_pts)

    pframe[[var_ix]] <- seqs
  }
  pframe <- data.frame(do.call("cbind", pframe))
  pframe <- setNames(pframe, independent_variables)
  
  predictions <- tidybayes::fitted_draws(model = model,
                                         newdata = pframe,
                                         category ="Response",
                                         value = "Value") %>%
                            ungroup() %>%
                            select(independent_variables, Response, Value)

  # Plot
  gg <- ggplot(data = predictions,
               mapping = mapping,
               ...,
               environment = environment)
  
  gg <- gg + xgx_geom_pi(percent_level = 0.975)
  gg <- gg + xgx_scale_x_log10()
  gg <- gg + labs(y = "Class Probability")
  gg <- gg + xgx_annotate_status(status)
  gg <- gg + xgx_theme()
  
  return(gg)
}

# xgx_ordinal_regression_plot(data = X,
#                             aes(x = CONC, y = Severity_label),
#                             formula = "Severity_label ~ log(CONC)",
#                             dependent_variable = "Severity_label",
#                             independent_variables = c(CONC),
#                             labels = c("Mild", "Moderate", "Severe"))