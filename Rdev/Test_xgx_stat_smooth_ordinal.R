# Testing xgx_stat_smooth for ordinal data with different formats for the ordinal categories
# e.g. different symbols

# example with ordinal data (method = "polr")
set.seed(12345)
data = data.frame(x = 120*exp(stats::rnorm(100,0,1)),
                  response = sample(c(1, 2, 3), 100, replace = TRUE),
                  covariate = sample(c("Male","Female"), 100, replace = TRUE)) %>%
  dplyr::mutate(y = (50 + 20*x/(200 + x))*exp(stats::rnorm(100, 0, 0.3)),
                response_2 = runif(100)) %>%
  dplyr::mutate(response_2 = case_when(response_2 > 0.9 ~ "> 0.9",
                                       response_2 > 0.8 ~ "< 0.9",
                                       response_2 > 0.6 ~ "6",
                                       response_2 >= 0.5 ~ ">= 50%",
                                       response_2 > 0.4 ~ "test\nline break",
                                       TRUE ~ "!@#$%^&*(){}~`-_=+<>,.?/"))

# example coloring by the response categories
xgx_plot(data = data) +
  xgx_stat_smooth(mapping = ggplot2::aes(x = x, response = response_2,
                                         colour = response_2, fill = response_2),
                  method = "polr") +
  ggplot2::scale_y_continuous(labels = scales::percent_format())

# example coloring by the response categories
xgx_plot(data = data) +
  xgx_stat_smooth(mapping = ggplot2::aes(x = x, response = response,
                                         colour = factor(response), fill = factor(response)),
                  method = "polr") +
  ggplot2::scale_y_continuous(labels = scales::percent_format())

