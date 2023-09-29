library(ggplot2)
library(xgxr)
xgx_theme_set()

tstop = 90
data <- data.frame(x = tmult*(0:tstop), y = rnorm(tstop+1))
g = ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point() +
  xgx_scale_x_time_units(units_dataset = "minute", units_plot = "minute")
print(g)
