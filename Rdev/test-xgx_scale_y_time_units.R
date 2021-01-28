context("scale x and y units")

set.seed(100)
data <- data.frame(x = 1:1000, y = rnorm(1000))

p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point() +
  xgx_scale_x_time_units(units_dataset = "hours", units_plot = "weeks")
vdiffr::expect_doppelganger("xgx_scale_x_time_units", p)

p <- ggplot2::ggplot(data = data, ggplot2::aes(x = y, y = x)) +
  ggplot2::geom_point() +
  xgx_scale_y_time_units(units_dataset = "hours", units_plot = "weeks")

vdiffr::expect_doppelganger("xgx_scale_y_time_units", p)

