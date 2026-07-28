# Regression tests for GitHub issue #72
# https://github.com/Novartis/xgxr/issues/72
#
# `breaks` are supplied by the user on the scale of the original data, but by
# the time the data reaches the stat it has already been transformed by the
# position scale.  The stat must therefore transform the breaks to match.
#
# The invariant exercised below is that binning the same data with the same
# breaks must put the same observations in the same bins, and so produce the
# same summary statistics, whether or not a log10 x scale is in use.  Only the
# x positions are allowed to differ, because they are reported on the scale of
# the axis.

skip_if_not_installed("ggplot2")

library(ggplot2)

make_binary_data <- function() {
  set.seed(1234)
  data.frame(
    x = c(stats::runif(300, 0.4, 2.8), stats::runif(142, 2.8, 5.2),
          stats::runif(25, 5.2, 7.6), stats::runif(3, 7.6, 10)),
    y = stats::rbinom(470, 1, 0.3))
}

make_ordinal_data <- function() {
  set.seed(5678)
  data.frame(
    x = 10^stats::runif(400, -0.5, 1),
    response = factor(sample(c("Mild", "Moderate", "Severe"), 400,
                             replace = TRUE)))
}

test_that("xgx_stat_ci(breaks) bins identically with and without a log x scale", {
  df <- make_binary_data()
  breaks <- stats::quantile(df$x)

  base <- ggplot(df, aes(x = x, y = y)) +
    xgx_stat_ci(breaks = breaks, geom = "point")

  linear <- layer_data(base)
  logged <- layer_data(base + xgx_scale_x_log10())

  # All four bins survive the log scale (before the fix only one did)
  expect_equal(nrow(linear), length(breaks) - 1)
  expect_equal(nrow(logged), nrow(linear))
  expect_false(any(is.na(logged$y)))

  # Identical membership means identical summary statistics
  expect_equal(logged$y, linear$y)
  expect_equal(logged$ymin, linear$ymin)
  expect_equal(logged$ymax, linear$ymax)

  # x is reported on the scale of the axis.  This stat places each point at the
  # midpoint of its bin, so on a log10 axis that is the geometric, rather than
  # the arithmetic, midpoint of the break pair.
  expect_equal(10^logged$x,
               unname(sqrt(head(breaks, -1) * tail(breaks, -1))))
})

test_that("xgx_stat_ci(breaks) is unaffected by coord_transform", {
  # coord_transform does not transform the data before it reaches the stat, so
  # the breaks must be left alone in that case.
  df <- make_binary_data()
  breaks <- stats::quantile(df$x)

  base <- ggplot(df, aes(x = x, y = y)) +
    xgx_stat_ci(breaks = breaks, geom = "point")

  # coord_trans() was renamed coord_transform() in ggplot2 4.0.0
  coord_fn <- if (utils::packageVersion("ggplot2") >= "4.0.0") {
    ggplot2::coord_transform
  } else {
    ggplot2::coord_trans
  }

  expect_equal(layer_data(base + coord_fn(x = "log10")), layer_data(base))
})

test_that("xgx_stat_ci(bins) is unaffected by a log x scale", {
  # bins are computed from the data itself, so they were always correct; this
  # guards against the fix regressing them.
  df <- make_binary_data()

  base <- ggplot(df, aes(x = x, y = y)) +
    xgx_stat_ci(bins = 4, geom = "point")

  expect_equal(layer_data(base + xgx_scale_x_log10())$y,
               layer_data(base)$y)
})

test_that("ordinal xgx_stat_ci(breaks) bins identically with a log x scale", {
  df <- make_ordinal_data()
  breaks <- stats::quantile(df$x)

  base <- ggplot(df, aes(x = x, response = response, colour = response)) +
    xgx_stat_ci(distribution = "ordinal", breaks = breaks, geom = "point")

  linear <- suppressMessages(layer_data(base))
  logged <- suppressMessages(layer_data(base + xgx_scale_x_log10()))

  n_response <- nlevels(df$response)
  expect_equal(nrow(linear), (length(breaks) - 1) * n_response)
  expect_equal(nrow(logged), nrow(linear))

  # Compare on a stable ordering, since the row order is not guaranteed
  key <- function(d, x) d[order(x, d$colour), c("y", "ymin", "ymax")]
  expect_equal(key(logged, 10^logged$x), key(linear, linear$x),
               ignore_attr = TRUE)

  # The ordinal stat reports the median x of each bin.  That is the median of
  # the transformed data, which for an even-sized bin is the geometric rather
  # than the arithmetic mean of the two central values, so the back-transformed
  # positions are close to but not identical to the untransformed medians.
  # What must hold is that every point still lands inside its own bin.
  x_logged <- sort(unique(10^logged$x))
  expect_length(x_logged, length(breaks) - 1)
  expect_true(all(x_logged > head(breaks, -1) & x_logged < tail(breaks, -1)))
  expect_equal(x_logged, sort(unique(linear$x)), tolerance = 1e-3)
})

test_that("ordinal xgx_stat_ci(breaks) includes the lowest observation", {
  # cut() without include.lowest = TRUE drops the minimum value into an NA bin,
  # which was then drawn as a spurious extra point.
  df <- make_ordinal_data()
  breaks <- stats::quantile(df$x)

  d <- suppressMessages(layer_data(
    ggplot(df, aes(x = x, response = response, colour = response)) +
      xgx_stat_ci(distribution = "ordinal", breaks = breaks, geom = "point")))

  expect_false(any(is.na(d$x)))
  expect_equal(length(unique(d$x)), length(breaks) - 1)
})
