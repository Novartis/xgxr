.onLoad <- function(...) {
  vctrs::s3_register("stats::predict", "nls")
  vctrs::s3_register("ggplot2::predictdf","nls")
  vctrs::s3_register("ggplot2::predictdf","polr")
}
