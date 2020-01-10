#' Plot data with median and percent intervals
#'
#' @inheritParams xgx_stat_pi
#' @return ggplot2 plot layer
#'
#' @examples 
#' data <- data.frame(x = rep(c(1, 2, 3), each = 20),
#'                    y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60))
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) + 
#'   xgx_geom_pi(percent_level = 0.95)
#'  
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom stats rnorm
#' @export
xgx_geom_pi <- function(mapping = NULL, data = NULL, percent_level = 0.95,
                        geom = list("line", "ribbon"),
                        position = "identity",
                        fun.args = list(),
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {
  return(list(xgx_stat_pi(mapping, data, percent_level,
                          geom,
                          position,
                          fun.args,
                          na.rm,
                          show.legend,
                          inherit.aes,
                          ...)))
}
