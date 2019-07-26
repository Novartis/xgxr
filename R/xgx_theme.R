#' Calls the standard theme for xGx graphics
#'
#' @return xgx ggplot2 compatible theme
#' 
#' @examples 
#' conc <- 10^(seq(-3, 3, by = 0.1))
#' EC50 <- 1
#' data <- data.frame(concentration = conc, 
#'                    bound_receptor = 1 * conc / (conc + EC50))
#' ggplot2::ggplot(data, ggplot2::aes(y = concentration, x = bound_receptor)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_line() +
#'   xgx_scale_y_log10() +
#'   xgx_scale_x_reverselog10() +
#'   xgx_theme()
#'   
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @export
xgx_theme <- function() {
  minor.color <- "grey83"
  major.color <- "grey83"

  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_line(color = minor.color),
      panel.grid.minor.y = ggplot2::element_line(color = minor.color),
      panel.grid.major.x = ggplot2::element_line(color = major.color),
      panel.grid.major.y = ggplot2::element_line(color = major.color))
}
