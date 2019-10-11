#' Sets the standard theme for xGx graphics
#'
#' xgx_theme_set
#' 
#' @return xgx ggplot2 compatible theme
#' 
#' @examples 
#' conc <- 10^(seq(-3, 3, by = 0.1))
#' ec50 <- 1
#' data <- data.frame(concentration = conc, 
#'                    bound_receptor = 1 * conc / (conc + ec50))
#' xgx_theme_set()
#' ggplot2::ggplot(data, ggplot2::aes(y = concentration, x = bound_receptor)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_line() +
#'   xgx_scale_y_log10() +
#'   xgx_scale_x_reverselog10()
#'   
#' @importFrom ggplot2 theme_set
#' @export
xgx_theme_set <- function() {
  ggplot2::theme_set(xgx_theme())
}
