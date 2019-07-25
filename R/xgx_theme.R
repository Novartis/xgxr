#' Calls the standard theme for xGx graphics
#'
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @export
xgx_theme <- function() {
  minor.color = "grey83"
  major.color = "grey83"
  
  ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid.minor.x=ggplot2::element_line(color=minor.color),
          panel.grid.minor.y=ggplot2::element_line(color=minor.color),
          panel.grid.major.x=ggplot2::element_line(color=major.color),
          panel.grid.major.y=ggplot2::element_line(color=major.color))
}