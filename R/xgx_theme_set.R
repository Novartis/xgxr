#' Sets the standard theme for xGx graphics
#'
#' xgx_theme_set
#' 
#' @return xgx ggplot2 compatible theme
#' 
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @export
xgx_theme_set <- function() {
  ggplot2::theme_set(ggplot2::theme_bw())
}
