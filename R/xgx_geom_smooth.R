#' 
#' 
#' 
#' @inheritParams xgx_stat_smooth
#' @export
#' 
xgx_geom_smooth <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            method = NULL,
                            formula = NULL,
                            se = TRUE,
                            n = 80,
                            span = 0.75,
                            fullrange = FALSE,
                            level = 0.95,
                            method.args = list(),
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  
  return(list(xgx_stat_smooth(mapping = mapping, 
                          data = data, 
                          geom = geom,
                          position = position,
                          method = method,
                          formula = formula,
                          se = se,
                          n = n,
                          span = span,
                          fullrange = fullrange,
                          level = level,
                          method.args = method.args,
                          na.rm = na.rm,
                          orientation = orientation,
                          show.legend = show.legend,
                          inherit.aes = inherit.aes,
                          ...)))
}

