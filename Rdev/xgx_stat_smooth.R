#' Wrapper for stat_smooth that also can deal with class (ordinal, multinomial, or binary variables)
xgx_stat_smooth <- function(mapping = NULL,
                        data = NULL,
                        conf_level = 0.95,
                        
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

  lays <- list()

  # Assume OLS / LM / nls / nlsLM / glm etc. model
  ggproto_stat <- StatSmooth

  # Class Model
  if (is.null(method)){ }
  else{
    if (method %in% c("polr")) {
      ggproto_stat <- StatSmoothOrdinal
    }
  }
  
  # Default parameters
  gg_params = list(method = method,
                  formula = formula,
                  se = se,
                  n = n,
                  fullrange = fullrange,
                  level = level,
                  na.rm = na.rm,
                  orientation = orientation,
                  method.args = method.args,
                  span = span)
  

  for (igeom in geom) {
    lay = layer(
      stat = ggproto_stat,
      data = data,
      mapping = mapping,
      geom = igeom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = gg_params
    )
    
    lays[[paste0("geom_", igeom)]] <- lay  
  }
  
  return(lays)
}
