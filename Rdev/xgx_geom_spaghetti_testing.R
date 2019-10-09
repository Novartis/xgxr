library(ggplot2)
library(dplyr)

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


GeomSpaghetti <- ggproto("GeomSpaghetti", GeomLine,
                         required_aes = c("x", "y", "group"),
                         default_aes = aes(shape = 19, colour = "black", size = 0.5, linetype = 1, fill = NA, alpha = NA, stroke = 0.5),
                         aesthetics = function (self)
                         {
                           c(union(self$required_aes, names(self$default_aes)), self$optional_aes)
                         },
                         draw_panel = function(data, panel_scales, coord, fatten = 3) {
                           
                           if (is.null(data$y))
                             return(GeomLine$draw_panel(data, panel_scales, coord))
                           ggname("geom_spaghetti",
                                  grid::gTree(children = grid::gList(
                                    GeomLine$draw_panel(data,
                                                        panel_scales, coord),
                                    GeomPoint$draw_panel(transform(data,
                                                                   size = size * fatten),
                                                         panel_scales, coord)))
                           )
                         }
                         
)

StatSpaghetti <- ggproto("StatSpaghetti", Stat,
                         compute_group = function(data, scales) {
                           data = data %>% rename(group = ID)
                         },
                         required_aes = c("x", "y", "ID")
)

xgx_geom_spaghetti <- function(mapping = NULL, data = NULL,
                               position = "identity", ..., na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE) {
  layer(
    data = data, stat = StatSpaghetti, mapping = mapping, geom = GeomSpaghetti,  
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#Examples

N = 20

test_data <- data.frame(ID = rep(seq(1,N)), ke = (rlnorm(N,log(0.5),0.3)), vd = (rlnorm(N,log(120),0.3)) ) %>%
  merge(data.frame(TIME = seq(1,24,3))) %>%
  mutate(DV = 50*exp(-ke*TIME)/vd)

ggplot(data = test_data) + theme_bw()  + xgx_geom_spaghetti(aes(x = TIME, y = DV)) # displays error since ID not specified
ggplot(data = test_data) + theme_bw()  + xgx_geom_spaghetti(aes(x = TIME, y = DV, ID = ID))

ggplot(data = test_data, aes(x = TIME, y = DV, group = ID)) + theme_bw()  + xgx_geom_spaghetti() # displays error since ID not specified
ggplot(data = test_data, aes(x = TIME, y = DV, ID = ID)) + theme_bw()  + xgx_geom_spaghetti()
