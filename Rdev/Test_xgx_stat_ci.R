
library(gridExtra)

library(grid)

library(ggplot2)

library(dplyr)

library(RxODE)

library(caTools)

library(xgxr, lib.loc = "../Rlib")

gg <- ggplot(data = iris, 
             aes(x = Sepal.Length, y = Sepal.Width, color = Species))
gg <- gg + geom_line()
gg <- gg + geom_point()
gg

gg <- ggplot(data = iris, 
             aes(x = Sepal.Length, y = Sepal.Width, color = Species))
gg <- gg + geom_line()
gg <- gg + xgx_stat_ci()
gg

gg <- ggplot(data = iris, 
             aes(x = Sepal.Length, y = Sepal.Width, color = Species))
gg <- gg + geom_line()
gg <- gg + geom_point()
gg