library(ggplot2)
library(dplyr)
devtools::load_all()

data = sad_pkpd %>%
  group_by(ID) %>%
  mutate(DOSE = AMT[1])

g = ggplot(sad_pkpd,
           aes(x=TIME,y=COBS,group=ID,color=factor(DOSE)))
g = g + geom_point()
g = g + geom_line()
g = g + xgx_scale_y_log10()
g = g + xgx_scale_x_time_units(units_dataset = "hours")
print(g)
