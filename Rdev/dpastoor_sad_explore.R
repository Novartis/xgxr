library(ggplot2)
library(dplyr)
devtools::load_all()

data = Single_Ascending_Dose_PKPD_dpastoor %>%
  group_by(ID) %>%
  mutate(DOSE = AMT[1])

g = ggplot(Single_Ascending_Dose_PKPD_dpastoor,
           aes(x=TIME,y=COBS,group=ID,color=factor(DOSE)))
g = g + geom_point()
g = g + geom_line()
g = g + xgx_scale_y_log10()
g = g + xgx_scale_x_time_units(units_dataset = "hours")
print(g)
