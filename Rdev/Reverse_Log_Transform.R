library(ggplot2)
library(xgxr)

nrep = 10
dose <- rep(c(0,200,400,800,1600),nrep)
ec50 <- 50
model <- data.frame(dose = dose, 
                   bound_receptor = .99 * dose / (dose + ec50))
data = model %>%
  mutate(bound_receptor = 1- (1-bound_receptor)*exp(rnorm(length(dose))*.3))

g = xgx_plot(data, aes(x = dose, y = bound_receptor)) + 
  geom_boxplot(aes(group = dose), outlier.shape = NULL) + 
  geom_line(data = model, alpha = 0.5) +
  geom_jitter(width = 10, height = 0, alpha = 0.5) + 
  scale_x_continuous(breaks = dose) +
  xgx_scale_y_reverselog10(minor_breaks = c(seq(0,80,10), 
                                            seq(90,99,1))/100, 
                           limits = c(0,.99), 
                           breaks = c(0, 70, 90, 97, 99)/100) + 
  labs(x = "Dose (mg)",
       y = "Receptor Occupancy (%)")
print(g)

