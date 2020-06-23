
library(ggplot2)
library(dplyr)
library(tidyr)
library(minpack.lm)
library(xgxr)

Nsubj <- 10
Doses <- c(0, 25, 50, 100, 200)
Ntot <- Nsubj*length(Doses)
times <- c(0,14,30,60,90)

dat1 <- data.frame(ID = 1:(Ntot),
                   DOSE = rep(Doses, Nsubj),
                   PD0 = rlnorm(Ntot, log(100), 1),
                   Kout = exp(rnorm(Ntot,-2, 0.3)),
                   Imax = 1,
                   ED50 = 25) %>%
  dplyr::mutate(PDSS = PD0*(1 - Imax*DOSE/(DOSE + ED50))*exp(rnorm(Ntot, 0.05, 0.3))  ) %>%
  merge(data.frame(ID = rep(1:(Ntot), each = length(times)), Time = times), by = "ID") %>%
  dplyr::mutate(PD = ((PD0 - PDSS)*(exp(-Kout*Time)) + PDSS),
                PCHG = (PD - PD0)/PD0)

gg <- ggplot2::ggplot(dat1 %>% subset(Time == 90),
                      ggplot2::aes(x = DOSE, y = PCHG)) +
  ggplot2::geom_boxplot(aes(group = DOSE)) +
  xgx_theme() +
  xgx_scale_y_percentchangelog10() +
  ylab("Percent Change from Baseline") +
  xlab("Dose (mg)")

gg + xgx_geom_smooth_emax()


gg +
  geom_smooth(method = "nls", formula = y ~ E0 + Emax*x/(ED50 + x),
              method.args = list(start = list(Emax = -0.50, ED50 = 25, E0 = 0)),
              se = TRUE)


## This one fails because of zero residual data
ggplot(data = data.frame(x = seq(0,10), y = 20 + 50*seq(0,10)/(seq(0,10) + 5)),
       aes(x = x, y = y))  +
  xgx_theme() +
  geom_point() +
  geom_smooth(method = "nls", se = TRUE,formula = y ~ E0 + Emax*x/(exp(logED50) + x),
              method.args = list(start = list(Emax = 50, logED50 = log(5), E0 = 21)))
# Warning message:
#   Computation failed in `stat_smooth()`:
#   number of iterations exceeded maximum of 50 


## This one works because it uses nlsLM which is ok with zero residual data
ggplot(data = data.frame(x = seq(0,10), y = 20 + 50*seq(0,10)/(seq(0,10) + 5)),
       aes(x = x, y = y)) +
  xgx_theme() +
  geom_point() +
  geom_smooth(method = "nlsLM", se = TRUE,formula = y ~ E0 + Emax*x/(exp(logED50) + x),
              method.args = list(start = list(Emax = 50, logED50 = log(5), E0 = 21)))

## This one fails due to singular gradient
ggplot(data = data.frame(x = seq(0,10), y = 20 + 5*seq(0,10)),
       aes(x = x, y = y)) +
  xgx_theme() +
  geom_point() +
  geom_smooth(method = "nls", se = FALSE,formula = y ~ E0 + Emax*x/(ED50 + x),
              method.args = list(start = list(Emax = -1000, ED50 = -25, E0 = 20)))
# Warning message:
#   Computation failed in `stat_smooth()`:
#   singular gradient 

## nlsLM also fails with this data because of singular gradient
ggplot(data = data.frame(x = seq(0,10), y = 20 + 5*seq(0,10)),
       aes(x = x, y = y)) +
  xgx_theme() +
  geom_point() +
  geom_smooth(method = "nlsLM", se = FALSE,formula = y ~ E0 + Emax*x/(ED50 + x),
              method.args = list(start = list(Emax = -1000, ED50 = -25, E0 = 20)))

# Warning messages:
#   1: In nls.lm(par = start, fn = FCT, jac = jac, control = control, lower = lower,  :
#                  lmdif: info = -1. Number of iterations has reached `maxiter' == 50.
# 
# 2: Computation failed in `stat_smooth()`:
# singular gradient matrix at initial parameter estimates 







