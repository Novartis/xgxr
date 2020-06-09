########################################## Fariba's code to generate Emax regression  #############################
########################################### line with CI via bootstrapping            #############################
library(ggplot2)
library(dplyr)
library(tidyr)
library(minpack.lm)
library(xgxr)
########################################## Dataset copied from Alison's Emax code #################################
################################################################################################################### 
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

################ nonlinear Emax regression model, not sure if I got it from ######################################
################ Andy or found it somewhere online few years ago            ######################################

bpc <- as.numeric(dat1$PCHG)
dose <- dat1$DOSE

B <- 1000
pred_mat <- matrix(0, nrow =NROW(dat1) , ncol = B) # initialize matrix
model1 <- nls(formula=bpc ~ E0 + Emax*dose/(ED50 + dose),
              start   = c(Emax = 50, ED50 = 5, E0 = 21))
resids1 <- residuals(model1)
preds1 <- predict(model1)
dat1$Pred <- preds1

for(i in 1:B){
  # bootstrapped dependent variable
  new_y <- preds1 + sample(resids1,replace = TRUE)
  dat1$NewY <- new_y
  # fit model
  Model_Boot <- nls(formula = NewY ~ E0 + Emax*DOSE/(ED50 + DOSE),
                    data    = dat1,
                    start   = c(Emax = 50, ED50 = 5, E0 = 21))

  # extract predictions
  pred_mat[,i] <- predict(Model_Boot)
}
#add 2.5% and 97.5% percentile intervals to regression dataset
regression <- cbind(dat1, t(apply(pred_mat, 1, FUN = function(x) quantile(x, c(.025, .975)))))
# rename the last two columns of the dataset
names(regression)[names(regression) %in% c("2.5%","97.5%")]<-c("Lower","Upper")

#ggplot with nls fitting
g = ggplot(data= regression, aes(x= DOSE))
g = g + geom_boxplot(aes(y = PCHG, group = DOSE))
g = g + geom_line(aes(y=predict(model1)), col="blue")
g = g + xgx_theme() 
g = g + geom_ribbon(aes(ymin = Lower, ymax = Upper),
                    alpha = .2, fill = 'gray')
g = g + xgx_scale_y_percentchangelog10()
g = g + ylab("Percent change from bseline (%)")
g = g + xlab("Dose(mg)")
g
##################################################################################################################
##################################################################################################################