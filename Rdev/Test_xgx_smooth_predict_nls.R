library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
load_all()

xgx_theme_set()

# Load Dataset ---
pkpd_data <- read.csv("../data_create/raw/ro_bcma.csv") %>%
  arrange(DOSE) %>%
  mutate(TRTACT = paste(DOSE, "ug/kg"),
         TRTACT_low2high = factor(TRTACT, levels = unique(TRTACT)), 
         TRTACT_high2low = factor(TRTACT, levels = rev(unique(TRTACT)))) %>%
  na.omit()

# Plot Data ----
g = ggplot(data = pkpd_data, aes(x = CTOT, y = T_FREEFRAC))
g = g + geom_point(aes(color = TRTACT_high2low))
g = g + xgx_scale_x_log10()
g = g + xgx_scale_y_log10()
print(g)
gdata = g

# xgx_stat_smooth_emax ----
# THIS GIVES A FUNNY LOOKING PLOT
g = gdata
g = g + xgx_geom_smooth_emax(se = FALSE)
print(g)

# xgx_stat_smooth - customize ----

# THIS GIVES AN ERROR
# Computation failed in `stat_smooth()`:
# Argument 1 must have names. 
g = gdata
g = g + xgx_stat_smooth(method = "nlsLM",
                        formula = y ~ 1 - x/(exp(logED50) + x),
                        method.args = list(
                          start = list(logED50 = log(1)),
                          lower = c(-Inf)
                        ), se = TRUE)
g = g + xgx_scale_y_log10(limits = (c(0.01,2)))
print(g)

# predict.nls ----
# THIS GIVES AN ERROR
# Error in predict.nls(mod) : could not find function "predict.nls"
binding_data = pkpd_data %>%
  mutate(Kd = 0.04,
         Ctot = CTOT,
         Tfreefrac = T_FREEFRAC)

Tfrac_formula = Tfreefrac ~ (-(Ctot - Ttot + Kd) + sqrt((Ctot - Ttot + Kd)^2 + 4 * Ttot * Kd))/ (2 * Ttot)
mod = minpack.lm::nlsLM(Tfrac_formula, data = binding_data, start = list(Ttot = 1))
broom::tidy(mod)

pred = predict.nls(mod)

