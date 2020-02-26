library(xgxr)
context("xgxr units check")

test_that("xgxr log-breaks works with units package", {
    tmp <- units::set_units(c(0.01, 10), "mg/ml");
    expect_equal(xgx_breaks_log10(tmp), units::set_units(c(0.01, 0.10, 1.00, 10.00), "mg/mL"))
})

for (u in c("h", "d", "s")) {
    test_that(sprintf("xgxr time-breaks works with units package (also detects units, %s)", u), {
        expect_equal(xgx_breaks_time(units::set_units(c(0, 5), u, mode="standard")),
                     units::set_units(xgx_breaks_time(c(0, 5), u), u, mode="standard"))
    })
}

test_that("xgxr time-breaks week", {
    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "weeks")),
                 units::set_units(xgx_breaks_time(c(0, 5), "w"), "weeks"))
    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "week")),
                 units::set_units(xgx_breaks_time(c(0, 5), "w"), "week"))
})


test_that("xgxr time-breaks years", {
    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "years")),
                 units::set_units(xgx_breaks_time(c(0, 5), "y"), "years"))

    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "year")),
                 units::set_units(xgx_breaks_time(c(0, 5), "y"), "year"))
})

test_that("xgxr time-breaks month", {
    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "months")),
                 units::set_units(xgx_breaks_time(c(0, 5), "m"), "months"))
    expect_equal(xgx_breaks_time(units::set_units(c(0, 5), "month")),
                 units::set_units(xgx_breaks_time(c(0, 5), "m"), "month"))
})


## Now test plots

library(dplyr)
library(units)
library(ggplot2)

# flag for labeling figures as draft
status <- "DRAFT"
pkpd_data <- case1_pkpd %>%
  arrange(DOSE) %>%
  select(-IPRED) %>%
  mutate(TRTACT_low2high = factor(TRTACT, levels = unique(TRTACT)),
         TRTACT_high2low = factor(TRTACT, levels = rev(unique(TRTACT))),
         DAY_label = paste("Day", PROFDAY),
         DAY_label = ifelse(DAY_label == "Day 0","Baseline",DAY_label))

LOQ = 0.05 #ng/ml
dose_max = as.numeric(max(pkpd_data$DOSE))
pk_data <- pkpd_data %>%
  filter(CMT == 2) %>%
  mutate(LIDVNORM = LIDV / as.numeric(DOSE))
pk_data_cycle1 <- pk_data %>%
    filter(CYCLE == 1)

time_units_dataset <- "hours"
time_units_plot    <- "days"
trtact_label       <- "Dose"
dose_label         <- "Dose (mg)"
conc_label         <- "Concentration (ng/ml)"
auc_label          <- "AUCtau (h.(ng/ml))"
concnorm_label     <- "Normalized Concentration (ng/ml)/mg"
sex_label          <- "Sex"
w100_label         <- "WEIGHTB>100"
pd_label           <- "FEV1 (mL)"
cens_label         <- "Censored"

p1 <- ggplot(data = pk_data_cycle1, aes(x     = NOMTIME,
                                        y     = LIDV,
                                        group = DOSE,
                                        color = TRTACT_high2low)) +
    xgx_geom_ci(conf_level = 0.95) +
    xgx_scale_y_log10() +
    xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
    labs(y = conc_label, color = trtact_label) +
    xgx_annotate_status(status)

vdiffr::expect_doppelganger("ci-base", p1)

pk_data_cycle1$NOMTIME <- set_units(pk_data_cycle1$NOMTIME, "hr")

p1 <- ggplot(data = pk_data_cycle1, aes(x     = NOMTIME,
                                        y     = LIDV,
                                        group = DOSE,
                                        color = TRTACT_high2low)) +
    xgx_geom_ci(conf_level = 0.95) +
    xgx_scale_y_log10() +
    xgx_scale_x_time_units() +
    labs(y = conc_label, x="Time", color = trtact_label) +
    xgx_annotate_status(status)

vdiffr::expect_doppelganger("ci-base-unit-y", p1)

p3 <- ggplot(data = pk_data_cycle1 %>% filter(drop_units(NOMTIME) > 0),
             aes(x     = NOMTIME,
                 y     = LIDV,
                 group = DOSE,
                 color = TRTACT_high2low)) +
    xgx_geom_ci(conf_level = 0.95) +
    xgx_scale_y_log10() +
    xgx_scale_x_log10(units=TRUE) +
    labs(y = conc_label, x="Time", color = trtact_label) +
    xgx_annotate_status(status)

vdiffr::expect_doppelganger("ci-base-unit-y-unit-x", p3)


pk_data_cycle1$LIDV <- set_units(pk_data_cycle1$LIDV, "ng/ml")

p4 <- ggplot(data = pk_data_cycle1 %>% filter(drop_units(NOMTIME) > 0),
             aes(x     = NOMTIME,
                 y     = LIDV,
                 group = DOSE,
                 color = TRTACT_high2low)) +
    xgx_geom_ci(conf_level = 0.95) +
    xgx_scale_y_log10(units=TRUE) +
    xgx_scale_x_time_units() +
    labs(y = "Concentration", x="Time", color = trtact_label) +
    xgx_annotate_status(status)


vdiffr::expect_doppelganger("ci-base-unit-ly-unit-lx", p4)
