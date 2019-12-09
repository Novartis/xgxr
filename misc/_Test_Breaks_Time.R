devtools::load_all()

units_range = c("year","month","day","hours")
tmax_range  = 3:1000

result = list()
i=0
for (units in units_range) {
  for (tmax in tmax_range) {
    breaks = xgx_breaks_time(c(0,tmax),units)
    i=i+1
    result[[i]] = data.frame(
      units = units,
      tmax = tmax,
      all_integer = all(round(breaks)==breaks)
    )
  }
}

results = bind_rows(result)

results %>%
  filter(!(all_integer)) %>%
  knitr::kable()

if (sum(!results$all_integer)==0) {
  message("SUCCESS: all breaks are integers")
}
  