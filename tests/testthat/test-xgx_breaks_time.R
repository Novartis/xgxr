test_that("xgx_breaks_time() allows user to set a preferred number of breaks", {
  expect_identical(xgx_breaks_time(c(0, 100), "hour"), # default 5 breaks
                   seq(0, 96, by = 24))
  expect_identical(xgx_breaks_time(c(0, 100), units_plot = "hour", number_breaks = 5),
                   seq(0, 96, by = 24))
  expect_identical(xgx_breaks_time(c(0, 100), units_plot = "hour", number_breaks = 10),
                   seq(0, 96, by = 12))
})
