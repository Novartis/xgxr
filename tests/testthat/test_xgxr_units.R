library(xgxr)
context("xgxr units check")

test_that("xgxr log-breaks works with units package", {
    tmp <- units::set_units(c(0.01,10),"mg/ml");
    expect_equal(xgx_breaks_log10(.tmp), units::set_units(c(0.01, 0.10, 1.00, 10.00), "mg/mL"))
})
