library(xgxr)
context("xgxr units check")

test_that("xgxr log-breaks works with units package", {
    tmp <- units::set_units(c(0.01,10),"mg/ml");
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
