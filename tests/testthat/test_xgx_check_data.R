library(xgxr)
context("Data Check")

data_missing <- data.frame(x = 1)

test_that("xgx_check_data throws correct errors", {
  expect_error(xgx_check_data(data_missing),
               "These columns must be present in the dataset: ID,EVID,AMT,TIME,DV,YTYPE")
})
