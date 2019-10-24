test_that("spoils with no arguments returns a list of simple
          features data frames", {
  expect_type(spoils(), "list")
  expect_s3_class(spoils()[[1]], c("sf","data.frame"), exact = TRUE)

})
