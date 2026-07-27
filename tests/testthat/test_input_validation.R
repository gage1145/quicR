library(testthat)
library(quicR)


# Test that input validation works as expected.
test_that("get_real rejects input that is neither a path nor a data frame", {
  expect_error(get_real(42))
})

test_that("get_meta rejects input that is neither a path nor a data frame", {
  expect_error(get_meta(42))
})

test_that("separate_raw rejects input that is neither a path nor a data frame", {
  expect_error(separate_raw(42, num_rows = 1, export_name = "x.xlsx"))
})

test_that("plate_view rejects plate sizes other than 96 or 384", {
  # plate_view returns an error message string rather than stopping.
  expect_match(plate_view(data.frame(), plate = 100), "Invalid plate layout")
})
