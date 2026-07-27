library(testthat)
library(quicR)


# Small hand-built data set with known answers.
# A01 peaks at 3, A02 peaks at 1.
df <- data.frame(
  Wells = rep(c("A01", "A02"), each = 5),
  Time  = rep(0:4, 2),
  Norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  Deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)


test_that("calculate_MPR returns the max normalized value per group", {
  res <- calculate_MPR(df, .by = "Wells")
  expect_equal(res$MPR, c(3, 1))
})

test_that("calculate_MPR keys output on the grouping column", {
  res <- calculate_MPR(df, .by = "Wells")
  expect_equal(res$Wells, c("A01", "A02"))
  expect_true(all(c("Wells", "MPR") %in% colnames(res)))
})

test_that("calculate_MPR respects an already-grouped data frame", {
  grouped <- dplyr::group_by(df, Wells)
  expect_equal(calculate_MPR(grouped), calculate_MPR(df, .by = "Wells"))
})
