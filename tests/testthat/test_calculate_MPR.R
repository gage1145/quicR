library(testthat)
library(quicR)


# Small hand-built data set with known answers.
# A01 peaks at 3, A02 peaks at 1.
df <- data.frame(
  well = rep(c("A01", "A02"), each = 5),
  time  = rep(0:4, 2),
  norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)


test_that("calculate_MPR returns the max normalized value per group", {
  res <- calculate_MPR(df, by = "well")
  expect_equal(res$mpr, c(3, 1))
})

test_that("calculate_MPR keys output on the grouping column", {
  res <- calculate_MPR(df, by = "well")
  expect_equal(res$well, c("A01", "A02"))
  expect_true(all(c("well", "mpr") %in% colnames(res)))
})

test_that("calculate_MPR respects an already-grouped data frame", {
  grouped <- dplyr::group_by(df, well)
  expect_equal(calculate_MPR(grouped), calculate_MPR(df, by = "well"))
})
