library(testthat)
library(quicR)


df <- data.frame(
  well = rep(c("A01", "A02"), each = 5),
  time  = rep(0:4, 2),
  norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)


test_that("calculate_MS returns the max derivative per group", {
  res <- calculate_MS(df, by = "well")
  expect_equal(res$ms, c(2, 0))
})

test_that("calculate_MS respects an already-grouped data frame", {
  grouped <- dplyr::group_by(df, well)
  expect_equal(calculate_MS(grouped), calculate_MS(df, by = "well"))
})
