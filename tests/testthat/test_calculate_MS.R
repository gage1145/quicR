library(testthat)
library(quicR)


df <- data.frame(
  Wells = rep(c("A01", "A02"), each = 5),
  Time  = rep(0:4, 2),
  Norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  Deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)


test_that("calculate_MS returns the max derivative per group", {
  res <- calculate_MS(df, .by = "Wells")
  expect_equal(res$MS, c(2, 0))
})

test_that("calculate_MS respects an already-grouped data frame", {
  grouped <- dplyr::group_by(df, Wells)
  expect_equal(calculate_MS(grouped), calculate_MS(df, .by = "Wells"))
})
