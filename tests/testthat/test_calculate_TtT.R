library(testthat)
library(quicR)


# A01 crosses threshold = 2 between Time 2 (Norm 1) and Time 3 (Norm 3):
#   linear interpolation -> 2 + (2 - 1) * (3 - 2) / (3 - 1) = 2.5
# A02 never crosses -> falls back to the max time (4).
df <- data.frame(
  well = rep(c("A01", "A02"), each = 5),
  time  = rep(0:4, 2),
  norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1)
)


test_that("calculate_TtT linearly interpolates the crossing time", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$ttt[1], 2.5)
})

test_that("calculate_TtT falls back to the max time when never crossed", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$ttt[2], max(df$time))
})

test_that("calculate_TtT reports whether the threshold was crossed", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$crossed, c(TRUE, FALSE))
})

test_that("calculate_TtT returns RAF as the inverse of TtT", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$raf, 1 / res$ttt)
})

test_that("calculate_TtT errors when the threshold is not higher than the background signal.", {
  expect_error(calculate_TtT(df, threshold = 1))
})
