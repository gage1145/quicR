library(testthat)
library(quicR)


# A01 crosses threshold = 2 between Time 2 (Norm 1) and Time 3 (Norm 3):
#   linear interpolation -> 2 + (2 - 1) * (3 - 2) / (3 - 1) = 2.5
# A02 never crosses -> falls back to the max time (4).
df <- data.frame(
  Wells = rep(c("A01", "A02"), each = 5),
  Time  = rep(0:4, 2),
  Norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1)
)


test_that("calculate_TtT linearly interpolates the crossing time", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$TtT[res$Wells == "A01"], 2.5)
})

test_that("calculate_TtT falls back to the max time when never crossed", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$TtT[res$Wells == "A02"], 4)
})

test_that("calculate_TtT reports whether the threshold was crossed", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$crossed, c(TRUE, FALSE))
})

test_that("calculate_TtT returns RAF as the inverse of TtT", {
  res <- calculate_TtT(df, threshold = 2)
  expect_equal(res$RAF, 1 / res$TtT)
})
