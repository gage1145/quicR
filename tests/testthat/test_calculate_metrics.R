library(testthat)
library(quicR)


# One frame that exercises the whole calculate_* family at once.
df <- data.frame(
  Wells = rep(c("A01", "A02"), each = 5),
  Time  = rep(0:4, 2),
  Norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  Deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)

res <- calculate_metrics(df, "Wells", threshold = 2)


test_that("calculate_metrics returns one row per group", {
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
})

test_that("calculate_metrics joins every metric into one frame", {
  expect_true(all(c("MPR", "MS", "TtT", "RAF", "AUC") %in% colnames(res)))
})

test_that("calculate_metrics values match the individual calculators", {
  a01 <- res[res$Wells == "A01", ]
  expect_equal(a01$MPR, 3)
  expect_equal(a01$MS, 2)
  expect_equal(a01$TtT, 2.5)
  expect_equal(a01$RAF, 1 / 2.5)
})
