library(testthat)
library(quicR)


# One frame that exercises the whole calculate_* family at once.
df <- data.frame(
  well  = rep(c("A01", "A02"), each = 5),
  time  = rep(0:4, 2),
  norm  = c(1, 1, 1, 3, 3,   1, 1, 1, 1, 1),
  deriv = c(0, 0, 2, 2, 0,   0, 0, 0, 0, 0)
)

res <- calculate_metrics(df, "well", threshold = 2)


test_that("calculate_metrics on a data frame returns one row per group", {
  expect_s3_class(res, "data.frame")
  expect_false(inherits(res, "quic_metrics"))
  expect_equal(nrow(res), 2)
})

test_that("calculate_metrics joins every metric into one frame", {
  expect_true(all(c("mpr", "ms", "ttt", "raf", "auc") %in% colnames(res)))
})

test_that("calculate_metrics values match the individual calculators", {
  a01 <- res[res$well == "A01", ]
  expect_equal(a01$mpr, 3)
  expect_equal(a01$ms, 2)
  expect_equal(a01$ttt, 2.5)
  expect_equal(a01$raf, 1 / 2.5)
})

test_that("calculate_metrics on a quic object returns a quic_metrics object", {
  x <- suppressMessages(get_quic("input_files/test2.xlsx"))
  m <- calculate_metrics(x, threshold = 3)

  expect_s3_class(m, "quic_metrics")
  expect_equal(nrow(m$data), nrow(x$data))
  expect_equal(m$params$by, c("sample", "dilution", "well"))
  expect_equal(m$params$threshold, 3)
  # Settings from get_quic() are carried forward.
  expect_equal(m$params$plate, x$params$plate)
  expect_equal(m$params$norm_point, x$params$norm_point)
})
