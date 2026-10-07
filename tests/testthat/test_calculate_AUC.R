library(testthat)
library(quicR)


# Trapezoidal area under Norm vs Time:
#   A01: points (0,0),(1,2),(2,4),(3,4),(4,4) -> 1 + 3 + 4 + 4 = 12
#   A02: flat line at Norm = 1 over 0..4      -> 4
df <- data.frame(
  well = rep(c("A01", "A02"), each = 5),
  time = rep(0:4, 2),
  norm = c(0, 2, 4, 4, 4,   1, 1, 1, 1, 1)
)


test_that("calculate_AUC computes the trapezoidal integral per group", {
  res <- calculate_AUC(df, by = "well")
  expect_equal(res$auc, c(12, 4))
})

test_that("calculate_AUC respects an already-grouped data frame", {
  grouped <- dplyr::group_by(df, well)
  expect_equal(calculate_AUC(grouped), calculate_AUC(df, by = "well"))
})
