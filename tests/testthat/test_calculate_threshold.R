library(testthat)
library(quicR)


# Background (Time == 0) RFU values are 1 and 3:
#   mean = 2, sd = sqrt(2) ~ 1.4142
df <- data.frame(
  Wells = rep(c("A01", "A02"), each = 3),
  Time  = rep(0:2, 2),
  RFU   = c(1, 5, 9,   3, 6, 9)
)


test_that("calculate_threshold returns mean + sd at the background time", {
  expect_equal(
    calculate_threshold(df, background_time = 0),
    2 + sd(c(1, 3))
  )
})

test_that("calculate_threshold applies the multiplier to the sd", {
  expect_equal(
    calculate_threshold(df, background_time = 0, multiplier = 10),
    2 + sd(c(1, 3)) * 10
  )
})

test_that("calculate_threshold returns NA when method is 'none'", {
  expect_true(is.na(calculate_threshold(df, method = "none")))
})
