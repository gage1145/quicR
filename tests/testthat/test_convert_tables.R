library(testthat)
library(quicR)


test_that("convert_tables flattens a list of tables into columns (row-major)", {
  tabs <- list(
    IDs = matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)
  )
  res <- suppressMessages(convert_tables(tabs))
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), "IDs")
  # as.vector(t(x)) reads the matrix row by row.
  expect_equal(res$IDs, c("a", "b", "c", "d"))
})

test_that("convert_tables drops NA rows when na_omit = TRUE", {
  tabs <- list(IDs = matrix(c("a", NA, "c", "d"), nrow = 2, byrow = TRUE))
  res <- suppressMessages(convert_tables(tabs, na_omit = TRUE))
  expect_equal(res$IDs, c("a", "c", "d"))
})

test_that("convert_tables rejects non-vector input", {
  expect_error(convert_tables(matrix(1:4, nrow = 2)))
})
