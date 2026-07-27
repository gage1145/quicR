library(testthat)
library(quicR)


test_that("get_meta returns a Meta_ID / Meta_info table", {
  res <- get_meta("input_files/test.xlsx")
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("Meta_ID", "Meta_info"))
})

test_that("get_meta accepts a data frame as input", {
  df <- readxl::read_excel("input_files/test.xlsx", sheet = 1, col_names = FALSE) |>
    suppressMessages()
  expect_s3_class(suppressMessages(get_meta(df)), "data.frame")
})
