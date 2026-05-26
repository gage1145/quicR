library(testthat)
library(quicR)


# use_r("BMG_format")

test_file <- "BMG_formatting/plate_layout.csv"
ref_file <- "BMG_formatting/formatted.txt"

test_that(
  "BMG_format output matches formatted file.",
  {
    expect_equal(
      data.frame(V1 = BMG_format(test_file)[[1]]),
      read.delim(ref_file, header = FALSE)
    )
  }
)
