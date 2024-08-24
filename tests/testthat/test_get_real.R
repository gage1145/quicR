library(testthat)
library(readxl)
library(quicR)





for (file in c("input files/test.xlsx", "input files/test2.xlsx", "input files/test3.xlsx")) {
  data <- read_xlsx(file, sheet = 2)

  test_that(
    "get_real accepts Excel file as input?",
    {expect_type(get_real(file), "list")}
  )

  test_that(
    "get_real accepts dataframe as input?",
    {expect_type(get_real(data), "list")}
  )

  test_that(
    "get_real returns list of dataframes?",
    {expect_true(is.data.frame(get_real(data)[[1]]))}
  )
}
