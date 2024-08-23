library(testthat)
library(usethis)
library(readxl)
library(quicR)

use_r("get_real")
files <- list.files("input files", full.names = TRUE)
# df_list <- get_real(files[1])[[1]]

for (file in files) {
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
