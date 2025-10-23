library(testthat)
library(quicR)
library(stringr)



files <- list.files("input_files", pattern=".xlsx", full.names=TRUE)
test <- function(file) {
  # data <- readxl::read_xlsx(file, sheet = 2)
  print(file)
  plate <- ifelse(str_detect(file, "384"), 384, 96)

  test_that(
    "get_quic accepts Excel file as input?",
    {
      expect_s3_class(get_quic(file, plate=plate), "data.frame")
    }
  )
}

lapply(files, test)
