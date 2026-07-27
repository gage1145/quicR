library(testthat)
library(quicR)
library(stringr)



files <- list.files("input_files", pattern=".xlsx", full.names=TRUE)
test <- function(file) {
  plate <- ifelse(str_detect(file, "384"), 384, 96)
  df <- get_quic(file, plate = plate)

  test_that(
    "get_quic accepts Excel file as input?",
    {
      expect_s3_class(df, "data.frame")
    }
  )

  test_that(
    "get_quic returns the expected long-format columns",
    {
      expect_true(
        all(c("Well", "Time", "RFU", "Norm", "Deriv") %in% colnames(df))
      )
    }
  )

  test_that(
    "get_quic normalizes each well to 1 at the norm point (cycle 2)",
    {
      # Norm = RFU / RFU[norm_point]; with the default norm_point = 2 the
      # second reading of every well must normalize to exactly 1.
      norm_at_point <- df |>
        dplyr::group_by(Well) |>
        dplyr::summarize(second = dplyr::nth(Norm, 2), .groups = "drop")
      expect_true(all(abs(norm_at_point$second - 1) < 1e-8))
    }
  )
}

lapply(files, test)
