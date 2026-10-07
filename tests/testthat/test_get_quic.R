library(testthat)
library(quicR)
library(stringr)



files <- list.files("input_files", pattern=".xlsx", full.names=TRUE)
test <- function(file) {
  plate <- ifelse(str_detect(file, "384"), 384, 96)
  x <- suppressMessages(get_quic(file, plate = plate))
  df <- as.data.frame(x)

  test_that(
    "get_quic returns a quic object",
    {
      expect_s3_class(x, "quic")
      expect_named(x, c("data", "params"))
      expect_equal(x$params$plate, plate)
    }
  )

  test_that(
    "get_quic nests one row per well with sample and dilution columns",
    {
      expect_true(all(c("well", "sample", "dilution", "data") %in% colnames(x$data)))
      expect_equal(anyDuplicated(x$data$well), 0)
    }
  )

  test_that(
    "as.data.frame returns the expected long-format columns",
    {
      expect_true(
        all(c("well", "time", "rfu", "norm", "deriv") %in% colnames(df))
      )
    }
  )

  test_that(
    "get_quic normalizes each well to 1 at the norm point (cycle 2)",
    {
      # norm = rfu / rfu[norm_point]; with the default norm_point = 2 the
      # second reading of every well must normalize to exactly 1.
      norm_at_point <- df |>
        dplyr::group_by(well) |>
        dplyr::summarize(second = dplyr::nth(norm, 2), .groups = "drop")
      expect_true(all(abs(norm_at_point$second - 1) < 1e-8))
    }
  )
}

lapply(files, test)

test_that("get_quic keeps per-well dilutions instead of recycling the first one", {
  x <- suppressMessages(get_quic("input_files/test3.xlsx"))
  expect_gt(length(unique(stats::na.omit(x$data$dilution))), 1)
})
