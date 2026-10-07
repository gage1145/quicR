library(testthat)
library(quicR)


x <- suppressMessages(get_quic("input_files/test2.xlsx"))
m <- calculate_metrics(x, threshold = 3)


test_that("print methods describe the object and return it invisibly", {
  expect_output(print(x), "<quic> 96-well plate")
  expect_output(print(m), "<quic_metrics> 96-well plate")
  utils::capture.output(vx <- withVisible(print(x)), vm <- withVisible(print(m)))
  expect_false(vx$visible)
  expect_false(vm$visible)
  expect_identical(vx$value, x)
})

test_that("summary.quic counts wells per sample and dilution", {
  s <- summary(x)
  expect_s3_class(s, "summary.quic")
  expect_equal(sum(s$table$wells), nrow(x$data))
  expect_output(print(s), "<quic summary>")
})

test_that("summary.quic_metrics averages metrics per sample and dilution", {
  s <- summary(m)
  expect_s3_class(s, "summary.quic_metrics")
  expect_equal(sum(s$table$wells), nrow(m$data))
  expect_equal(sum(s$table$crossed), sum(m$data$crossed))
  expect_true(all(c("mpr", "ms", "ttt", "auc") %in% colnames(s$table)))
  expect_output(print(s), "wells crossed")
})

test_that("as.data.frame and as_tibble flatten quic objects", {
  df <- as.data.frame(x)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), sum(vapply(x$data$data, nrow, integer(1))))
  expect_s3_class(tibble::as_tibble(x), "tbl_df")
  expect_equal(as.data.frame(m), as.data.frame(m$data))
})

test_that("autoplot returns ggplots and plot returns them invisibly", {
  expect_s3_class(ggplot2::autoplot(x), "ggplot")
  expect_s3_class(ggplot2::autoplot(m), "ggplot")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_s3_class(plot(x, plot_deriv = FALSE), "ggplot")
  expect_invisible(plot(m))
})

test_that("plate_view and plot_metrics dispatch on quic objects", {
  expect_s3_class(plate_view(x), "ggplot")
  expect_s3_class(plot_metrics(m), "ggplot")
})
