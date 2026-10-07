#' Get all time-series and metadata.
#'
#' Accepts an Excel file or a data frame of real-time RT-QuIC data.
#'
#' @param file An Excel file exported by BMG.
#' @param transpose_table `r lifecycle::badge("deprecated")` This argument no longer has any effect.
#' @param norm_point Integer, defines the cycle to use as background fluorescence.
#' @param which_table Integer, defines which table in the Excel sheet contains the real-time data. Should usually be set to 1.
#' @param window_size Integer, defines the window size for estimating the derivative.
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by 'well'. Can also include 'sample' and 'dilution' if present.
#' @param plate Integer; either 96 or 384 to denote the type of well plate being used.
#' @param smooth Logical; if true, will smooth the data using a rolling mean.
#' @param smooth_factor Integer; the window size for smoothing.
#' @param zero Logical; if true, will zero out the background fluorescence.
#' @param sheet Integer; the sheet number to read from.
#'
#' @return An S3 object of class "quic": a list with `data`, a tibble with one row
#'   per well and the time series nested in a `data` list-column, and `params`,
#'   the settings used to build it. Use `as.data.frame()` to get the long-format
#'   data, and `print()`, `summary()`, `plot()` or `ggplot2::autoplot()` to inspect it.
#'
#' @import dplyr
#' @importFrom purrr pluck
#' @importFrom tidyr nest
#' @importFrom readxl read_xlsx
#' @importFrom zoo rollmean
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' x <- get_quic(file)
#' x
#' summary(x)
#' head(as.data.frame(x))
#'
#' @export
get_quic <- function(file, transpose_table=lifecycle::deprecated(), norm_point=2, which_table=1,
                     window_size=2, smooth = FALSE, smooth_factor = 4, zero=FALSE, 
                     .by=lifecycle::deprecated(), by=c("well"), plate=96, sheet = 2) {

  if (lifecycle::is_present(transpose_table)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "get_quic(transpose_table)"
    )
  }

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "get_quic(.by)"
    )
    by <- .by
  }

  stopifnot(!smooth || norm_point > smooth_factor / 2)
  
  data <- file %>%
    read_xlsx(sheet=sheet, col_names=FALSE) %>%
    get_real() %>%
    pluck(which_table) %>%
    rename_with(tolower) %>%
    rename(any_of(c(sample = "sample ids", dilution = "dilutions")))

  # Exports without a Sample IDs or Dilutions table still get those columns.
  data[setdiff(c("sample", "dilution"), names(data))] <- NA_character_

  data <- data %>%
    mutate(
      rfu = if (smooth) rollmean(rfu, smooth_factor, na.pad=TRUE) else rfu,
      norm = rfu/rfu[norm_point] - zero,
      deriv = (lead(norm, window_size) - lag(norm, window_size)) / (lead(time, window_size) - lag(time, window_size)),
      .by = all_of(by)
    ) %>%
    nest(data = c(time, rfu, norm, deriv)) %>%
    suppressMessages()

  params <- list(
    plate = plate, by = by, norm_point = norm_point, window_size = window_size,
    smooth = smooth, smooth_factor = smooth_factor, zero = zero
  )

  validate_quic(new_quic(data, params))
}
