#' Retrieve the BMG metadata
#'
#' Takes the Excel file exported from MARS and compiles the metadata in the
#' header.
#'
#' @param file The Excel file exported from MARS.
#'
#' @return A dataframe containing the Meta_ID and Meta_info
#'
#' @importFrom readxl read_excel
#' @importFrom stats na.omit
#' @importFrom tidyr separate_wider_delim
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_meta(file)
#'
#' @export
get_meta <- function(file) {
  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 1, col_names = FALSE) %>%
      suppressMessages()
  } else if (is.data.frame(file)) {
    data <- file
  } else {
    stop("Please enter either .xlsx string or dataframe. ")
  }

  data[1:which(is.na(data[[1]]))[1], 1] |>
    as.data.frame() |>
    na.omit() |>
    separate_wider_delim(
      1,
      ": ",
      names = c("Meta_ID", "Meta_info"),
      too_few = "align_start",
      too_many = "merge"
    )
}
