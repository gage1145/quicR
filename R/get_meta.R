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
#' @importFrom stringr str_split
#'
#' @export
get_meta <- function(file) {

  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 2, col_name = FALSE)
  }
  else if (is.data.frame(file)) {
    data <- file
  }
  else {
    return ("Please enter either .xlsx string or dataframe. ")
  }

  for (i in 1: nrow(data[, 1])) {
    if (!(is.na(data[i, 2]))) {
      break
    }
  }
  data <- na.omit(data.frame(data[1:i, 1]))
  colnames(data) <- c("a")
  data <- data.frame(within(data, {
    new_columns <- str_split(a, ":", n = 2, simplify = TRUE)})[, -1])
  colnames(data) <- c("Meta_ID", "Meta_info")

  return (data)
}
