#' Get the Wells Used in the RT-QuIC Run.
#'
#' Returns the well IDs used in the plate.
#'
#' @param file Excel file exported from MARS
#' @param sheet The sheet containing the well IDs in the Excel file.
#'
#' @return A vector containing well IDs.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_wells(file)
#'
#' @export
get_wells <- function(file, sheet = 2) {
  {
    if (is.character(file)) {
      read_excel(file, sheet = sheet, col_names = FALSE) %>%
        suppressMessages()
    } else if (is.data.frame(file)) {
      file
    } else {
      stop("Please enter either .xlsx string or dataframe. ")
    }
  } %>%
    filter(.$`...1` == "Well") %>%
    select(-(1:2)) %>%
    as.character()
}
