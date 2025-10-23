#' Organize MARS Tables
#'
#' Extracts the tables from the microplate view sheet in the MARS Excel file
#' and adds each table to a list.
#'
#' @param file An Excel file exported from MARS.
#' @param plate Integer either 96 or 384 to denote microplate type.
#'
#' @return A list containing tibbles.
#'
#' @importFrom readxl cell_cols
#' @importFrom readxl read_xlsx
#' @importFrom tidyr unite
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' organize_tables(file)
#'
#' @export
organize_tables <- function(file, plate = 96) {

  if (plate != 96 & plate != 384) {
    stop("Please enter either 96 or 384 for the plate argument. ")
  }
  if (!is.character(file) & !is.data.frame(file)) {
    stop("Please enter either an excel file string or a dataframe. ")
  }

  # Block allows input of an excel file string or a dataframe.
  if (is.character(file)) {
    data <- read_xlsx(
      file,
      sheet = 1,
      col_names = FALSE,
      if (plate == 96) range = cell_cols(2:13) else range = cell_cols(2:25)
    ) |>
      suppressMessages()
  } else {
    if (plate == 96) data <- file[2:13] else data <- file[2:25]
  }

  # Remove leading NA rows.
  for (i in 1:nrow(data)) {
    if (!is.na(data[i,1])) {
      if (i == 1) break
      data <- data[-(1:i-1),]
      break
    }
  }

  # Create a vector with named tibbles for each table.
  step <- ifelse(plate == 96, 11, 19)
  df_dic <- list()
  i <- 1

  while (i < nrow(data)) {
    name <- paste0(sub("^\\d+\\. ", "", data[[i, 1]]))
    df_dic[[name]] <- data[(i + 2): (i + step - 2), ]
    i <- i + step
  }

  df_dic$Wells <- (
    expand.grid(
      {if (plate == 96) LETTERS[1:8] else LETTERS[1:16]},
      {if (plate == 96) sprintf("%02d", 1:12) else sprintf("%02d", 1:24)}
    ) %>%
      unite("Wells", 1,2, sep="")
  )[[1]] %>%
    matrix(
      nrow=ifelse(plate == 96, 8, 16),
      ncol=ifelse(plate == 96, 12, 24)
    )

  return(df_dic)
}
