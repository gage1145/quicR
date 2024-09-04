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
#'
#' @export
organize_tables <- function(file, plate=96) {

  # Block allows input of an excel file string or a dataframe.
  if (is.character(file)) {
    # Read the Excel file into R.
    data <- read_excel(
      file, sheet=1, col_names=FALSE,
      if (plate == 96) {
        range = cell_cols(1:13)
      } else if (plate == 384) {
        range = cell_cols(1:25)
      } else {
        return(print("Please enter either 96 or 384 for the plate argument. "))
      }
    )
  } else if (is.data.frame(file)) {
    data <- ifelse(plate == 96, file[,1:13], file[,1:25])
  } else {
    return ("Please enter either an excel file string or a dataframe. ")
  }

  # Separate data from metadata.
  for (i in 1: nrow(data[, 2])) {
    if (!(is.na(data[i, 2]))) {
      i <- i - 1
      break
    }
  }
  tidy_data <- data[-(1:i), 2:length(data)]
  metadata <- data[1:i,1]

  # Create a vector with named tibbles for each table.
  i <- 0
  name_list <- list()
  df_dic <- list()
  while (i < nrow(tidy_data)) {
    table_name <- paste0(sub("^\\d+\\. ", "", tidy_data[(1+i), 1]))
    name_list <- append(name_list, table_name)
    new_table <- tidy_data[(3+i):ifelse(plate == 96, (10+i), (18+i)),]
    df_dic <- append(df_dic, list(new_table))
    i <- ifelse(plate == 96, i + 11, i + 19)
  }
  names(df_dic) <- name_list

  return (df_dic)
}
