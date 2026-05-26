#' Format Table for BMG Sample ID Import
#'
#' BMG_format accepts a plate layout .CSV file and formats the Sample IDs into
#' a format which can be easily imported into the BMG control software.
#'
#' @param file A .CSV file containing the plate layout of Sample IDs.
#' @param save_path The path to the directory that you want the file saved.
#' @param save_name The name of the output file. Should have the ".txt" extension.
#' @param write_file Logical. If true, function will write a .txt file; otherwise it will return a character vector.
#'
#' @return A text file containing information for import into the BMG control software.
#'
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' layout_file <- system.file(
#'   "extdata/BMG_formatting",
#'   file = "plate_layout.csv",
#'   package = "quicR"
#' )
#' BMG_format(layout_file)
#'
#' @export
BMG_format <- function(file, save_path = "", save_name = "formatted.txt", write_file = FALSE) {
  df_ <- read.csv(file, header = F)
  colnames(df_) <- c("col", df_[1, -1])
  df_ <- df_[-1, ]

  wells <- expand.grid(LETTERS[1:nrow(df_)], 1:(ncol(df_) - 1)) |>
    unite("Wells", 1:2, sep="") |>
    mutate(Wells = as.factor(Wells))

  dic <- df_ |>
    pivot_longer(2:ncol(df_), values_to="Samples") |>
    unique() |>
    mutate(Plate_ID = "X") |>
    na.omit() |>
    unite("Wells", c("col", "name"), sep = "") |>
    mutate(Wells = factor(Wells, levels = wells$Wells)) |>
    arrange(Wells)

  x <- 0
  previous <- dic[["Samples"]][1]
  current <- ""
  for (i in 1:nrow(dic)) {
    current <- dic[["Samples"]][i]

    if (tolower(current) %in% c("n", "p", "b")) {
      dic[i, "Plate_ID"] <- toupper(current)
      next
    }

    if (previous != current) x <- x + 1

    dic[i, "Plate_ID"] <- paste0(dic[[i, "Plate_ID"]], x)
    previous <- current
  }

  # Function to format each row
  format_row <- function(row) {
    sprintf("%-4s%-7s%s", row[1], row[3], row[2])
  }

  # Apply the function to each row of the data frame
  formatted <- dic |>
    apply(1, format_row) |>
    na.omit()
  if (write_file == TRUE) {
    writeLines(formatted, paste0(save_path, save_name))

  }
  return(data.frame(formatted))
}
