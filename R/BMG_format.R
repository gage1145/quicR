#' Format Table for BMG Sample ID Import
#'
#' BMG_format accepts a plate layout .CSV file and formats the Sample IDs into
#' a format which can be easily imported into the BMG control software.
#'
#' @param file A .CSV file containing the plate layout of Sample IDs.
#'
#' @return A text file containing information for import into the BMG control software.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom tidyr unite
#' @importFrom stats na.omit
#' @importFrom reshape2 melt
#'
#' @export
BMG_format <- function(file) {
  df_ <- read.csv(file, header = F)
  colnames(df_) <- c("col", df_[1, -1])
  df_ <- df_[-1, ]

  locations <- c()
  samples <- c()
  for (i in 1:(ncol(df_) - 1)) {
    for (j in 1:nrow(df_)) {
      locations <- rbind(locations, paste0(LETTERS[j], i))
      samples <- rbind(samples, df_[j, (i + 1)])
    }
  }

  locations <- locations |>
    cbind(samples) |>
    as.data.frame()

  colnames(locations) <- c("Wells", "Samples")

  dic <- df_ |>
    melt(id.vars = 1) |>
    unique() |>
    mutate(Plate_ID = "X") |>
    na.omit() |>
    unite("Wells", c("col", "variable"), sep = "")

  x <- 0
  previous <- dic[["value"]][1]
  current <- ""
  for (i in 1:nrow(dic)) {
    current <- dic[["value"]][i]
    if (tolower(current) == "n") {
      dic[i, "Plate_ID"] <- "N"
    } else if (tolower(current) == "p") {
      dic[i, "Plate_ID"] <- "P"
    } else if (tolower(current) == "b") {
      dic[i, "Plate_ID"] <- "B"
    } else {
      if (previous != current) {
        x <- x + 1
      }
      dic[i, "Plate_ID"] <- paste0(dic[[i, "Plate_ID"]], x)
    }
    previous <- current
  }

  dic <- select(dic, -"value")

  # colnames(dic) <- c("Samples", "Plate_ID")
  locations <- left_join(locations, dic)

  # locations <- locations[, c("Wells", "Plate_ID", "Samples")]

  # Function to format each row
  format_row <- function(row) {
    sprintf("%-4s%-7s%s", row[1], row[3], row[2])
  }

  # Apply the function to each row of the data frame
  formatted <- locations |>
    apply(1, format_row) |>
    na.omit()

  return(formatted)
}
