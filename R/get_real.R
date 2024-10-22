#' Get Real-Time RT-QuIC Fluorescence Data
#'
#' Accepts an Excel file or a dataframe of real-time RT-QuIC data.
#'
#' @param file Either an Excel file or a dataframe.
#' @param ordered Boolean, if true (default), will organize the columns by sample ID.
#'
#' @return A list of dataframes containing the formatted real-time data.
#'
#' @importFrom dplyr rename
#' @importFrom readxl read_excel
#'
#' @export
get_real <- function(file, ordered = TRUE) {
  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 2, col_names = FALSE)
  } else if (is.data.frame(file)) {
    data <- file
  } else {
    return("Please enter either .xlsx string or dataframe. ")
  }

  # Determines the number of rows to remove.
  for (i in 1:nrow(data)) {
    if (!(is.na(data[i, 2]))) {
      num_rows <- i
      break
    }
  }

  # Remove metadata.
  tidy_data <- data[-(1:(num_rows - 1)), -1] %>%
    na.omit(tidy_data)


  # Set the first row as column names.
  colnames(tidy_data) <- tidy_data[1, ]
  tidy_data <- tidy_data[-1, ]
  # tidy_data <- tidy_data[1:65,]

  # Add leading "0" before single digits in column names.
  colnames(tidy_data) <- gsub(" X(\\d)$", " X0\\1", colnames(tidy_data))

  # Identify and handle duplicate column names.
  dup_cols <- colnames(tidy_data)[duplicated(colnames(tidy_data))]
  if (length(dup_cols) > 0) {
    # Add suffix to duplicate column names
    for (col in dup_cols) {
      indices <- which(colnames(tidy_data) == col)
      colnames(tidy_data)[indices] <- paste0(col, "_", indices)
    }
  }

  # Rename the first column as "Time"
  tidy_data <- tidy_data %>%
    rename(Time = 1)

  # Rearrange columns to group replicates of the same sample
  if (ordered == TRUE) {
    tidy_data <- tidy_data %>%
      select(Time, order(colnames(tidy_data), decreasing = FALSE))
  }

  # Remove suffixes from column names
  colnames(tidy_data) <- gsub("_\\d+$", "", colnames(tidy_data))

  # Designate the integers used to calculate how the data will be cut
  cycles <- length(unique(tidy_data$Time)) # Number of cycles
  num_rows <- cycles # This will change after sending
  # one data type to a data frame
  reads <- length(which(tidy_data$Time == 0)) # Number of types of data (e.g. Raw,
  # Normalized, or Derivative)

  # Create a data frame with only the "Time" column with no duplicates
  time_df <- data.frame(unique(tidy_data$Time)) %>%
    rename(Time = 1)

  # Create separate data frames for different read types
  i <- 1
  df_list <- list()
  while (i <= reads) {
    if (num_rows == cycles) {
      df <- cbind(time_df, tidy_data[(num_rows - cycles):num_rows, -1])
      num_rows <- num_rows + cycles
    } else {
      df <- cbind(time_df, tidy_data[(1 + num_rows - cycles):num_rows, -1])
      num_rows <- num_rows + cycles
    }
    i <- i + 1
    df_list <- append(df_list, list(df))
  }
  return(df_list)
}
