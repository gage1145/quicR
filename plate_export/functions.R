library(stringr)
library(readxl)
library(tidyverse)



plate_view <- function(df, meta, well_names, plate=96) {

  if (plate != 96 & plate != 384) {
    return ("Invalid plate layout. Format should be either 96 or 384. ")
  }

  # Ensures that the input is a dataframe.
  df <- data.frame(df)

  well_names <- meta$A
  colnames(df) <- well_names

  # Create a template of all possible columns
  template_columns <- expand.grid(if (plate == 96) {Var1 = LETTERS[1:8]}
                                  else {Var1 = LETTERS[1:16]},
                                  if (plate == 96) {Var2 = sprintf("%02d", 1:12)}
                                  else {Var2 = sprintf("%02d", 1:24)})
  template_columns <- sort(paste0(template_columns$Var1, template_columns$Var2))
  rm(Var1, Var2)

  # Iterate over all columns.
  for (col in template_columns) {
    # If the column is in df, add it to result_df.
    if (!(col %in% colnames(df))) {
      df[[col]] <- NA
    }
  }

  # Ensure columns are sorted alphabetically.
  df <- df[, order(names(df))]

  # Rename columns as the content again now that empty columns have been added.
  i <- 1
  for (col in template_columns) {
    if (col %in% well_names) {
      colnames(df)[i] <- meta$B[meta$A == col]
    } else {
      # Adds whitespace * i so each empty column is unique. Otherwise the melt
      # function would combine them.
      colnames(df)[i] <- paste(replicate(i, " "), collapse = "")
    }
    i <- i + 1
  }

  # Add a "Time" column. This is important for the melt function.
  df <- cbind(Time = rownames(df), df)

  # Melt the data to help with the faceting.
  df <- reshape2::melt(df, id.vars = "Time")

  # Ensures that Time and observations are numeric.
  df$Time <- as.numeric(df$Time)
  df$value <- as.numeric(df$value)

  # Create a facet plot.
  p <- ggplot(df, aes(x = Time, y = value)) +
    geom_line() +
    labs(y = "RFU",
         x = "Time (h)") +
    theme_classic2() +
    theme(panel.border     = element_rect(colour = "black",
                                          fill = NA,
                                          size = 0.5),
          strip.background = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_blank()) +
    facet_wrap(vars(variable),
               if (plate == 96) {nrow = 8} else {nrow = 16},
               if (plate == 96) {ncol = 12} else {ncol = 24})
  return (p)
}

organize_tables <- function(file, plate=96) {

  # Read the Excel file into R.
  data <- read_excel(file,
                     sheet = 1,
                     if (plate == 96) {range = cell_cols(1:13)}
                     else {range = cell_cols(1:25)},
                     col_name = FALSE)

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
    new_table <- list(tidy_data[(3+i):(10+i),])
    df_dic <- append(df_dic, new_table)
    i = i + 11
  }
  names(df_dic) <- name_list

  return (df_dic)
}

get_wells <- function (file) {
  df <- read_excel(file, sheet = 2, col_name = FALSE)

  # Get the wells used in the run.
  for (i in 1: nrow(df)) {
    while (is.na(df[i, 1])) {
      i <- i + 1
    }
    if (df[i, 1] == "Well") {
      wells <- c(df[i, ])
      break
    }
  }
  return (wells[-(1:2)])
}

get_real <- function(file, ordered=TRUE) {

  # Read the Excel file into R.
  data <- read_excel(file, sheet = 2, col_names = FALSE)

  # Determines the number of rows to remove.
  for (i in 1:nrow(data)) {
    if (!(is.na(data[i, 2]))) {
      num_rows <- i
      break
    }
  }

  # Remove metadata.
  tidy_data <- data[-(1:(num_rows-1)), -1] %>%
    na.omit(tidy_data)


  # Set the first row as column names.
  colnames(tidy_data) <- tidy_data[1, ]
  tidy_data <- tidy_data[-1, ]
  #tidy_data <- tidy_data[1:65,]

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
  #rm(col, dup_cols, indices)

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
  cycles <- length(unique(tidy_data$Time))    # Number of cycles
  num_rows <- cycles                          # This will change after sending
  # one data type to a data frame
  reads <- length(which(tidy_data$Time==0))   # Number of types of data (e.g. Raw,
  # Normalized, or Derivative)

  # Create a data frame with only the "Time" column with no duplicates
  time_df <- data.frame(unique(tidy_data$Time)) %>%
    rename(Time = 1)

  # Create separate data frames for different read types
  i = 1
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
  return (df_list)
}

get_meta <- function(file) {
  # Read the Excel file into R.
  data <- read_excel(file, sheet = 1, col_names = FALSE)

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

add_reps <- function(df) {
  if (ncol(df) > 2) {
    return ("Dataframe should only have two columns with well IDs and Sample IDs")
  }
  df[, "C"] = NA
  count_list <- list()
  for (i in 1: nrow(df)) {
    samp <- df[i, 2]
    count_list <- append(count_list, samp)
    df[i, 3] <- sum(count_list == samp)
  }
  df[, "B"] <- paste(df$B,
                     df$C,
                     sep = "_")
  df <- df[, 1:2]
  return (df)
}
