library(stringr)
library(readxl)
library(tidyverse)
library(writexl)
library(openxlsx)
library(reshape2)



plate_view <- function(df, meta, well_names, plate=96, color="black") {

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
    geom_line(colour = color) +
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

  # Block allows input of an excel file string or a dataframe.
  if (is.character(file)) {
    # Read the Excel file into R.
    data <- read_excel(file,
                       sheet = 1,
                       if (plate == 96) {range = cell_cols(1:13)}
                       else if (plate == 384) {range = cell_cols(1:25)}
                       else {return (print("Please enter either 96 or 384 for the 'plate' argument. "))},
                       col_name = FALSE)


  } else if (is.data.frame(file)) {
    if (plate == 96) {data <- file[,1:13]}
    else {data <- file[,1:25]}
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

get_wells <- function (file) {

  if (is.character(file)) {
    df <- read_excel(file, sheet = 2, col_name = FALSE)
  }
  else if (is.data.frame(file)) {
    df <- file
  }
  else {
    return ("Please enter either .xlsx string or dataframe. ")
  }


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

  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 2, col_name = FALSE)
  }
  else if (is.data.frame(file)) {
    data <- file
  }
  else {
    return ("Please enter either .xlsx string or dataframe. ")
  }

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

# Separates the raw run files in the .xlsx file.
separate_raw <- function(file, num_rows, export_name) {

  if (is.character(file)) { # Read the Excel file into R.
    data <- read_excel(file, sheet = 2)
  } else if (is.data.frame(file)) {
    data <- file
  }  else {
    return ("Please enter either .xlsx string or dataframe. ")
  }


  # Remove metadata.
  tidy_data <- data[-(1:num_rows-1), -1] %>%
    na.omit(tidy_data)


  # Set the first row as column names.
  col_names <- tidy_data[1, ]
  tidy_data <- tidy_data[-1, ]
  colnames(tidy_data) <- col_names

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

  # Convert tidy_data to numeric.
  tidy_data <- as.data.frame(sapply(tidy_data, as.numeric))

  # Rearrange columns to group replicates of the same sample
  tidy_data <- tidy_data %>%
    select(Time, order(colnames(tidy_data), decreasing = FALSE))

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
  while (i <= reads) {
    if (num_rows == cycles) {
      df <- cbind(time_df, tidy_data[(num_rows - cycles):num_rows, -1])
      assign(paste0("df", i), df)
      num_rows <- num_rows + cycles
    } else {
      df <- cbind(time_df, tidy_data[(1 + num_rows - cycles):num_rows, -1])
      assign(paste0("df", i), df)
      num_rows <- num_rows + cycles
    }
    i <- i + 1
  }

  # Export the organized data
  existing_file <- openxlsx::loadWorkbook(export_name)

  # Function to write a data frame to a new sheet in the workbook
  write_to_sheet <- function(df, sheet_name, existing_file) {
    openxlsx::addWorksheet(existing_file, sheetName = sheet_name)
    openxlsx::writeData(existing_file, sheet = sheet_name, df, startRow = 1,
                        startCol = 1, rowNames = FALSE)
  }

  # Write each data frame to a new sheet in the workbook
  i = 1
  while (i <= reads) {
    df_name <- paste0("df", i)
    df <- get(df_name)
    sheet_name <- paste0("Data", i)
    write_to_sheet(df, sheet_name, existing_file)
    i = i + 1
  }
  #rm(df, df_name, i, reads, sheet_name)

  # Save the modified file
  openxlsx::saveWorkbook(existing_file, file, overwrite = TRUE)

  # Open the file for the user to view
  if (Sys.info()["sysname"] == "Windows") {
    shell.exec(file)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", file))
  }
}

normalize_RFU <- function (df, bg_cycle=4) {
  # Accepts output from get_real function.
  # Apply the column names as the first row instead.
  df <- rbind(colnames(df), df)
  df <- df %>%
    unname() %>%
    t() %>%
    as.data.frame()

  # Rename first cell as "Sample ID".
  df[1,1] <- "Sample ID"

  # Make all columns numeric except for the first column with sample IDs.
  # Combine the sample ID column with the numeric columns
  df <- cbind(df[,1],
              mutate_all(df[, -1],
                         function(x) as.numeric(as.character(x))))

  # Make the first row the column names for the df and remove first row.
  colnames(df) <- df[1,]
  df <- df[-1,]

  df_norm <- df
  for (i in 1:nrow(df)) {
    for (j in 2:ncol(df)) {
      raw_value <- df_norm[i,j]
      df_norm[i,j] <- raw_value / df[i, bg_cycle + 1]
    }
  }
  return (df_norm)
}

calculate_TtT <- function (data, threshold, start_col=3) {
  # Initialize the list containing the times-to-threshold.
  TtT_list <- c(rep(NA, nrow(data)))
  
  # Set the cycle interval.
  cycle_interval <- diff(as.numeric(colnames(data[,2:3])))
  
  # Calculate the Time to Threshold
  for (i in 1: nrow(data))  {
    for (j in start_col: (ncol(data))) {
      
      # Use the sample-unique threshold calculated in "summary".
      current_read <- data[i, j]
      
      if (is.na(TtT_list[i]) & current_read >= threshold) {
        
        previous_read <- data[i, j-1]
        
        delta_rfu <-  current_read - previous_read
        slope <- delta_rfu / cycle_interval
        
        delta_threshold <- threshold - previous_read
        delta_t <- delta_threshold / slope
        
        previous_cycle <- as.numeric(colnames(data[j-1]))
        TtT_list[i] <- previous_cycle + delta_t
      }
    }
  }
  return (TtT_list)
}

calculate_MPR <- function (data, start_col=3, data_is_norm=FALSE) {
  if (data_is_norm == FALSE) {
    # Calculate the normalized real-time data.
    data <- normalize_RFU(data)
  }
  
  # Initialize the list containing the maxpoint ratios.
  MPR_list <- c(rep(NA, nrow(data)))
  
  # Identify the maxpoint ratio.
  for (i in 1: nrow(data)) {
    maximum <- max(data[i, start_col:(ncol(data))])
    MPR_list[i] <- maximum
  }
  return (MPR_list)
}

calculate_MS <- function (data, start_col=3) {
  # Calculate the slope using a moving window linear regression.
  df_norm_t <- t(data)
  colnames(df_norm_t) <- df_norm_t[1,]
  df_norm_t <- df_norm_t[-1,]
  df_norm_t <- cbind(Time = as.numeric(rownames(df_norm_t)), df_norm_t)
  df_norm_t <- as.data.frame(df_norm_t)
  
  unique_cols <- colnames(df_norm_t)[1]
  
  x <- 1
  for (i in colnames(df_norm_t)[-1]) {
    unique_cols <- cbind(unique_cols, paste0(i, "_", x))
    x <- x + 1
  }
  
  unique_cols <- gsub("-", "_", unique_cols, fixed = TRUE)
  
  colnames(df_norm_t) <- unique_cols
  
  df_deriv <- df_norm_t$Time
  
  # Make sure there are no "-" in the sample IDs. This affects the formula below.
  for (i in colnames(df_norm_t)[-1]) {
    slope_column <- slide(df_norm_t,
                          ~ lm(as.formula(paste(i, "~ Time")),
                               data = .x)[[1]][[2]] / 3600,
                          .before = 3,
                          .complete = TRUE)
    df_deriv <- cbind(df_deriv, slope_column)
  }
  
  # Reformat df_deriv to match data formatting.
  df_deriv <- df_deriv %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  df_deriv <- df_deriv[-1,]
  df_deriv <- cbind(data$`Sample ID`, df_deriv)
  colnames(df_deriv) <- colnames(data)
  df_deriv[,-1] <- mutate_all(df_deriv[,-1], 
                              function(x) as.numeric(as.character(x)))
  
  # Initialize the list containing the max slope.
  MS_list <- c(rep(NA, nrow(data)))
  
  for (i in 1: nrow(df_deriv)) {
    MS_list[i] <- max(df_deriv[i,5:(ncol(df_deriv))])
  }
  return (MS_list)
}

BMG_format <- function(file) {
  df_ <- read.csv(file, header = TRUE)
  locations <- c()
  samples <- c()
  for (i in 1: (ncol(df_)-1)) {
    for (j in 1: nrow(df_)) {
      locations <- rbind(locations, paste0(LETTERS[j], i))
      samples <- rbind(samples, df_[j, (i+1)])
    }
  }
  
  locations <- cbind(locations, samples) %>%
    as.data.frame()
  
  colnames(locations) <- c("Wells", "Samples")

  dic <- unique(melt(df_, id.vars=1)[3]) %>%
    mutate(Plate_ID = "X") %>%
    na.omit()
  
  x <- 0
  previous <- dic$value[1]
  current <- ""
  for (i in 1:nrow(dic)) {
    current <- dic$value[i]
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
  colnames(dic) <- c("Samples", "Plate_ID")
  locations <- left_join(locations, dic)
  
  # Function to format each row
  format_row <- function(row) {
    sprintf("%-4s%-7s%s", row[1], row[3], row[2])
  }
  
  # Apply the function to each row of the data frame
  formatted <- apply(locations, 1, format_row) %>%
    na.omit()
  
  
  return (formatted)
}










