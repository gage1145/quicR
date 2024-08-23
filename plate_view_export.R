library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(quicR)


# Initialize parameters for downstream functions.###############################

file <- ""
while (file == "") {
  file <- paste0(readline("Please input .xlsx file name WITHOUT extension: "),
                 ".xlsx")
  if (!(file.exists(file))) {
    print("File does not exist. Please ensure file name was typed correctly")
    file <- ""
  }
}

plate <- ""
while (plate == "") {
  plate <-  as.integer(readline("Please enter 96 or 384 for plate type: "))
  if (plate != 96 & plate != 384) {
    plate <- ""
  }
}

# Following block exports the plate view figure.################################

# Define the layout using the first sheet in the excel file.
# The sheet should be formatted so that each ID in the "layout" table is unique.
# "organize_tables" is sourced from plate_view_function.R.
df_dic <- organize_tables(file, plate = plate)
IDs <- t(df_dic[["Sample IDs"]])
ID_list <- list()
for (i in IDs) {
  for (j in i) {
    if (!(is.na(j))) {
      ID_list <- append(ID_list, j)
    }
  }
}

# Determine if there is a dilutions table.
dilution_bool <- "Dilutions" %in% names(df_dic)

# Add dilution factors if applicable.
if (dilution_bool) {
  dilutions <- c()
  for (i in t(df_dic[["Dilutions"]])) {
    for (j in i) {
      if (!is.na(j)) {
        dilutions <- rbind(dilutions, j)
      }
    }
  }
}

# Read in the real-time data.
# "get_real" will return a list of dataframes depending on how many real-time
# measurements the user exported from MARS.
df_list <- get_real(file, ordered=FALSE)

df_id <- as.integer(
  readline(
    paste(
      "There are",
      length(df_list),
      "real-time data sets. Please enter a number in that range: "
    )
  )
)

df <- data.frame(df_list[[df_id]])
df_meta <- get_meta(file)

# Specify the time column.
time_col <- df[, 1]

# Remove the time column and ID row.
df <- df[, -1]

# Set the time column as the df index.
rownames(df) <- time_col

# Get the wells used in the run.
wells <- get_wells(file)

# Take the metadata and apply it into a dataframe for the plate_view function.
sample_locations <- na.omit(do.call(rbind, Map(data.frame, A=wells, B=ID_list)))

# Add the dilutions if applicable.
if (dilution_bool) {
  sample_locations <- sample_locations %>%
    mutate(Dilutions = -log10(as.numeric(dilutions))) %>%
    unite(B, B:Dilutions, sep="\n")
}

# Run plate_view function which produces a plate view figure.
# This function is from plate_view_function.R.
plate_view(df, sample_locations, wells, plate)

ggsave("plate_view.png", width = 3600, height = 2400, units = "px")
