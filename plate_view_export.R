library(readxl)
library(dplyr)
library(ggplot2)
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
df_dic <- quicR::organize_tables(file, plate = plate)

IDs <- df_dic[["Sample IDs"]] |>
  t() |>
  as.data.frame() |>
  tidyr::gather() |>
  dplyr::select(value)

# IDs <- t(df_dic[["Sample IDs"]])
# ID_list <- list()
# for (i in IDs) {
#   for (j in i) {
#     if (!(is.na(j))) {
#       ID_list <- append(ID_list, j)
#     }
#   }
# }

# Determine if there is a dilutions table.
dilution_bool <- "Dilutions" %in% names(df_dic)

# Add dilution factors if applicable.
if (dilution_bool) {
  dilutions <- df_dic[["Dilutions"]] |>
    t() |>
    as.data.frame() |>
    tidyr::gather() |>
    dplyr::select(value) |>
    dplyr::mutate(value = -log10(as.numeric(value)))
  # for (i in t(df_dic[["Dilutions"]])) {
  #   for (j in i) {
  #     if (!is.na(j)) {
  #       dilutions <- rbind(dilutions, j)
  #     }
  #   }
  # }
}

# Read in the real-time data.
# "get_real" will return a list of dataframes depending on how many real-time
# measurements the user exported from MARS.
df_list <- quicR::get_real(file, ordered=FALSE)

df_id <- as.integer(
  readline(
    paste(
      "There are",
      length(df_list),
      "real-time data sets. Please enter a number in that range: "
    )
  )
)

df <- as.data.frame(df_list[[df_id]])

# Specify the time column.
time_col <- df[, 1]

# Remove the time column and ID row.
df <- df[, -1]

# Set the time column as the df index.
rownames(df) <- time_col

# Get the wells used in the run.
wells <- quicR::get_wells(file)

# Take the metadata and apply it into a dataframe for the plate_view function.
sample_locations <- do.call(rbind, Map(data.frame, A = wells, B = ID_list)) %>%
  na.omit()

# Add the dilutions if applicable.
if (dilution_bool) {
  sample_locations <- sample_locations %>%
    mutate(Dilutions = -log10(as.numeric(dilutions))) %>%
    unite(B, B:Dilutions, sep = "\n")
}

# Run plate_view function which produces a plate view figure.
quicR::plate_view(df, sample_locations, plate)

ggsave("plate_view.png", width = 3600, height = 2400, units = "px")
