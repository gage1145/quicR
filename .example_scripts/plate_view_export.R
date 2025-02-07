# require(readxl)
require(tidyverse)
require(quicR)



# Initialize parameters for downstream functions. -------------------------



file <- ""
while (file == "") {
  file <- paste0(
    readline("Please input .xlsx file name WITHOUT extension: "),
    ".xlsx"
  )
  if (!(file.exists(file))) {
    print("File does not exist. Please ensure file name was typed correctly")
    file <- ""
  }
}

plate <- 0
while (plate == 0) {
  plate <- as.integer(readline("Please enter 96 or 384 for plate type: "))
  if (plate != 96 & plate != 384) {
    plate <- 0
  }
}



# Following block exports the plate view figure. --------------------------



# Define the layout using the first sheet in the excel file.
# The sheet should be formatted so that each ID in the "layout" table is unique.
df_dic <- quicR::organize_tables(file, plate = plate)

# Determine if there is a dilutions table.
dilution_bool <- "Dilutions" %in% names(df_dic)

# Read in the real-time data.
# get_real will return a list of dataframes depending on how many real-time
# measurements the user exported from MARS.
df_ <- quicR::get_real(file, ordered = FALSE)

df_id <- ifelse(
  length(df_) > 1,
  as.integer(
    readline(
      paste(
        "There are",
        length(df_),
        "real-time data sets. Please enter a number in that range: "
      )
    )
  ),
  1
)

df_ <- df_[[df_id]] %>%
  as.data.frame()

sample_locations <- get_sample_locations(
  file = file,
  tab_name = "Sample IDs",
  dilution_bool = dilution_bool,
  dilution_fun = function(x) -log10(x),
  sep = "\n",
  plate = plate
)



# Run plate_view function which produces a plate view figure. -------------



quicR::plate_view(df_, sample_locations, plate) +
  ggtitle(str_split_i(file, "\\.", 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plate_view.png", width = 3600, height = 2400, units = "px")
