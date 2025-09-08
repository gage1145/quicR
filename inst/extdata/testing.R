library(quicR)
library(dplyr)
library(tidyr)


file <- "input_files/test3.xlsx"
raw <- get_quic(file, plate=96)
analyzed <- calculate_metrics(raw, "Sample IDs", "Dilutions", "Wells")

file <- "input_files/raw.csv"
df <- read.csv(file, check.names = FALSE)
  # mutate_at(c("Sample IDs", "Dilutions", "Wells"), as.factor)



plate_view(df, plot_deriv = F)
