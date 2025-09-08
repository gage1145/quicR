library(quicR)
library(dplyr)
library(tidyr)


file <- "input_files/test3.xlsx"
raw <- get_quic(file, plate=96)
analyzed <- calculate_metrics(raw, "Sample IDs", "Dilutions", "Wells")
plate_view(raw, plot_deriv = F)
plot_metrics(analyzed, "MPR", "MS", "TtT", "RAF", dilution_bool = FALSE)
