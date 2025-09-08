library(quicR)

# source("../../R/calculate_MPR.R")
# source("../../R/calculate_TtT.R")
# source("../../R/calculate_MS.R")
# source("../../R/calculate_metrics.R")



file <- "input_files/test384.xlsx"
raw <- get_quic(file, plate=96)
analyzed <- calculate_metrics(raw, "Sample IDs", "Dilutions", "Wells")
plate_view(raw)
