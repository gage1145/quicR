# library(quicR)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(purrr)
library(zoo)
library(ggplot2)

source("../../R/get_quic.R")
source("../../R/organize_tables.R")
source("../../R/convert_tables.R")
source("../../R/get_real.R")
source("../../R/calculate_MPR.R")
source("../../R/calculate_TtT.R")
source("../../R/calculate_MS.R")
source("../../R/calculate_metrics.R")
source("../../R/plate_view.R")
source("../../R/plot_metrics.R")



file <- "input_files/test4.xlsx"
raw <- get_quic(file)

analyzed <- calculate_metrics(raw, `Sample IDs`, Dilutions, Wells)

plot_metrics(analyzed, "MPR", "MS", "TtT", "RAF", dilution_bool = F)



# plate_view(raw)
#
