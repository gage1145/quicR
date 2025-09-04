library(quicR)
library(dplyr)
library(tidyr)

source("~/quicR/R/get_quic.R")
source("~/quicR/R/organize_tables.R")
source("~/quicR/R/convert_tables.R")
source("~/quicR/R/get_real.R")
source("~/quicR/R/calculate_MPR.R")
source("~/quicR/R/calculate_TtT.R")



file <- "input_files/test4.xlsx"
real <- get_real(file)

raw <- get_quic(file)

analyzed <- raw %>%
  group_by(`Sample IDs`, Dilutions, Wells) %>%
  reframe() %>%
  left_join(calculate_MPR(raw)) %>%
  left_join(calculate_TtT(raw, 2))
