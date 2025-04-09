library(quicR)
library(tidyverse)
library(janitor)
library(readxl)

functions <- list.files("R", full.names = TRUE)
sapply(functions, source)
files <- list.files("inst/extdata/input_files", ".xlsx", full.names = TRUE)
df_list <- sapply(files, get_real)
norm_list <- sapply(df_list, normalize_RFU)
tabs <- sapply(files, organize_tables)


time_change <- function(fun1, fun2) {
  start1 <- Sys.time()
  fun1
  time1 <- as.numeric(Sys.time() - start1)

  start2 <- Sys.time()
  fun2
  time2 <- as.numeric(Sys.time() - start2)

  paste0("time decreased by ", round(100 * (1 - (time1 / time2)), 2), "%")
}




# get_real ----------------------------------------------------------------



get_real_delta <- paste(
  "get_real",
  time_change(
    sapply(df_list, get_real),
    sapply(df_list, quicR::get_real)
  )
)



# normalize_RFU -----------------------------------------------------------



normalize_RFU_delta <- paste(
  "normalize_RFU",
  time_change(
    sapply(df_list, normalize_RFU),
    sapply(df_list, quicR::normalize_RFU)
  )
)



# calculate_TtT -----------------------------------------------------------



calculate_TtT_delta <- paste(
  "calculate_TtT",
  time_change(
    sapply(norm_list, calculate_TtT, threshold = 2),
    sapply(norm_list, quicR::calculate_TtT, threshold = 2)
  )
)



# organize_tables ---------------------------------------------------------



organize_tables_delta <- paste(
  "organize_tables",
  time_change(
    sapply(files, organize_tables),
    sapply(files, quicR::organize_tables)
  )
)



# convert_tables ----------------------------------------------------------



convert_tables_delta <- paste(
  "convert_tables",
  time_change(
    sapply(tabs, convert_tables),
    sapply(tabs, quicR::convert_tables)
  )
)



# get_meta ----------------------------------------------------------------



get_meta_delta <- paste(
  "get_meta",
  time_change(
    sapply(files, get_meta),
    sapply(files, quicR::get_meta)
  )
)



# Show change in time. ----------------------------------------------------


cat(
  get_real_delta,
  "\n",
  normalize_RFU_delta,
  "\n",
  calculate_TtT_delta,
  "\n",
  organize_tables_delta,
  "\n",
  convert_tables_delta,
  "\n",
  get_meta_delta,
  "\n"
)

