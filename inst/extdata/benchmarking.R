library(quicR)
library(QuICSeedR)
library(rtquicR)
library(dplyr)
library(readxl)
library(stringr)
library(rbenchmark)



# QuICSeedR Pipeline ------------------------------------------------------



path <- "input_files/QuICSeedR benchmarking"

bulk <- BulkReadMARS(path, "plate", "raw") %>%
  BulkProcessing(do_analysis = FALSE)

files <- list.files(path, ".xlsx", full.names = TRUE, recursive = TRUE)

raw <- read_xlsx(files[2])
plate <- read_xlsx(files[1])
replicates <- GetReplicate(plate)
meta <- CleanMeta(raw, plate, replicates)
time <- ConvertTime(raw)
clean_raw <- CleanRaw(meta, raw, time, 48)
calcs <- GetCalculation(clean_raw, meta, threshold_method = "bg_ratio", binw = 3)


quicseedr_results <- benchmark(
  BulkReadMARS(path, "plate", "raw") %>%
    BulkProcessing(do_analysis = FALSE),
  GetReplicate(read_xlsx(files[1])),
  CleanMeta(read_xlsx(files[2]), read_xlsx(files[1]), replicates),
  ConvertTime(read_xlsx(files[2])),
  CleanRaw(meta, read_xlsx(files[2]), time, 48),
  GetCalculation(clean_raw, meta, threshold_method = "bg_ratio", binw = 3),
  PlotMetric(calcs),
  PlotPlate(read_xlsx(files[2]), time),
  replications = 50,
  columns = c("test", "elapsed", "replications")
)




# quicR Pipeline ----------------------------------------------------------



bulk_analyze <- function(files) {
  calculate <- function(file) {
    if (!str_detect(file, "384")) {
      calculate_metrics(
        get_real(file)[[1]] |>
          normalize_RFU(),
        organize_tables(file, ifelse(str_detect(file, "384"), 384, 96)) |>
          convert_tables()
      )
    } else return()
  }
  sapply(files, calculate)
}

files <- list.files("input_files", ".xlsx", full.names = TRUE)
file <- files[1]

plate <- read_xlsx(file)
raw <- read_xlsx(file, sheet = 2)
data <- get_real(raw)[[1]]
norm <- normalize_RFU(data)
meta <- convert_tables(organize_tables(file))
analyzed <- calculate_metrics(norm, meta)
locations <- get_sample_locations(file)

quicr_results <- benchmark(
  bulk_analyze(files),
  convert_tables(organize_tables(file)),
  get_real(raw),
  normalize_RFU(data),
  calculate_metrics(norm, meta),
  calculate_MPR(norm),
  calculate_TtT(norm, 2),
  plot_metrics(analyzed),
  get_sample_locations(file),
  plate_view(data, locations),
  replications = 50,
  columns = c("test", "elapsed", "replications")
)



# rtquicR -----------------------------------------------------------------



files <- list.files("input_files/rtquicR benchmarking", "test", full.names = TRUE)
file <- files[1]

batch_rtqr <- function(files) {
  analyze <- function(file) {
    data <- load_quic_results(file, "raw_table", "All Cycles", 11, 2)
    meta <- read_xlsx(file, sheet = 3)
    plot_data <- signal_curve(data, meta)
    lag_data <- calc_lag_phase(data, meta, cutoff = 48)
    calc_AUC_sig(plot_data)
    calc_mpr(plot_data)
  }
  sapply(files, analyze)
}

raw <- load_quic_results(file, "raw_table", "All Cycles", 11, 2)
meta <- read_xlsx(file, sheet = 3)
data <- signal_curve(raw, meta, "baseline_RFU_per_well", 4)
# p <- plot_signal_curve(data)



rtquicR_results <- benchmark(
  load_quic_results(file, "raw_table", "All Cycles", 11, 2),
  batch_rtqr(files),
  calc_mpr(data),
  calc_lag_phase(raw, meta, cutoff = 48),
  plot_signal_curve(data),
  replications = 50,
  columns = c("test", "elapsed", "replications")
)



# Combine -----------------------------------------------------------------



quicseedr_results$package <- "QuICSeedR"
quicr_results$package <- "quicR"
rtquicR_results$package <- "rtquicR"

results <- rbind(quicseedr_results, quicr_results, rtquicR_results)
results$time_per_test <- results$elapsed / results$replications
write.csv(results, "results.csv")
