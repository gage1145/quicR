library(quicR)
library(QuICSeedR)
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
    calculate_metrics(
      get_real(file)[[1]] |>
        normalize_RFU(),
      organize_tables(file, ifelse(str_detect(file, "384"), 384, 96)) |>
        convert_tables()
    )
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
  plot_metrics(analyzed),
  get_sample_locations(file),
  plate_view(data, locations),
  replications = 50,
  columns = c("test", "elapsed", "replications")
)



# Combine -----------------------------------------------------------------



quicseedr_results$package <- "QuICSeedR"
quicr_results$package <- "quicR"

results <- rbind(quicseedr_results, quicr_results)
results$time_per_test <- results$elapsed / results$replications
write.csv(results, "results.csv")
