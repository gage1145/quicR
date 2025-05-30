install.packages("quicR")
library(quicR)
library(dplyr)



user <- Sys.info()[["user"]]
set.seed(45)

files <- list.files("raw", ".xlsx", full.names = TRUE)

get_data <- function(file) {
  get_real(file)[[1]] %>%
    normalize_RFU()
}

get_meta <- function(file) {
  organize_tables(file) %>%
    convert_tables()
}

meta <- lapply(files, get_meta) %>%
  bind_rows() %>%
  mutate(Dilutions = -log10(as.integer(Dilutions)))

data <- lapply(files, get_data) %>%
  bind_rows()

calcs <- data.frame(
  `Sample IDs` = meta$`Sample IDs`,
  Dilutions = meta$Dilutions,
  check.names = FALSE
)%>%
  mutate(
    MPR = calculate_MPR(data),
    MS  = calculate_MS(data),
    TtT = calculate_TtT(data, 2),
    TtT = ifelse(is.na(TtT), 72, TtT),
    RAF = 1/TtT
  )

write.csv(meta,  paste0(user, "_meta.csv"),  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data,  paste0(user, "_data.csv"),  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(calcs, paste0(user, "_calcs.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), paste0(user, "_sessionInfo.txt"))








