library(stringr)
library(tibble)

# List and read all session info files
session_files <- list.files("data", pattern = "_sessionInfo\\.txt$", full.names = TRUE)

parse_session_info <- function(file) {
  lines <- readLines(file)
  user <- sub("_sessionInfo\\.txt", "", file) %>%
    str_remove("data/")
  
  r_version <- str_extract(lines[1], "R version [^ ]+") %>%
    str_split_i(" ", 3)
  platform  <- str_extract(lines[grepl("Platform:", lines)], "Platform: (.+)") %>% 
    str_remove("Platform: ")
  OS <- str_remove(lines[3], "Running under: ")
  
  # Extract loaded package versions (simple approach)
  pkg_lines <- lines[which(grepl("^\\s*(quicR|dplyr|base)\\s", lines))]
  base_pkg <- lines[match("attached base packages:", lines) + 1]
  attached <- lines[match("other attached packages:", lines) + 1]
  
  pkg_info <- str_split(attached, " ")
  quicR_version <- pkg_info[[1]][which(str_detect(pkg_info[[1]], "quicR"))] %>%
    str_split_i("_", 2)
  
  
  pkg_df <- tibble(
    user = user,
    r_version = r_version,
    quicR_version = quicR_version,
    platform = platform,
    os = OS
  )
  
  return(pkg_df)
}

# Combine all into one table
session_summary <- data.frame(
  user_id = seq(1, length(session_files)),
  do.call(rbind, lapply(session_files, parse_session_info))
)

# View by user
session_summary

write.csv(session_summary, "session_summary.csv", row.names = FALSE)
