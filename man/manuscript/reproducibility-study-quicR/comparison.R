library(dplyr)
library(readr)
library(purrr)

# Set tolerance for numeric comparisons
tolerance <- 1e-12

# Load all user files (assumes pattern like Gage_meta.csv, Alice_meta.csv, etc.)
files <- list.files("data", ".csv", full.names = TRUE)

# Organize files into a list by user
grouped <- files %>%
  split(gsub("_(meta|data|calcs)\\.csv", "", .)) %>%
keep(~ length(.x) == 3)  # Only keep complete sets

compare_files <- function(user_files) {
  meta <- read_csv(user_files[grep("_meta", user_files)], show_col_types = FALSE)
  data <- read_csv(user_files[grep("_data", user_files)], show_col_types = FALSE)
  calcs <- read_csv(user_files[grep("_calcs", user_files)], show_col_types = FALSE)
  list(meta = meta, data = data, calcs = calcs)
}

# Load data for each user
user_data <- map(grouped, compare_files)

# Choose a reference user (first one alphabetically)
reference_user <- names(user_data)[[1]]
ref <- user_data[[reference_user]]

# Function to compare two data frames
compare_dfs <- function(df1, df2, df_name, user, tol = tolerance) {
  if (!all(names(df1) == names(df2))) {
    message(sprintf("❌ Column names mismatch in '%s' for user %s.", df_name, user))
    return(FALSE)
  }
  if (!all(nrow(df1) == nrow(df2))) {
    message(sprintf("❌ Row count mismatch in '%s' for user %s.", df_name, user))
    return(FALSE)
  }
  is_equal <- map2_lgl(df1, df2, function(col1, col2) {
    if (is.numeric(col1)) all(abs(col1 - col2) < tol, na.rm = TRUE)
    else all(col1 == col2, na.rm = TRUE)
  })
  if (all(is_equal)) {
    message(sprintf("✅ %s matches for user %s.", df_name, user))
    return(TRUE)
  } else {
    mismatch_cols <- names(df1)[!is_equal]
    message(sprintf("⚠️ Differences in columns of '%s' for user %s: %s",
                    df_name, user, paste(mismatch_cols, collapse = ", ")))
    return(FALSE)
  }
}

# Compare each user against the reference
for (user in names(user_data)) {
  for (ref_user in setdiff(names(user_data), user)) {
    user_dfs <- user_data[[user]]
    compare_dfs(user_data[[ref_user]]$meta, user_dfs$meta, "meta", paste(user, "\tvs", ref_user))
    compare_dfs(user_data[[ref_user]]$data, user_dfs$data, "data", paste(user, "\tvs", ref_user))
    compare_dfs(user_data[[ref_user]]$calcs, user_dfs$calcs, "calc", paste(user, "\tvs", ref_user))
  }
}
