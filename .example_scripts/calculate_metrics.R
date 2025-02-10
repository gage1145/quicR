library(ggpubr)
library(openxlsx)
library(agricolae)
library(tidyverse)
library(quicR)



# Initialize the file and read in the data --------------------------------



# For testing, use "test".
# Request the user to input the file name for analysis.
file <- ""
while (file == "") {
  file <- paste0(
    readline(
      "Please input the Excel file name without .xlsx extension: "
    ),
    ".xlsx"
  )
  if (!file.exists(file)) {
    print(paste(file, "does not exist. Please enter a valid file name."))
    file <- ""
  }
}

# Ask the user for the threshold.
threshold <- "Please enter the desired threshold for RAF calculation: " %>%
  readline() %>%
  as.integer()



# Identify the raw real-time data -----------------------------------------



# Get the real-time data from the BMG export file.
df <- quicR::get_real(file, ordered = FALSE)

# Ask the user which real-time data set they want to use.
df_id <- ifelse(
  length(df) > 1,
  paste(
    "There are",
    length(df),
    "real-time data sets. Please enter a number in that range: "
  ) %>%
    readline() %>%
    as.integer(),
  1
)

# Select the real-time data set that the user signified.
df <- df[[df_id]]



# Get the metadata of each sample -----------------------------------------



# Export the tables in the first sheet of the file.
dic <- file %>%
  quicR::organize_tables() %>%
  quicR::convert_tables()



# Calculate the normalized real-time data ---------------------------------



df_norm <- df %>%
  transpose_real() %>%
  normalize_RFU(transposed = TRUE)



# Add dilution factors if applicable ---------------------------------------



# Determine if there is a dilutions table.
dilution_bool <- "Dilutions" %in% names(dic)



# Calculate the relevant metrics ------------------------------------------



# Define the number of hours that the rxn ran for.
hours <- as.numeric(colnames(df_norm)[ncol(df_norm)])

# Initialized the dataframe with the calculated metrics.
df_analyzed <- calculate_metrics(df_norm, dic) %>%
  mutate(
    Dilutions = if (dilution_bool) -log10(as.numeric(dilutions)),
    crossed = TtT != hours
  ) %>%
  rename(Sample_ID = `Sample IDs`) %>%
  arrange(Sample_ID)



# Summarize the data ------------------------------------------------------



# Create a summary data frame.
summary <- (
  if (dilution_bool) {
    summary <- df_analyzed %>%
      group_by(Sample_ID, Dilutions)
  } else {
    summary <- df_analyzed %>%
      group_by(Sample_ID)
  }) %>%
  summarise(
    reps      = n(),
    mean_TtT  = mean(TtT),
    mean_RAF  = mean(RAF),
    mean_MPR  = mean(MPR),
    mean_MS   = mean(MS),
    thres_pos = sum(crossed) / n() > 0.5
  )



# Run the statistical analysis against the negative control ---------------



metrics <- c("MPR", "MS")
for (metric in metrics) {
  formula <- as.formula(
    paste0(
      metric, " ~ Sample_ID", ifelse(dilution_bool, " + Dilutions", "")
    )
  )
  # Create a dataframe of the individual comparisons.
  comps <- LSD.test( # Perform the post-hoc multiple comparisons test.
    # Create the statistical model using ANOVA.
    aov(formula = formula, data = df_analyzed),
    trt = if (dilution_bool) c("Sample_ID", "Dilutions") else "Sample_ID",
    p.adj = "none", group = FALSE
  )[["comparison"]]

  # Initialize columns which will hold unique IDs for each sample compared.
  comps <- comps %>%
    cbind(
      rownames(comps) %>%
        strsplit(" - ") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame()
    ) %>%
    select(-c("difference", "LCL", "UCL")) %>%
    # Remove all comparisons that are not against "N".
    subset(
      V1 == "N" |
        str_detect(V1, "N:") |
        str_detect(V1, "N_") |
        V2 == "N" |
        str_detect(V2, "N:") |
        str_detect(V2, "N_")
    ) %>%
    mutate(
      V1_dilutions = str_split_i(V1, ":", 2),
      V1 = str_split_i(V1, ":", 1),
      V2_dilutions = str_split_i(V2, ":", 2),
      V2 = str_split_i(V2, ":", 1)
    ) %>%
    rename(
      "{metric}_pvalue" := pvalue,
      "{metric}_significance" := signif.
    ) %>%
    mutate(
      V1 = replace(V1, V1 == "N" | str_detect(V1, "N_"), NA),
      V2 = replace(V2, V2 == "N" | str_detect(V2, "N_"), NA),
      V1_dilutions = ifelse(is.na(V1), NA, V1_dilutions),
      V2_dilutions = ifelse(is.na(V2), NA, V2_dilutions)
    ) %>%
    unite(
      Sample_ID,
      c("V1", "V2"),
      sep = "",
      na.rm = TRUE
    ) %>%
    unite(
      Dilutions,
      c("V1_dilutions", "V2_dilutions"),
      sep = "",
      na.rm = TRUE
    ) %>%
    rbind(c(NA, NA, "N", -3)) %>%
    mutate_at(c(1, 4), as.double)

  summary <- left_join(summary, comps)
}

summary <- summary %>%
  mutate(Positive = thres_pos & MPR_pvalue <= 0.05 & MS_pvalue <= 0.05)



# Save the data to an Excel workbook --------------------------------------



# Initialize the workbook for Excel.
wb <- createWorkbook()

# Add the sheets.
addWorksheet(wb, "Total")
addWorksheet(wb, "Summary")

# Write the "summary" df to the "Summary" sheet.
writeData(wb, "Total", df_analyzed)
writeData(wb, "Summary", summary)

# Save the Excel file.
saveWorkbook(wb, "summary.xlsx", overwrite = TRUE)



# Plot the metrics in a facet plot ----------------------------------------



df_analyzed %>%
  select(-crossed) %>%
  plot_metrics("Sample_ID", "Dilutions", dilution_bool = dilution_bool) +
  geom_dotplot(
    binaxis = "y",
    stackdir = "center",
    dotsize = 0.5,
    position = "dodge",
    stackratio = 0.5
  )

ggsave("summary.png", width = 4000, height = 2500, units = "px")
