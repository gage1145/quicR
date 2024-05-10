library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(gridExtra)



# Users will need to adjust this for their own log-in info.
source("functions/functions.R")


# Initialize parameters for downstream functions.###############################
file <- ""
while (file == "") {
  file <- paste0(readline("Please input .xlsx file name WITHOUT extension: "),
                 ".xlsx")
  if (!(file.exists(file))) {
    print("File does not exist. Please ensure file name was typed correctly")
    file <- ""
  }
}

plate <- ""
while (plate == "") {
  plate <-  as.integer(readline("Please enter 96 or 384 for plate type: "))
  if (!(plate == 96) & !(plate == 384)) {
    plate <- ""
  }
}


# Following block exports the plate view figure.################################



# Define the layout using the first sheet in the excel file.
# The sheet should be formatted so that each ID in the "layout" table is unique.
# "organize_tables" is sourced from plate_view_function.R.
df_dic <- organize_tables(file, plate = plate)
IDs <- t(df_dic[["Sample IDs"]])
ID_list <- list()
for (i in IDs) {
  for (j in i) {
    if (!(is.na(j))) {
      ID_list <- append(ID_list, j)
    }
  }
}

# Read in the real-time data.
# "get_real" will return a list of dataframes depending on how many real-time
# measurements the user exported from MARS.
df_list <- get_real(file, ordered=FALSE)

df_id <- as.integer(
  readline(
    paste(
      "There are",
      length(df_list),
      "real-time data sets. Please enter a number in that range: "
    )
  )
)

df <- data.frame(df_list[[df_id]])
df_meta <- get_meta(file)

# Specify the time column.
time_col <- df[, 1]

# Remove the time column and ID row.
df <- df[, -1]

# Set the time column as the df index.
rownames(df) <- time_col

# Get the wells used in the run.
wells <- get_wells(file)

# Take the metadata and apply it into a dataframe for the plate_view function.
sample_locations <- na.omit(do.call(rbind, Map(data.frame, A=wells, B=ID_list)))

# Add replicate number to Sample IDs column.
# Uses add_reps function from functions.R
sample_locations <- add_reps(sample_locations)

# Run plate_view function which produces a plate view figure.
# This function is from plate_view_function.R.
p <- plate_view(df, sample_locations, wells, plate, color = "black")

ggsave("plate_view.png", p, width = 3600, height = 2400, units = "px")



# Following block exports the summary figure.###################################



# Rename rate calculation to "RAF".
df_dic_names <- c(names(df_dic))
for (i in 1:length(df_dic_names)) {
  if (grepl("Rate", df_dic_names[i])) {
    df_dic_names[i] <- "Rate of amyloid formation"
  }
}
names(df_dic) <- df_dic_names

# Convert ID_list to transposed dataframe and name the column "Sample".
ID_list <- t(data.frame(ID_list))
colnames(ID_list) <- c("Sample")

# Initialize the summary df and identify the tibbles needed in df_dic.
df_summary <- list()
columns <- c("Maxpoint",
             "Rate of amyloid formation",
             "Max slope",
             "Time to threshold")

# Fill dataframe with required analyses.
for (i in columns) {
  df <- data.frame(t(as.data.frame.list(df_dic[i])))
  df <- data.frame(column=unlist(df, use.names = FALSE))
  df_summary <- append(df_summary, df)
}
rm(df)

# Convert df_summary to dataframe and remove NAs.
df_summary <- data.frame(df_summary) %>%
  na.omit(df_summary)

# Use columns vector to rename columns in df_summary.
colnames(df_summary) <- columns

# Add the sample ID column to df_summary.
df_summary <- cbind(ID_list, df_summary)

# Group the data based on the Sample ID.
df_summary <- df_summary %>%
  group_by(Sample)

# Ensure all the data is numeric except for Sample column.
df_summary[, -1] <- sapply(df_summary[, -1], as.numeric)

# Create a dataframe with mean and standard deviation.
summary <- df_summary %>%
  group_by(Sample) %>%
  summarise(across(colnames(df_summary)[-1],
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"))

# Count the replicates of each sample and add replicate column.
reps <- c()
for (i in summary$Sample) {
  print(i)
  reps <- rbind(reps, length(which(ID_list == i)))
}
summary <- cbind(summary, reps)

# Find the row with the negative control values.
negative_row <- which(summary == "N")


# Perform the statistical test if negative control is present.
if (length(negative_row) > 0) {
  # Perform T-test for maxpoint values.
  MPR_p_values <- c()
  for (i in 1:nrow(summary)) {
    n <- summary$Maxpoint_mean[i] - summary$Maxpoint_mean[negative_row]
    d <- summary$Maxpoint_sd[i] / sqrt(summary$reps[i])
    p <- pt(q = n / d,
            df=summary$reps[i] - 1,
            lower.tail = FALSE)
    MPR_p_values <- rbind(MPR_p_values, p)
  }

  # Perform t-test for max slope values.
  Max_slope_p_values <- c()
  for (i in 1:nrow(summary)) {
    n <- summary$`Max slope_mean`[i] - summary$`Max slope_mean`[negative_row]
    d <- summary$`Max slope_sd`[i] / sqrt(summary$reps[i])
    p <- pt(q = n / d,
            df=summary$reps[i] - 1,
            lower.tail = FALSE)
    Max_slope_p_values <- rbind(Max_slope_p_values, p)
  }

  # Test whether MPR mean is greater than negative control.
  greater_than_neg <- c()
  for (i in 1:nrow(summary)) {
    greater_than_neg <- rbind(greater_than_neg, summary$Maxpoint_mean[i] >
                                summary$Maxpoint_mean[negative_row])
  }

  # Create a column of the final result from all the metrics.
  result <- c()
  for (i in 1:nrow(summary)) {
    diagnosis <- MPR_p_values[i] < 0.05 &
      Max_slope_p_values[i] < 0.05 &
      greater_than_neg[i]

    result <- rbind(result, if (diagnosis == TRUE) {"Detected"}
                    else {"Not detected"})
  }

  summary <- cbind(summary, MPR_p_values, Max_slope_p_values, greater_than_neg,
                   result)
  pdf("result.pdf", height=8, width=24)
  grid.table(summary)
  dev.off()
  rm(MPR_p_values, Max_slope_p_values, greater_than_neg, n, d, p, reps)
} else {
  print("No negative control is present. Statistical tests unavailable")
}






# Melt the dataframe for the faceting in ggplot.
df_summary <- reshape2::melt(df_summary, id.vars = "Sample")

# Faceted plot of the four analyses.
p <- ggplot(df_summary, aes(x = Sample, y = value, fill = Sample)) +
  scale_fill_discrete() +
  geom_dotplot(binaxis = "y",
               stackdir="center",
               stackratio = 0.5) +
  facet_wrap(vars(variable),
             scales = "free",
             labeller = as_labeller(c(Maxpoint =
                                        "Max RFU / Initial RFU",
                                      `Rate of amyloid formation` =
                                        "RAF (1/s)",
                                      `Max slope` =
                                        "Max Slope (1/s)",
                                      `Time to threshold` =
                                        "Time to Threshold (h)")),
             strip.position = "left") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits = rev) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "lightgrey"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")
p
ggsave("Summary.png", p, width = 3600, height = 2400, units = "px")


# Separate the raw tables and save them to the Excel workbook.
separate_raw(file, 11, TRUE)
