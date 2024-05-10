library(tidyverse)
library(ggplate)
library(stringr)



source("C:/Users/rowde002/Box/Shiny App/standalone_scripts/use_this/R/functions.R")



# Load in the tables from the MARS output.
df_dic <- organize_tables("20240213_peter_premix.xlsx")

# Rename rate calculation to "RAF".
df_dic_names <- c(names(df_dic))
for (i in 1:length(df_dic_names)) {
  if (grepl("Rate", df_dic_names[i])) {
    df_dic_names[i] <- "Rate of amyloid formation"
  }
}
names(df_dic) <- df_dic_names

# Create the well IDs
wells <- c()
for (i in 1:12) {
  for (j in LETTERS[1:8]) {
    wells <- rbind(wells, paste0(j, as.character(i)))
  }
}

# Get the maxpoint values
MPR_values <- c()
for (i in df_dic[["Maxpoint"]]) {
  for (j in i) {
    MPR_values <- rbind(MPR_values, j)
  }
}

# Get the RAF values
RAF_values <- c()
for (i in df_dic[["Rate of amyloid formation"]]) {
  for (j in i) {
    RAF_values <- rbind(RAF_values, j)
  }
}

# Get the max slope values
MS_values <- c()
for (i in df_dic[["Max slope"]]) {
  for (j in i) {
    MS_values <- rbind(MS_values, j)
  }
}

# Get the sample IDs
labels <- c()
for (i in df_dic[["Sample IDs"]]) {
  for (j in i) {
    # Wrap the text if it has a "_" as a delimiter
    labels <- rbind(labels, stringr::str_wrap(gsub("_", " ", j), width = 3))
  }
}

# Convert to tibble so plate_plot can understand the data.
df <- as_tibble(cbind(wells, MPR_values, RAF_values, MS_values, labels))
colnames(df) <- c("wells", "MPR_values", "RAF_values", "MS_values", "labels")

# Convert relevant columns to numeric.
df$MPR_values <- as.numeric(df$MPR_values)
df$RAF_values <- as.numeric(df$RAF_values)
df$MS_values <- as.numeric(df$MS_values)


# Generate a new graphics device with a defined size
png("MPR.png", width = 4800, height = 3200, unit = "px", res = 300)

plate_plot(
  data = df,
  title = "Maxpoint Plate View",
  position = wells,
  value = MPR_values,
  label = labels,
  colour = c(
    "darkblue",
    "lightblue"),
  plate_type = "round",
  label_size = 2,
  scale = 3
)

# Close graphics device
dev.off()

# Generate a new graphics device with a defined size
png("RAF.png", width = 4800, height = 3200, unit = "px", res = 300)

plate_plot(
  data = df,
  title = "Maxpoint Plate View",
  position = wells,
  value = RAF_values,
  label = labels,
  colour = c(
    "darkblue",
    "lightblue"),
  plate_type = "round",
  label_size = 2,
  scale = 3
)

# Close graphics device
dev.off()

# Generate a new graphics device with a defined size
png("MS.png", width = 4800, height = 3200, unit = "px", res = 300)

plate_plot(
  data = df,
  title = "Maxpoint Plate View",
  position = wells,
  value = MS_values,
  label = labels,
  colour = c(
    "darkblue",
    "lightblue"),
  plate_type = "round",
  label_size = 2,
  scale = 3
)

# Close graphics device
dev.off()
