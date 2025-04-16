# quicR <img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/quicR)](https://cran.r-project.org/package=quicR)
[![R-CMD-check](https://github.com/gage1145/quicR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gage1145/quicR/actions/workflows/R-CMD-check.yml)

<!-- badges: end -->

## Description
Real-time quaking induced conversion (RT-QuIC) has quickly become a valuable diagnostic tool for protein misfolding disorders such as Creutzfeldt-Jakob disease and Parkinson's disease. Given that the technology is relatively new, academic and industry standards for quality filtering data and high throughput analysis of results have yet to be fully established. The open source R library, quicR, was developed to provide a standradized approach to RT-QuIC data analysis. quicR provides functions, which can be easily integrated into existing R workflows, for data curation, analysis, and vizualization.

![Workflow hierarchy of the **quicR** package. Blue nodes indicate steps where BMG software is needed. Purple nodes indicate functions dedicated to handling metadata. Red nodes are functions that acquire and manipulate raw data. Orange nodes are functions which calculate some metric. Finally, yellow nodes represent data analysis endpoints.](man/manuscript/images/workflow.png)

export(Export report to Excel)

excel --> A
micro --> table --> trans --> test
A --> export
```

Having both the microplate view and the table view helps with
integrating plate layouts and the real-time data.

For many of the functions, it is important to have a table labelled
“Sample IDs” on the microplate view sheet of the Excel file.

## Examples

### Plate IDs for BMG Import

Use BMG_format.R script to format a .CSV file with a Sample IDs in a
plate view into a format importable into the BMG control software.

For example .CSV file, see ```tests/testthat/BMG_formatting/plate_layout.csv```

The file, “formatted.txt” in the same folder, is how the export should
look to be imported into the BMG software.

### Importing Raw Real-Time Data

Getting the real-time data from the MARS export is quite simple. Normalization can also be performed on the data.

``` R
# Import the raw data
df_ <- get_real("file.xlsx")

# Normalize the data based on the initial background reading.
df_norm <- normalize_RFU(df_)
```

### Importing Metadata

The MARS software exports microplate views of the samples with information that the user chose. Often, you will want to ensure that you included "Sample IDs" in that export. To import the metadata into your environment, run the following:

``` R
# Import the metadata as a named list of tables.
tabs <- organize_tables("file.xlsx", plate = 96)

# Convert the tables into dataframe columns
df_ <- convert_tables(tabs)
```

Additionally, MARS will also export the run metadata which is not sample dependent. This includes information such as the date, run ID, user ID, etc. To get this information run the following:

``` R
get_meta("file.xlsx")
```

If you need to know which wells were used in the plate, run:

``` R
wells <- get_wells("file.xlsx")
```

### Plate View

The function, ```plate_view``` exports a useful figure for quickly analyzing real-time data. It plots every sample in an 8x12 or 16x24 faceted grid for 96-well and 384-well plates, respectively.

Use ```.example_scripts/plate_view_export``` to export a plate view of the real-time data from an Excel file made in MARS.

### Calculations

quicR provides functions for calculating kinetic information from real-time data. These metrics include:
1. Maxpoint ratio: ```calculate_MPR```
2. Maximum slope: ```calculate_MS```
3. Time-to-threshold: ```calculate_TtT```

There is no function included for rate of amyloid formation (RAF) since that can be calculated as the inverse of time-to-threshold.

The script provided at ```.example_scripts/calculate_metrics.R``` will calculate these metrics.

An Excel file will also be created with the full data and summarized
data. A plot of the summarized data will also be created.

## Example Files
For example .xlsx files, see: 
```
inst/extdata/input_files/
```

## Installation
``` R
# For latest release
install.packages("quicR")

# For development version
devtools::install_github("https://github.com/gage1145/quicR")
```

## Author

Gage Rowden
