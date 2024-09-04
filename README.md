---
editor_options: 
  markdown: 
    wrap: 72
---

# quicR

## Description

quicR is a package made for the analysis of real-time quaking-induced
conversion (RT-QuIC) assays.

Much of the functionality is designed to be integrated with the Excel
output files generated by the BMG analysis software, MARS. For best
results, in the MARS software select:

``` mermaid
flowchart TB
excel(Excel Export)

subgraph A[Select the following export options]
  direction LR
  micro(Microplate View)
  table(Table View)
  trans(Transpose Table)
  test(Add test run \n information)
end

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

For example .CSV file, see:

```         
tests -> testthat -> BMG_formatting -> plate_layout.csv
```

The file, “formatted.txt” in the same folder, is how the export should
look to be imported into the BMG software.

### Plate View

Use plate_view_export.R to export a plate view of the real-time data
from an Excel file made in MARS.

For example .CSV files, see:

```         
tests -> testthat -> input_files
```

### Calculations

The program, calculate_metrics.R, will calculate relevant metrics such
as maxpoint ratio, max slope, time-to-threshold, and rate of amyloid
formation.

An Excel file will also be created with the full data and summarized
data. A plot of the summarized data will also be created.

For example .CSV files, see:

```         
tests -> testthat -> input_files
```

## Author

Gage Rowden
