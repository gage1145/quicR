# quicR 2.1.3

## Updated README
- Removed the mermaid diagram and replaced it with a workflow diagram image and a screenshot of the window where export selections are made.
- Added a plate view section with example figure.
- Added a section explaining key calculated metrics such as maxpoint ratio, max slope, time-to-threshold, and rate of amyloid formation.
- Included examples with newly created functions that were not present in previous versions such as `calculate_metrics` and `plot_metrics`.
- Added a funding section.
- Added a dependency section with hyperlinks to all dependencies.
- Updated the author section.

# quicR 2.1.2

## Optimizations

Several functions have been optimized using vectorization. Forgive me, I wrote a lot of the functions when I was still learning R.

-   `get_real` time decreased by **0.58%**
-   `normalize_RFU` time decreased by **98.03%**
-   `calculate_TtT` time decreased by **1.55%**
-   `organize_tables` time decreased by **3.44%**
-   `convert_tables` time decreased by **70.89%**
-   `get_meta` time decreased by **26.93%**

# quicR 2.1.1

## Bug fixes

-   Fixed an issue where `plot_metrics` and `calculate_metrics` would not work if the input data frames did not include dilution factors.

# quicR 2.1.0

## New features

-   Included a new function `calculate_metrics` which will make calls to the "calculate" family of quicR functions, and return a data frame of all the metrics passed to the function.
-   Added the `plot_metrics` function which accepts the output of the `calculate_metrics` function and returns a faceted plot of the calculated metrics.

# quicR 2.0.0

## Bug fixes

-   Adjusted the delimiter in `get_meta` so that the second column doesn't have a leading white space.
-   Fixed example scripts to work with the updated functions.
-   Now suppressing messages from `get_wells`. This wasn't necessarily a bug, but was not ideal and the messages didn't provide any relevant information about what the function was doing.
-   Fixed a typo in `get_wells` which caused the function to assigning a data frame as a variable instead of returning the data frame.
-   Fixed a bug where the plate view would not render properly if the plate was not completely full.

## New features

-   Provided the `calculate_threshold` function for calculating detection thresholds using a common method done in the literature.
-   Added a `sep` argument to `get_sample_locations` for delimiting sample IDs and dilution factors.
-   Converted the `calculate_MS` function to a vectorized function, and instead of using `lm`, the function looks at differences within the moving window. This provides a much faster calculation, but does reduce the accuracy slightly. May provide the option to do either methods in the future.
-   Added the default option "Sample IDs" for `tab_name` in the `get_sample_locations` function.

## Miscellaneous

-   Made the quicR logo!
-   Provided better test files which have more ideal RT-QuIC data.
-   Now includes the manuscript in the man folder which will be submitted for publication.

# quicR 1.1.1

-   Fixed organize_tables function to accept Excel files which do not have the metadata preamble. Will not affect output if file still has the preamble.
-   Sped up the get_meta function by having it read in sheet 1 from the Excel file which typically has much less data to read in.

# quicR 1.1.0

-   Added a new function "transpose_real.R" which takes the output of "get_real.R" and transposes it. This format makes it such that each read is a column labelled with the time of the read.

-   Removed the transposition sub-function from "normalize_RFU.R" to be its own separate function.

-   Updated example scripts to incorporate the new transposition function.

# quicR 1.0.3

-   Added janitor as a dependency.
-   Added a new test file which includes multiple real-time data-sets. This is useful for the get_real function since the output returns a list of data frames if there are multiple real-time reads.
-   Converted many of the functions to be more in line with functional programming paradigm.
-   Included a documentation file that fixes the NOTE when using "." in a dplyr pipeline.
-   Fixed bugs in the example scripts.

# quicR 1.0.2

# quicR 1.0.1

# quicR 1.0.0

# quicR 0.1.1

-   Initial CRAN submission.
