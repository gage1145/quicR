# quicR 1.1.1

-   Fixed organize_tables function to accept Excel files which do not have the metadata preamble. Will not affect output if file still has the preamble.
-   Sped up the get_meta function by having it read in sheet 1 from the Excel file which typically has much less data to read in.

# quicR 1.1.0

-   Added a new function "transpose_real.R" which takes the output of "get_real.R" and transposes it. This format makes it such that each read is a column labelled with the time of the read.

-   Removed the transposition sub-function from "normalize_RFU.R" to be its own separate function.

-   Updated example scripts to incorporate the new transposition function.

# quicR 1.0.3

-   Added janitor as a dependency.
-   Added a new test file which includes multiple real-time datasets. This is useful for the get_real function since the output returns a list of data frames if there are multiple real-time reads.
-   Converted many of the functions to be more in line with functional programming paradigm.
-   Included a documentation file that fixes the NOTE when using "." in a dplyr pipeline.
-   Fixed bugs in the example scripts.

# quicR 1.0.2

# quicR 1.0.1

# quicR 1.0.0

# quicR 0.1.1

-   Initial CRAN submission.
