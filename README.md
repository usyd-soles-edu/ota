
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ota

<!-- badges: start -->

<!-- badges: end -->

Online Teaching Allocation (OTA) roster management tool for The
University of Sydney. This R package streamlines the process of managing
teaching rosters, tracking changes, and generating reports for teaching
staff assignments. It is designed for teaching staff and coordinators
who need to manage and track changes to teaching rosters, handling the
comparison of rosters as they are updated, identifying changes, and
producing reports for submission to the University’s Online Teaching
Allocation (OTA) system.

If you are not from USYD there is probably not much here for you!

## Installation

You can install the development version of ota from
[GitHub](https://github.com/usyd-soles-edu/ota) with:

``` r
# install.packages("pak")
pak::pak("usyd-soles-edu/ota")
```

## Quick Start

Here’s a typical workflow for managing rosters:

``` r
library(ota)

# Load roster from Excel file
df <- roster("path/to/roster.xlsx", unit = "biol1007")

# Create a snapshot to record current state
snapshot(df)

# ... later, after roster updates ...

# Load updated roster
df_new <- roster("path/to/roster.xlsx", unit = "biol1007")

# Create new snapshot
snapshot(df_new)

# Compare the two snapshots to identify changes
result <- compare(df_new)

# View formatted change summary
document_changes(result)

# Add paycode information based on staff attributes
df_new <- add_paycodes(df_new)
```

## Main Functions

- `roster()` - Load and process roster data from Excel
- `snapshot()` - Save timestamped roster snapshots
- `compare()` - Detect changes between roster snapshots
- `document_changes()` - Display formatted change summaries
- `add_paycodes()` - Assign paycodes to staff members

## Notes

You’ll still need to render `README.Rmd` regularly to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
