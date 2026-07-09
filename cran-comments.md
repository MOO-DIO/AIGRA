## Test environments

* local Windows installation, R release
* `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 0 significant notes

## Package update

This is a user-interface update to AIGRA.

The update changes the generation workflow so that item generation uses an R
data frame as input. Users can read CSV or XLSX templates into R using
`aigra_read_template()` and then pass the resulting object to
`aigra_generate_items(data = ...)`.

This change addresses file-path and encoding issues observed during external
testing with non-English item-bank templates. The wrapper now validates required
AIGRA tabular columns in R and writes a temporary UTF-8 CSV internally before
calling the Python backend.

Changes in this version:

* `aigra_generate_items()` now accepts a data frame through `data = ...`.
* Added `aigra_read_template()` for CSV and XLSX templates.
* Added `aigra_generate_from_data()` as an alias for data-frame input.
* Added internal UTF-8 temporary CSV writing before the Python backend is called.
* Added validation for required AIGRA tabular columns.

Note: this update removes the earlier direct `file_path` argument from
`aigra_generate_items()` in favour of a data-frame-only interface.

All examples that require external API calls remain wrapped in `\dontrun{}`.
