#' Read an AIGRA template from CSV or XLSX
#'
#' Reads an AIGRA tabular item-bank template into R as a data frame. CSV files
#' are read as UTF-8 by default. XLSX files require the readxl package.
#'
#' @param path Path to a CSV, XLSX, or XLS template file.
#' @param sheet Sheet name or sheet index for XLSX files. Defaults to the first sheet.
#' @param fileEncoding Encoding used for CSV files. Defaults to UTF-8.
#' @param ... Additional arguments passed to the underlying reader.
#'
#' @return A data frame with the standard AIGRA template columns.
#' @export
#'
#' @examples
#' \dontrun{
#' template <- aigra_read_template("template.csv")
#' template <- aigra_read_template("template.xlsx")
#' }
aigra_read_template <- function(path,
                                sheet = 1,
                                fileEncoding = "UTF-8",
                                ...) {
  if (missing(path) || !nzchar(path)) {
    stop("Please supply a CSV or XLSX template path.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("The template file does not exist: ", path, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    out <- utils::read.csv(
      path,
      fileEncoding = fileEncoding,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      ...
    )
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Reading XLSX files requires the readxl package. Install it with: install.packages('readxl')",
        call. = FALSE
      )
    }

    out <- as.data.frame(
      readxl::read_excel(path, sheet = sheet, ...),
      stringsAsFactors = FALSE
    )

    names(out) <- trimws(names(out))
  } else {
    stop(
      "Unsupported template file type: .", ext,
      ". Please use .csv, .xlsx, or .xls.",
      call. = FALSE
    )
  }

  .aigra_validate_tabular_data(out)
}
