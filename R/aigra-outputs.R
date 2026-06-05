
#' AIGRA output directory
#'
#' @return Path to the backend output directory.
#' @export
aigra_output_dir <- function() {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  file.path(.aigra_env$backend_path, "outputs")
}


#' List AIGRA output files
#'
#' @param pattern File-name pattern.
#'
#' @return A data frame of output files.
#' @export
aigra_outputs <- function(pattern = "aigra(_tabular)?_results_.*\\.(csv|jsonl)$") {
  out_dir <- aigra_output_dir()

  files <- list.files(
    out_dir,
    pattern = pattern,
    full.names = TRUE
  )

  if (!length(files)) {
    return(data.frame())
  }

  info <- file.info(files)

  out <- data.frame(
    file = normalizePath(files, winslash = "/", mustWork = FALSE),
    name = basename(files),
    size = info$size,
    modified = info$mtime,
    stringsAsFactors = FALSE
  )

  out[order(out$modified, decreasing = TRUE), ]
}


#' Get latest AIGRA CSV output path
#'
#' @return Path to latest CSV output.
#' @export
aigra_latest_csv <- function() {
  outs <- aigra_outputs(
    pattern = "aigra(_tabular)?_results_.*\\.csv$"
  )

  if (!nrow(outs)) {
    stop("No AIGRA CSV output found.")
  }

  outs$file[1]
}


#' Read latest AIGRA CSV output
#'
#' @return A data frame containing the latest AIGRA CSV output.
#' @export
aigra_read_latest_csv <- function() {
  csv <- aigra_latest_csv()
  utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)
}

