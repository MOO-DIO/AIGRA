#' Generate items from a tabular item bank
#'
#' Runs the AIGRA generation, solver, critic, and export pipeline using a CSV or Excel item bank.
#'
#' @param file_path Path to CSV or Excel item bank.
#' @param provider LLM provider, such as "gemini", "openai", "groq", or "anthropic".
#' @param model Provider model name.
#' @param source_language Language of the source item bank.
#' @param target_language Language for generated items.
#' @param review_language Language for review comments.
#' @param subject Subject name.
#' @param exam Examination or item-bank name.
#' @param n_clones Number of clones per source item.
#' @param max_items Maximum number of valid source items to process. Use NULL to process all valid items.
#' @param output_dir Optional output directory.
#' @param read_csv If TRUE, returns the CSV output as a data frame.
#'
#' @return A data frame if read_csv is TRUE; otherwise invisibly returns the CSV path.
#' @export
aigra_generate_tabular_items <- function(
  file_path,
  provider = "gemini",
  model = "gemini-3.1-pro-preview",
  source_language = "English",
  target_language = "English",
  review_language = "English",
  subject = "General",
  exam = "Item Bank",
  n_clones = 1,
  max_items = NULL,
  output_dir = NULL,
  read_csv = TRUE
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  file_path <- normalizePath(
    file_path,
    winslash = "/",
    mustWork = TRUE
  )

  message("AIGRA generation file: ", file_path)

  preview <- aigra_parse_tabular_items(
    file_path = file_path,
    source_language = source_language,
    subject = subject,
    exam = exam
  )

  message("Rows visible to AIGRA before generation: ", nrow(preview))

  if (is.null(output_dir)) {
    output_dir <- file.path(.aigra_env$backend_path, "outputs")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  output_dir <- normalizePath(
    output_dir,
    winslash = "/",
    mustWork = TRUE
  )

  start_time <- Sys.time()

  orchestrator <- reticulate::import("aigra_backend.orchestrator")

  py_max_items <- if (is.null(max_items)) {
    reticulate::py_none()
  } else {
    as.integer(max_items)
  }

  orchestrator$run_aigra_tabular_pipeline(
    file_path = file_path,
    provider = provider,
    model = model,
    source_language = source_language,
    target_language = target_language,
    review_language = review_language,
    subject = subject,
    exam = exam,
    n_clones = as.integer(n_clones),
    max_items = py_max_items,
    output_dir = output_dir
  )

  csv_files <- list.files(
    output_dir,
    pattern = "aigra_tabular_results_.*\\.csv$",
    full.names = TRUE
  )

  if (!length(csv_files)) {
    stop("No tabular AIGRA CSV output was produced.")
  }

  info <- file.info(csv_files)

  candidates <- csv_files[info$mtime >= (start_time - 10)]

  if (!length(candidates)) {
    candidates <- csv_files
  }

  candidate_info <- file.info(candidates)
  latest <- candidates[order(candidate_info$mtime, decreasing = TRUE)][1]

  latest <- normalizePath(latest, winslash = "/", mustWork = TRUE)

  message("CSV produced by this generation run: ", latest)

  if (read_csv) {
    return(utils::read.csv(latest, stringsAsFactors = FALSE, check.names = FALSE))
  }

  invisible(latest)
}
