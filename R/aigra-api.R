
#' Set the AIGRA Python backend
#'
#' Connects the R package to the AIGRA Python backend.
#'
#' @param backend_path Path to the AIGRA_BACKEND folder.
#'
#' @return Invisibly returns backend configuration.
#' @export
aigra_set_backend <- function(
  backend_path = NULL
) {
  aigra_use_backend(backend_path = backend_path)
}


#' Check AIGRA backend status
#'
#' Checks whether the R package can access the Python backend.
#'
#' @param backend_path Optional path to the AIGRA_BACKEND folder.
#'
#' @return A list containing backend path, Python path, Python version, and import status.
#' @export
aigra_status <- function(
  backend_path = NULL
) {
  if (!is.null(backend_path)) {
    aigra_use_backend(backend_path = backend_path)
  } else if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  aigra_backend_status()
}


#' Parse assessment items from a PDF
#'
#' Parses a supported assessment-item PDF through the AIGRA Python backend
#' and returns a data frame of source items.
#'
#' @param pdf_path Path to the source item-bank PDF. Defaults to backend data/Kz.pdf.
#' @param source_language Language of the source item bank.
#' @param subject Subject name.
#' @param exam Examination name.
#'
#' @return A data frame of parsed assessment items.
#' @export
aigra_parse_items <- function(
  pdf_path = NULL,
  source_language = "Russian",
  subject = "Physics",
  exam = "Kazakhstan UNT"
) {
  aigra_parse_pdf(
    pdf_path = pdf_path,
    source_language = source_language,
    subject = subject,
    exam = exam
  )
}


#' Generate and review assessment items
#'
#' Runs the AIGRA Python backend pipeline to generate, solve, review,
#' and export assessment items.
#'
#' @param pdf_path Path to source item-bank PDF. Defaults to backend data/Kz.pdf.
#' @param target_language Language for generated items.
#' @param n_clones Number of clones per source item.
#' @param max_items Maximum number of source items to process.
#' @param provider LLM provider. Currently usually "groq".
#' @param model LLM model name.
#' @param source_language Language of the source item bank.
#' @param review_language Language for review comments.
#' @param subject Subject name.
#' @param exam Examination name.
#' @param read_csv If TRUE, returns the latest CSV output as a data frame.
#'
#' @return A data frame if read_csv is TRUE; otherwise invisibly returns latest CSV path.
#' @export
aigra_generate_items <- function(
  pdf_path = NULL,
  target_language = "English",
  n_clones = 1,
  max_items = 1,
  provider = "gemini",
  model = "gemini-3.1-pro-preview",
  source_language = "Russian",
  review_language = "English",
  subject = "Physics",
  exam = "Kazakhstan UNT",
  read_csv = TRUE
) {
  aigra_generate(
    pdf_path = pdf_path,
    target_language = target_language,
    n_clones = n_clones,
    max_items = max_items,
    provider = provider,
    model = model,
    source_language = source_language,
    review_language = review_language,
    subject = subject,
    exam = exam,
    read_csv = read_csv
  )
}


#' List AIGRA output files
#'
#' Lists generated CSV and JSONL files from the backend output directory.
#'
#' @return A data frame of output files.
#' @export
aigra_list_outputs <- function() {
  aigra_outputs()
}


#' Get latest AIGRA CSV output path
#'
#' @return Path to the latest AIGRA CSV output.
#' @export
aigra_latest_output <- function() {
  aigra_latest_csv()
}


#' Read latest AIGRA CSV output
#'
#' @return A data frame containing the latest AIGRA CSV output.
#' @export
aigra_read_latest_output <- function() {
  aigra_read_latest_csv()
}

