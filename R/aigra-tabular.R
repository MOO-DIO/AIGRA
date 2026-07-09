
#' Parse tabular item bank
#'
#' Parses a CSV or Excel item bank directly from the supplied file path.
#'
#' @param file_path Path to CSV or Excel item bank.
#'
#' @param source_language Source language of the input tabular item bank.
#' @param subject Subject area of the assessment items.
#' @param exam Examination, assessment, or project name.
#' @return A data frame of parsed assessment items.
#' @export
aigra_parse_tabular_items <- function(
  file_path,
  source_language = "English",
  subject = "General",
  exam = "Item Bank"
) {
  file_path <- normalizePath(
    file_path,
    winslash = "/",
    mustWork = TRUE
  )

  message("Reading tabular item bank from: ", file_path)

  ext <- tolower(tools::file_ext(file_path))

  if (ext == "csv") {
    data <- utils::read.csv(
      file_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    )
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package 'readxl' is required to read Excel files. ",
        "Install it with install.packages('readxl')."
      )
    }

    data <- as.data.frame(
      readxl::read_excel(file_path),
      stringsAsFactors = FALSE
    )
  } else {
    stop("Unsupported file type. Use .csv, .xlsx, or .xls.")
  }

  names(data) <- trimws(names(data))

  required <- c(
    "stem",
    "option_A",
    "option_B",
    "option_C",
    "option_D",
    "correct_answer"
  )

  missing <- setdiff(required, names(data))

  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  add_default <- function(name, value) {
    if (!name %in% names(data)) {
      data[[name]] <<- value
    }
  }

  add_default("item_id", seq_len(nrow(data)))
  add_default("source_language", source_language)
  add_default("subject", subject)
  add_default("exam", exam)
  add_default("difficulty", NA_character_)
  add_default("grade", NA_character_)
  add_default("section", NA_character_)
  add_default("topic", NA_character_)
  add_default("objective", NA_character_)

  # Multimodal/source-diagram columns
  add_default("source_diagram_required", FALSE)
  add_default("source_diagram_path", "")
  add_default("source_diagram_caption", "")
  add_default("source_diagram_type", "")

  out <- data.frame(
    item_id = as.character(data$item_id),
    source_language = as.character(data$source_language),
    subject = as.character(data$subject),
    exam = as.character(data$exam),
    difficulty = as.character(data$difficulty),
    grade = as.character(data$grade),
    section = as.character(data$section),
    topic = as.character(data$topic),
    objective = as.character(data$objective),
    stem = as.character(data$stem),
    option_A = as.character(data$option_A),
    option_B = as.character(data$option_B),
    option_C = as.character(data$option_C),
    option_D = as.character(data$option_D),
    correct_answer = toupper(substr(as.character(data$correct_answer), 1, 1)),
    source_diagram_required = data$source_diagram_required,
    source_diagram_path = as.character(data$source_diagram_path),
    source_diagram_caption = as.character(data$source_diagram_caption),
    source_diagram_type = as.character(data$source_diagram_type),
    stringsAsFactors = FALSE
  )

  out
}

