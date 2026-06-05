
#' Validate a tabular AIGRA item bank
#'
#' Checks a CSV or Excel item bank before parsing or generation.
#'
#' @param file_path Path to CSV or Excel item bank.
#'
#' @return A list containing validation status, issues, and a summary.
#' @export
aigra_validate_tabular_items <- function(file_path) {
  file_path <- normalizePath(file_path, winslash = "/", mustWork = TRUE)

  ext <- tolower(tools::file_ext(file_path))

  if (ext == "csv") {
    data <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package 'readxl' is required to validate Excel files. ",
        "Install it with install.packages('readxl')."
      )
    }

    data <- readxl::read_excel(file_path)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file type. Use .csv, .xlsx, or .xls.")
  }

  required <- c(
    "stem",
    "option_A",
    "option_B",
    "option_C",
    "option_D",
    "correct_answer"
  )

  optional <- c(
    "item_id",
    "difficulty",
    "grade",
    "section",
    "topic",
    "objective",
    "source_language",
    "subject",
    "exam"
  )

  issues <- data.frame(
    row = integer(),
    column = character(),
    issue = character(),
    stringsAsFactors = FALSE
  )

  add_issue <- function(row, column, issue) {
    issues <<- rbind(
      issues,
      data.frame(
        row = row,
        column = column,
        issue = issue,
        stringsAsFactors = FALSE
      )
    )
  }

  missing_required <- setdiff(required, names(data))

  for (col in missing_required) {
    add_issue(NA_integer_, col, "Missing required column")
  }

  if (length(missing_required) == 0) {
    for (i in seq_len(nrow(data))) {
      stem <- trimws(as.character(data$stem[i]))

      if (is.na(stem) || !nzchar(stem)) {
        add_issue(i, "stem", "Stem is empty")
      }

      for (opt_col in c("option_A", "option_B", "option_C", "option_D")) {
        value <- trimws(as.character(data[[opt_col]][i]))

        if (is.na(value) || !nzchar(value)) {
          add_issue(i, opt_col, "Option is empty")
        }
      }

      key <- toupper(trimws(as.character(data$correct_answer[i])))

      if (is.na(key) || !(key %in% c("A", "B", "C", "D"))) {
        add_issue(i, "correct_answer", "Correct answer must be A, B, C, or D")
      }
    }
  }

  if ("item_id" %in% names(data)) {
    ids <- trimws(as.character(data$item_id))
    dup_ids <- ids[duplicated(ids) & nzchar(ids)]

    if (length(dup_ids)) {
      duplicated_rows <- which(ids %in% dup_ids)

      for (i in duplicated_rows) {
        add_issue(i, "item_id", "Duplicate item_id")
      }
    }
  }

  unknown_columns <- setdiff(names(data), c(required, optional))

  summary <- list(
    file = file_path,
    rows = nrow(data),
    columns = names(data),
    required_columns = required,
    missing_required_columns = missing_required,
    unknown_columns = unknown_columns,
    issue_count = nrow(issues),
    valid = nrow(issues) == 0
  )

  list(
    valid = nrow(issues) == 0,
    summary = summary,
    issues = issues
  )
}


#' Print tabular item-bank validation results
#'
#' @param validation Validation result returned by [aigra_validate_tabular_items()].
#'
#' @return Invisibly returns the validation object.
#' @export
aigra_print_validation <- function(validation) {
  if (!is.list(validation) || is.null(validation$summary)) {
    stop("validation must be returned by aigra_validate_tabular_items().")
  }

  message("AIGRA tabular validation")
  message("========================")
  message("File:        ", validation$summary$file)
  message("Rows:        ", validation$summary$rows)
  message("Issue count: ", validation$summary$issue_count)
  message("Valid:       ", validation$valid)

  if (length(validation$summary$missing_required_columns)) {
    message("Missing required columns: ",
        paste(validation$summary$missing_required_columns, collapse = ", "),
        "\n")
  }

  if (length(validation$summary$unknown_columns)) {
    message("Unknown columns: ",
        paste(validation$summary$unknown_columns, collapse = ", "),
        "\n")
  }

  if (nrow(validation$issues)) {
    message("nIssues:")
    message(paste(capture.output(validation$issues), collapse = "n"))
  }

  invisible(validation)
}
