# Data-frame input helpers for AIGRA

.aigra_required_tabular_columns <- function() {
  c(
    "item_id", "stem", "option_A", "option_B", "option_C", "option_D",
    "correct_answer", "difficulty", "grade", "section", "topic", "objective",
    "subject", "exam", "source_language", "target_language",
    "source_diagram_required", "source_diagram_path", "source_diagram_type"
  )
}

.aigra_validate_tabular_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame. Use aigra_read_template() to read CSV or XLSX files first.", call. = FALSE)
  }

  required <- .aigra_required_tabular_columns()
  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "The supplied data frame is missing required AIGRA columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data <- data[, c(required, setdiff(names(data), required)), drop = FALSE]

  data[] <- lapply(data, function(x) {
    if (is.factor(x)) as.character(x) else x
  })

  data
}

.aigra_write_temp_utf8_csv <- function(data) {
  data <- .aigra_validate_tabular_data(data)

  tmp_file <- tempfile(pattern = "aigra_tabular_", fileext = ".csv")
  con <- file(tmp_file, open = "w", encoding = "UTF-8")

  on.exit(close(con), add = TRUE)

  utils::write.csv(
    data,
    con,
    row.names = FALSE,
    na = "",
    quote = TRUE
  )

  tmp_file
}

