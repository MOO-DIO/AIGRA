
#' Create an AIGRA tabular item-bank template
#'
#' Creates a sample item-bank data frame with the required columns for
#' CSV/Excel-based AIGRA generation.
#'
#' @return A data frame containing sample item-bank rows.
#' @export
aigra_template_items <- function() {
  data.frame(
    item_id = c("ITEM001", "ITEM002"),
    stem = c(
      "A 10 N force acts on a 2 kg object. What is the acceleration?",
      "Which physical quantity is measured in joules?"
    ),
    option_A = c("2 m/s^2", "Power"),
    option_B = c("5 m/s^2", "Energy"),
    option_C = c("10 m/s^2", "Force"),
    option_D = c("20 m/s^2", "Pressure"),
    correct_answer = c("B", "B"),
    difficulty = c("B", "A"),
    grade = c("9", "8"),
    section = c("Mechanics", "Mechanics"),
    topic = c("Newton's second law", "Energy"),
    objective = c("Apply F = ma", "Identify SI units"),
    source_language = c("English", "English"),
    subject = c("Physics", "Physics"),
    exam = c("Demo Item Bank", "Demo Item Bank"),
    source_diagram_required = c(FALSE, FALSE),
    source_diagram_path = c("", ""),
    source_diagram_caption = c("", ""),
    source_diagram_type = c("", ""),
    stringsAsFactors = FALSE
  )
}


#' Write an AIGRA CSV item-bank template
#'
#' @param file Output CSV file path.
#' @param overwrite If TRUE, overwrite an existing file.
#'
#' @return The normalized output file path.
#' @export
aigra_write_template_csv <- function(
  file = "aigra_item_template.csv",
  overwrite = FALSE
) {
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists. Use overwrite = TRUE to replace it: ", file)
  }

  template <- aigra_template_items()

  utils::write.csv(
    template,
    file = file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  normalizePath(file, winslash = "/", mustWork = TRUE)
}


#' Write an AIGRA Excel item-bank template
#'
#' @param file Output Excel file path.
#' @param overwrite If TRUE, overwrite an existing file.
#'
#' @return The normalized output file path.
#' @export
aigra_write_template_excel <- function(
  file = "aigra_item_template.xlsx",
  overwrite = FALSE
) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop(
      "Package 'writexl' is required to write Excel templates. ",
      "Install it with install.packages('writexl')."
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists. Use overwrite = TRUE to replace it: ", file)
  }

  template <- aigra_template_items()

  writexl::write_xlsx(
    x = template,
    path = file
  )

  normalizePath(file, winslash = "/", mustWork = TRUE)
}
