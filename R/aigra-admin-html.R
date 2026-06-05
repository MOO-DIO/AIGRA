
aigra_admin_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}


aigra_admin_image_data_uri <- function(path) {
  path <- as.character(path)

  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return("")
  }

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return("")
  }

  ext <- tolower(tools::file_ext(path))

  mime <- switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    webp = "image/webp",
    "image/png"
  )

  encoded <- base64enc::base64encode(path)

  paste0("data:", mime, ";base64,", encoded)
}



aigra_admin_pick_key <- function(data, row) {
  candidates <- c(
    "clone_correct_answer",
    "generated_correct_answer",
    "correct_answer",
    "answer_key",
    "generated_key",
    "clone_key",
    "key",
    "correct_option",
    "correct",
    "answer"
  )

  for (nm in candidates) {
    if (nm %in% names(data)) {
      value <- data[[nm]][row]
      if (!is.na(value) && nzchar(trimws(as.character(value)))) {
        return(toupper(substr(as.character(value), 1, 1)))
      }
    }
  }

  # Fallback: if the solver matched the key but the key column was not exported,
  # use solver_answer as the administration key.
  if (
    "solver_matches_key" %in% names(data) &&
      "solver_answer" %in% names(data)
  ) {
    matched <- tolower(as.character(data$solver_matches_key[row])) %in%
      c("true", "t", "1", "yes", "y")

    solver_answer <- data$solver_answer[row]

    if (
      matched &&
        !is.na(solver_answer) &&
        nzchar(trimws(as.character(solver_answer)))
    ) {
      return(toupper(substr(as.character(solver_answer), 1, 1)))
    }
  }

  ""
}


aigra_pick_col <- function(data, row, candidates, default = "") {
  for (nm in candidates) {
    if (nm %in% names(data)) {
      value <- data[[nm]][row]

      if (!is.na(value) && nzchar(trimws(as.character(value)))) {
        return(as.character(value))
      }
    }
  }

  default
}


#' Write an AIGRA administration HTML file
#'
#' Creates a print-ready HTML file containing generated item stems,
#' diagrams, response options, and an answer key.
#'
#' @param result A data frame returned by AIGRA, preferably after diagram generation.
#' @param file Output HTML file. If NULL, writes to backend outputs folder.
#' @param title Title shown at the top of the paper.
#' @param include_key If TRUE, include answer key at the end.
#' @param include_metadata If TRUE, show topic, section, and difficulty.
#' @param only_accepted If TRUE, include only rows with status ok or edited.
#'
#' @return Path to the HTML file.
#' @export
aigra_write_admin_html <- function(
  result,
  file = NULL,
  title = "AIGRA Generated Assessment Items",
  include_key = TRUE,
  include_metadata = FALSE,
  only_accepted = TRUE
) {
  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
  }

  if (only_accepted && "status" %in% names(result)) {
    keep <- tolower(as.character(result$status)) %in% c("ok", "edited")
    result <- result[keep, , drop = FALSE]
  }

  if (!nrow(result)) {
    stop("No rows available for administration report.")
  }

  if (is.null(file)) {
    out_dir <- if (!is.null(.aigra_env$backend_path)) {
      file.path(.aigra_env$backend_path, "outputs")
    } else {
      tempdir()
    }

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    file <- file.path(
      out_dir,
      paste0("aigra_admin_items_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    )
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    paste0("<title>", aigra_admin_html_escape(title), "</title>"),
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 2rem; line-height: 1.5; color: #111; }",
    ".paper-title { text-align: center; margin-bottom: 2rem; }",
    ".instructions { border: 1px solid #ddd; padding: 1rem; margin-bottom: 2rem; background: #fafafa; }",
    ".item { margin-bottom: 2.2rem; page-break-inside: avoid; }",
    ".item-number { font-weight: bold; font-size: 1.05rem; margin-bottom: 0.5rem; }",
    ".stem { margin-bottom: 0.9rem; }",
    ".diagram { text-align: center; margin: 1rem 0; }",
    ".diagram img { max-width: 620px; width: 100%; border: 1px solid #ccc; padding: 0.4rem; background: #fff; }",
    ".options { margin-top: 0.8rem; }",
    ".option { margin: 0.35rem 0; }",
    ".metadata { font-size: 0.85rem; color: #555; margin-bottom: 0.6rem; }",
    ".answer-key { margin-top: 3rem; border-top: 2px solid #222; padding-top: 1rem; }",
    ".answer-key table { border-collapse: collapse; width: 100%; max-width: 500px; }",
    ".answer-key th, .answer-key td { border: 1px solid #ccc; padding: 0.45rem; text-align: left; }",
    "@media print {",
    "  body { margin: 1.2cm; }",
    "  .item { page-break-inside: avoid; }",
    "  .answer-key { page-break-before: always; }",
    "}",
    "</style>",
    "</head>",
    "<body>",
    paste0("<h1 class='paper-title'>", aigra_admin_html_escape(title), "</h1>"),
    "<div class='instructions'>",
    "<strong>Instructions:</strong> Choose the correct option for each item.",
    "</div>"
  )

  key_rows <- character()

  for (i in seq_len(nrow(result))) {
    stem <- aigra_pick_col(
      result,
      i,
      c("clone_stem", "stem", "generated_stem")
    )

    opt_A <- aigra_pick_col(result, i, c("opt_A", "option_A", "A"))
    opt_B <- aigra_pick_col(result, i, c("opt_B", "option_B", "B"))
    opt_C <- aigra_pick_col(result, i, c("opt_C", "option_C", "C"))
    opt_D <- aigra_pick_col(result, i, c("opt_D", "option_D", "D"))

    key <- aigra_admin_pick_key(result, i)

    original_id <- aigra_pick_col(result, i, c("original_id", "item_id"), default = as.character(i))

    topic <- aigra_pick_col(result, i, c("original_topic", "topic"))
    section <- aigra_pick_col(result, i, c("original_section", "section"))
    difficulty <- aigra_pick_col(result, i, c("original_difficulty", "difficulty"))

    diagram_path <- aigra_pick_col(result, i, c("diagram_path", "clone_diagram_path"))
    img_src <- aigra_admin_image_data_uri(diagram_path)

    html <- c(
      html,
      "<div class='item'>",
      paste0("<div class='item-number'>", i, ". Item ID: ", aigra_admin_html_escape(original_id), "</div>")
    )

    if (include_metadata) {
      html <- c(
        html,
        paste0(
          "<div class='metadata'>",
          "Section: ", aigra_admin_html_escape(section),
          " | Topic: ", aigra_admin_html_escape(topic),
          " | Difficulty: ", aigra_admin_html_escape(difficulty),
          "</div>"
        )
      )
    }

    html <- c(
      html,
      paste0("<div class='stem'>", aigra_admin_html_escape(stem), "</div>")
    )

    if (nzchar(img_src)) {
      html <- c(
        html,
        "<div class='diagram'>",
        paste0("<img src='", img_src, "' alt='Diagram for item ", i, "'>"),
        "</div>"
      )
    }

    html <- c(
      html,
      "<div class='options'>",
      paste0("<div class='option'><strong>A.</strong> ", aigra_admin_html_escape(opt_A), "</div>"),
      paste0("<div class='option'><strong>B.</strong> ", aigra_admin_html_escape(opt_B), "</div>"),
      paste0("<div class='option'><strong>C.</strong> ", aigra_admin_html_escape(opt_C), "</div>"),
      paste0("<div class='option'><strong>D.</strong> ", aigra_admin_html_escape(opt_D), "</div>"),
      "</div>",
      "</div>"
    )

    key_rows <- c(
      key_rows,
      paste0(
        "<tr><td>", i, "</td><td>",
        aigra_admin_html_escape(original_id),
        "</td><td><strong>",
        aigra_admin_html_escape(key),
        "</strong></td></tr>"
      )
    )
  }

  if (include_key) {
    html <- c(
      html,
      "<div class='answer-key'>",
      "<h2>Answer Key</h2>",
      "<table>",
      "<tr><th>No.</th><th>Item ID</th><th>Key</th></tr>",
      key_rows,
      "</table>",
      "</div>"
    )
  }

  html <- c(
    html,
    "</body>",
    "</html>"
  )

  writeLines(html, file, useBytes = TRUE)

  normalizePath(file, winslash = "/", mustWork = TRUE)
}
