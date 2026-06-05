aigra_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}



aigra_image_data_uri <- function(path) {
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

aigra_file_uri <- function(path) {
  path <- as.character(path)
  if (is.na(path) || !nzchar(path)) {
    return("")
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (.Platform$OS.type == "windows") {
    paste0("file:///", utils::URLencode(path, reserved = TRUE))
  } else {
    paste0("file://", utils::URLencode(path, reserved = TRUE))
  }
}


#' Write an AIGRA HTML report
#'
#' Writes a CRAN-safe local HTML report from an AIGRA result data frame.
#' If diagram_path is present, generated diagrams are shown inline.
#'
#' @param result A data frame returned by AIGRA. If NULL, reads latest output.
#' @param file Output HTML path. If NULL, writes to backend outputs folder.
#' @param title Report title.
#'
#' @return Path to the report.
#' @export
aigra_write_report <- function(
  result = NULL,
  file = NULL,
  title = "AIGRA Quality Report"
) {
  if (is.null(result)) {
    result <- aigra_read_latest_output()
  }

  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
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
      paste0("aigra_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    )
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  total_rows <- nrow(result)

  status_counts <- if ("status" %in% names(result)) {
    table(result$status, useNA = "ifany")
  } else {
    integer()
  }

  generated_diagrams <- if ("diagram_path" %in% names(result)) {
    sum(!is.na(result$diagram_path) & nzchar(trimws(as.character(result$diagram_path))))
  } else {
    0
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    paste0("<title>", aigra_html_escape(title), "</title>"),
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 2rem; line-height: 1.45; }",
    "h1, h2, h3 { color: #222; }",
    ".summary { background: #f5f5f5; padding: 1rem; border-radius: 8px; margin-bottom: 1.5rem; }",
    ".item { border: 1px solid #ddd; padding: 1rem; border-radius: 8px; margin-bottom: 1.5rem; }",
    ".meta { color: #555; font-size: 0.95rem; }",
    ".status-ok { color: #176b2c; font-weight: bold; }",
    ".status-reject, .status-pipeline_failed { color: #a11616; font-weight: bold; }",
    ".diagram { margin-top: 1rem; }",
    ".diagram img { max-width: 720px; width: 100%; border: 1px solid #ccc; border-radius: 6px; padding: 0.5rem; background: white; }",
    "pre { white-space: pre-wrap; background: #fafafa; padding: 0.75rem; border-radius: 6px; border: 1px solid #eee; }",
    "</style>",
    "</head>",
    "<body>",
    paste0("<h1>", aigra_html_escape(title), "</h1>"),
    "<div class='summary'>",
    paste0("<p><strong>Total rows:</strong> ", total_rows, "</p>"),
    paste0("<p><strong>Generated diagrams:</strong> ", generated_diagrams, "</p>")
  )

  if (length(status_counts)) {
    html <- c(
      html,
      "<p><strong>Status counts:</strong></p>",
      "<ul>",
      paste0(
        "<li>",
        aigra_html_escape(names(status_counts)),
        ": ",
        as.integer(status_counts),
        "</li>"
      ),
      "</ul>"
    )
  }

  html <- c(html, "</div>", "<h2>Items</h2>")

  for (i in seq_len(nrow(result))) {
    row <- result[i, , drop = FALSE]

    original_id <- if ("original_id" %in% names(row)) row$original_id else i
    status <- if ("status" %in% names(row)) row$status else ""
    status_class <- paste0("status-", gsub("[^A-Za-z0-9_-]", "_", status))

    clone_stem <- if ("clone_stem" %in% names(row)) row$clone_stem else ""
    original_topic <- if ("original_topic" %in% names(row)) row$original_topic else ""
    diagram_required <- if ("diagram_required" %in% names(row)) row$diagram_required else ""
    diagram_prompt <- if ("diagram_prompt" %in% names(row)) row$diagram_prompt else ""
    diagram_path <- if ("diagram_path" %in% names(row)) row$diagram_path else ""
    review_comment <- if ("review_comment" %in% names(row)) row$review_comment else ""
    record_note <- if ("record_note" %in% names(row)) row$record_note else ""

    html <- c(
      html,
      "<div class='item'>",
      paste0("<h3>Item ", i, ": ", aigra_html_escape(original_id), "</h3>"),
      paste0("<p class='meta'><strong>Status:</strong> <span class='", status_class, "'>", aigra_html_escape(status), "</span></p>"),
      paste0("<p class='meta'><strong>Topic:</strong> ", aigra_html_escape(original_topic), "</p>"),
      paste0("<p class='meta'><strong>Diagram required:</strong> ", aigra_html_escape(diagram_required), "</p>"),
      "<h4>Generated clone stem</h4>",
      paste0("<p>", aigra_html_escape(clone_stem), "</p>")
    )

    if (!is.na(diagram_path) && nzchar(trimws(as.character(diagram_path)))) {
      img_src <- aigra_image_data_uri(diagram_path)

      if (!nzchar(img_src)) {
        img_src <- aigra_file_uri(diagram_path)
      }

      html <- c(
        html,
        "<div class='diagram'>",
        "<h4>Generated diagram</h4>",
        paste0("<p class='meta'>", aigra_html_escape(diagram_path), "</p>"),
        paste0("<img src='", img_src, "' alt='AIGRA generated diagram'>"),
        "</div>"
      )
    }

    if (!is.na(diagram_prompt) && nzchar(trimws(as.character(diagram_prompt)))) {
      html <- c(
        html,
        "<h4>Diagram prompt</h4>",
        paste0("<pre>", aigra_html_escape(diagram_prompt), "</pre>")
      )
    }

    if (!is.na(review_comment) && nzchar(trimws(as.character(review_comment)))) {
      html <- c(
        html,
        "<h4>Review comment</h4>",
        paste0("<p>", aigra_html_escape(review_comment), "</p>")
      )
    }

    if (!is.na(record_note) && nzchar(trimws(as.character(record_note)))) {
      html <- c(
        html,
        "<h4>Record note</h4>",
        paste0("<p>", aigra_html_escape(record_note), "</p>")
      )
    }

    html <- c(html, "</div>")
  }

  html <- c(
    html,
    "</body>",
    "</html>"
  )

  writeLines(html, file, useBytes = TRUE)

  normalizePath(file, winslash = "/", mustWork = TRUE)
}
