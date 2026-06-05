.aigra_env <- new.env(parent = emptyenv())

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

aigra_use_backend <- function(
  backend_path = NULL
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install reticulate: install.packages('reticulate')")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install jsonlite: install.packages('jsonlite')")
  }

  backend_path <- normalizePath(
    backend_path,
    winslash = "/",
    mustWork = TRUE
  )

  python_path <- file.path(
    backend_path,
    ".venv",
    "Scripts",
    "python.exe"
  )

  if (!file.exists(python_path)) {
    stop("Python executable not found at: ", python_path)
  }

  Sys.setenv(RETICULATE_PYTHON = python_path)
  Sys.setenv(HF_HUB_OFFLINE = "1")
  Sys.setenv(TRANSFORMERS_OFFLINE = "1")

  reticulate::use_python(python_path, required = TRUE)

  backend_json <- jsonlite::toJSON(
    backend_path,
    auto_unbox = TRUE
  )

  reticulate::py_run_string(sprintf(
    "import sys\nbackend_path = %s\nif backend_path not in sys.path:\n    sys.path.insert(0, backend_path)",
    backend_json
  ))

  .aigra_env$backend_path <- backend_path
  .aigra_env$python_path <- python_path

  invisible(list(
    backend_path = backend_path,
    python_path = python_path
  ))
}

aigra_backend_status <- function() {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  cfg <- reticulate::py_config()

  list(
    backend_path = .aigra_env$backend_path,
    python_path = cfg$python,
    python_version = cfg$version,
    backend_import_ok = reticulate::py_module_available("aigra_backend.parsers")
  )
}

aigra_parse_pdf <- function(
  pdf_path = NULL,
  source_language = "Russian",
  subject = "Physics",
  exam = "Kazakhstan UNT"
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  if (is.null(pdf_path)) {
    pdf_path <- file.path(.aigra_env$backend_path, "data", "Kz.pdf")
  }

  pdf_path <- normalizePath(
    pdf_path,
    winslash = "/",
    mustWork = TRUE
  )

  parsers <- reticulate::import("aigra_backend.parsers")

  items <- parsers$parse_kazakhstan_physics_pdf(
    pdf_path,
    source_language = source_language,
    subject = subject,
    exam = exam
  )

  items_r <- lapply(items, function(x) {
    reticulate::py_to_r(x$model_dump())
  })

  rows <- lapply(items_r, function(x) {
    opts <- x$options %||% list()

    data.frame(
      item_id = x$item_id %||% NA_character_,
      source_language = x$source_language %||% NA_character_,
      subject = x$subject %||% NA_character_,
      exam = x$exam %||% NA_character_,
      difficulty = x$difficulty %||% NA_character_,
      grade = x$grade %||% NA_character_,
      section = x$section %||% NA_character_,
      topic = x$topic %||% NA_character_,
      objective = x$objective %||% NA_character_,
      stem = x$stem %||% NA_character_,
      option_A = opts$A %||% NA_character_,
      option_B = opts$B %||% NA_character_,
      option_C = opts$C %||% NA_character_,
      option_D = opts$D %||% NA_character_,
      correct_answer = x$correct_answer %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

aigra_output_dir <- function() {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  file.path(.aigra_env$backend_path, "outputs")
}

aigra_outputs <- function(pattern = "aigra_results_.*\\.(csv|jsonl)$") {
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

  data.frame(
    file = normalizePath(files, winslash = "/", mustWork = FALSE),
    name = basename(files),
    size = info$size,
    modified = info$mtime,
    stringsAsFactors = FALSE
  )[order(info$mtime, decreasing = TRUE), ]
}

aigra_latest_csv <- function() {
  outs <- aigra_outputs(pattern = "aigra_results_.*\\.csv$")

  if (!nrow(outs)) {
    stop("No AIGRA CSV output found.")
  }

  outs$file[1]
}

aigra_read_latest_csv <- function() {
  csv <- aigra_latest_csv()
  utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)
}

aigra_run_pipeline <- function(
  pdf_path = NULL,
  provider = "gemini",
  model = "gemini-3.1-pro-preview",
  source_language = "Russian",
  target_language = "English",
  review_language = "English",
  subject = "Physics",
  exam = "Kazakhstan UNT",
  n_clones = 1,
  max_items = 1,
  output_dir = NULL
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  if (is.null(pdf_path)) {
    pdf_path <- file.path(.aigra_env$backend_path, "data", "Kz.pdf")
  }

  if (is.null(output_dir)) {
    output_dir <- file.path(.aigra_env$backend_path, "outputs")
  }

  pdf_path <- normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  orchestrator <- reticulate::import("aigra_backend.orchestrator")

  records <- orchestrator$run_aigra_pipeline(
    pdf_path = pdf_path,
    provider = provider,
    model = model,
    source_language = source_language,
    target_language = target_language,
    review_language = review_language,
    subject = subject,
    exam = exam,
    n_clones = as.integer(n_clones),
    max_items = as.integer(max_items),
    output_dir = output_dir
  )

  invisible(records)
}

aigra_generate <- function(
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
  aigra_run_pipeline(
    pdf_path = pdf_path,
    provider = provider,
    model = model,
    source_language = source_language,
    target_language = target_language,
    review_language = review_language,
    subject = subject,
    exam = exam,
    n_clones = n_clones,
    max_items = max_items
  )

  latest <- aigra_latest_csv()

  message("Latest AIGRA CSV: ", latest)

  if (read_csv) {
    return(utils::read.csv(latest, stringsAsFactors = FALSE, check.names = FALSE))
  }

  invisible(latest)
}
