
.aigra_resolve_backend_path <- function(backend_path = NULL, must_work = TRUE) {
  if (is.null(backend_path) || !nzchar(backend_path)) {
    backend_path <- Sys.getenv('AIGRA_BACKEND_PATH', unset = '')
  }

  if (!nzchar(backend_path)) {
    stop(
      'backend_path is required unless AIGRA_BACKEND_PATH is set.',
      call. = FALSE
    )
  }

  normalizePath(backend_path, winslash = '/', mustWork = must_work)
}


#' Get AIGRA backend path
#'
#' Returns the configured AIGRA Python backend path.
#'
#' AIGRA first checks the active session configuration. If no backend has
#' been set in the current R session, it checks the AIGRA_BACKEND_PATH
#' environment variable.
#'
#' @return Backend path as a character string.
#' @export
aigra_backend_path <- function() {
  if (!is.null(.aigra_env$backend_path)) {
    return(.aigra_env$backend_path)
  }

  path <- Sys.getenv("AIGRA_BACKEND_PATH", unset = "")

  if (!nzchar(path)) {
    stop(
      "AIGRA backend path is not set. ",
      "Use aigra_set_backend('path/to/AIGRA_BACKEND') or set ",
      "Sys.setenv(AIGRA_BACKEND_PATH = 'path/to/AIGRA_BACKEND')."
    )
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}


#' Set AIGRA backend path
#'
#' Sets the path to the AIGRA Python backend.
#'
#' @param backend_path Path to the AIGRA_BACKEND folder. If NULL, uses
#'   the AIGRA_BACKEND_PATH environment variable.
#'
#' @return Invisibly returns the normalized backend path.
#' @export
aigra_set_backend <- function(backend_path = NULL) {
  if (is.null(backend_path)) {
    backend_path <- Sys.getenv("AIGRA_BACKEND_PATH", unset = "")
  }

  if (!nzchar(backend_path)) {
    stop(
      "backend_path is required unless AIGRA_BACKEND_PATH is set."
    )
  }

  backend_path <- normalizePath(
    backend_path,
    winslash = "/",
    mustWork = TRUE
  )

  if (!dir.exists(file.path(backend_path, "aigra_backend"))) {
    stop(
      "The supplied backend_path does not look like an AIGRA backend. ",
      "Expected folder not found: ",
      file.path(backend_path, "aigra_backend")
    )
  }

  .aigra_env$backend_path <- backend_path

  python_path <- file.path(
    backend_path,
    ".venv",
    "Scripts",
    "python.exe"
  )

  if (file.exists(python_path)) {
    .aigra_env$python_path <- normalizePath(
      python_path,
      winslash = "/",
      mustWork = TRUE
    )

    reticulate::use_python(
      .aigra_env$python_path,
      required = FALSE
    )
  }

  if (!backend_path %in% reticulate::py_config()$pythonpath) {
    if (!backend_path %in% .libPaths()) {
      # no action needed; backend is added through sys.path below
    }
  }

  reticulate::py_run_string(sprintf(
    'import sys; p = r\"%s\"; sys.path.insert(0, p) if p not in sys.path else None',
    backend_path
  ))

  invisible(backend_path)
}


#' Use AIGRA backend
#'
#' Backward-compatible alias for [aigra_set_backend()].
#'
#' @param backend_path Optional backend path. If NULL, uses AIGRA_BACKEND_PATH.
#'
#' @return Invisibly returns backend path.
#' @export
aigra_use_backend <- function(backend_path = NULL) {
  aigra_set_backend(backend_path)
}

