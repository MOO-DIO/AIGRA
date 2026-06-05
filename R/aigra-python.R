#' Find uv executable
#'
#' @return Path to uv executable.
#' @export
aigra_find_uv <- function() {
  uv <- Sys.which("uv")

  if (nzchar(uv)) {
    return(unname(uv))
  }

  possible <- file.path(Sys.getenv("USERPROFILE"), ".local", "bin", "uv.exe")

  if (file.exists(possible)) {
    return(normalizePath(possible, winslash = "/", mustWork = TRUE))
  }

  stop(
    "uv was not found. Install uv first, then restart R/RStudio. ",
    "On Windows, uv is often installed at ~/.local/bin/uv.exe."
  )
}


#' Show AIGRA Python environment information
#'
#' @param backend_path Path to the AIGRA_BACKEND folder.
#'
#' @return A list with Python environment information.
#' @export
aigra_python_info <- function(
  backend_path = NULL
) {
  backend_path <- if (is.null(backend_path) || !nzchar(backend_path)) Sys.getenv("AIGRA_BACKEND_PATH", unset = "") else backend_path
  if (!nzchar(backend_path)) stop("No backend path supplied. Set AIGRA_BACKEND_PATH or pass backend_path explicitly.")
  backend_path <- .aigra_resolve_backend_path(backend_path, must_work = TRUE)

  venv_path <- file.path(backend_path, ".venv")
  python_path <- file.path(venv_path, "Scripts", "python.exe")

  uv_path <- tryCatch(
    aigra_find_uv(),
    error = function(e) NA_character_
  )

  python_exists <- file.exists(python_path)

  import_check <- NA_character_
  pip_check <- NA_character_

  if (python_exists) {
    py_code <- "import importlib.util; mods=['pydantic','pypdf','pandas','faiss','sentence_transformers','groq','openai','anthropic']; missing=[m for m in mods if importlib.util.find_spec(m) is None]; import sys; sys.stdout.write('OK' if not missing else 'MISSING:' + ','.join(missing))"

    import_check <- tryCatch(
      system2(
        python_path,
        args = c("-c", shQuote(py_code, type = "cmd")),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) paste("ERROR:", conditionMessage(e))
    )

    pip_check <- tryCatch(
      system2(
        python_path,
        args = c("-m", "pip", "--version"),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) paste("ERROR:", conditionMessage(e))
    )
  }

  list(
    backend_path = backend_path,
    venv_path = normalizePath(venv_path, winslash = "/", mustWork = FALSE),
    python_path = normalizePath(python_path, winslash = "/", mustWork = FALSE),
    python_exists = python_exists,
    uv_path = uv_path,
    pip_check = pip_check,
    package_check = import_check
  )
}


#' Ensure AIGRA Python environment
#'
#' Creates or repairs the Python virtual environment used by the AIGRA backend.
#'
#' @param backend_path Path to the AIGRA_BACKEND folder.
#' @param python_version Python version to use with uv.
#' @param force If TRUE, recreates the virtual environment.
#' @param install_providers If TRUE, installs provider SDKs for Groq, OpenAI, Gemini, and Anthropic.
#'
#' @return A list with Python environment information.
#' @export
ensure_aigra_python <- function(
  backend_path = NULL,
  python_version = "3.11",
  force = FALSE,
  install_providers = TRUE,
  verbose = FALSE
) {
  backend_path <- if (is.null(backend_path) || !nzchar(backend_path)) Sys.getenv("AIGRA_BACKEND_PATH", unset = "") else backend_path
  if (!nzchar(backend_path)) stop("No backend path supplied. Set AIGRA_BACKEND_PATH or pass backend_path explicitly.")
  backend_path <- .aigra_resolve_backend_path(backend_path, must_work = TRUE)

  uv <- aigra_find_uv()

  venv_path <- file.path(backend_path, ".venv")
  python_path <- file.path(venv_path, "Scripts", "python.exe")

  if (isTRUE(verbose)) message("AIGRA backend: ", backend_path)
  if (isTRUE(verbose)) message("uv: ", uv)
  if (isTRUE(verbose)) message("Python: ", python_path)

  if (force || !file.exists(python_path)) {
    if (isTRUE(verbose)) message("Creating AIGRA Python environment...")

    args <- c(
      "venv",
      if (force) "--clear",
      venv_path,
      "--python",
      python_version
    )

    out <- system2(
      command = uv,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )

    status <- attr(out, "status")
    if (is.null(status)) status <- 0

    if (isTRUE(verbose)) message(paste(out, collapse = "\\n"))

    if (!identical(status, 0)) {
      stop("Failed to create Python environment with uv.")
    }
  } else {
    if (isTRUE(verbose)) message("Python environment already exists.")
  }

  if (!file.exists(python_path)) {
    stop("Python executable was not found after environment setup: ", python_path)
  }

  pip_check <- system2(
    python_path,
    args = c("-m", "pip", "--version"),
    stdout = TRUE,
    stderr = TRUE
  )

  pip_status <- attr(pip_check, "status")
  if (!is.null(pip_status) && !identical(pip_status, 0)) {
    if (isTRUE(verbose)) message("pip not found. Installing pip with ensurepip...")

    out <- system2(
      python_path,
      args = c("-m", "ensurepip", "--upgrade"),
      stdout = TRUE,
      stderr = TRUE
    )

    status <- attr(out, "status")
    if (is.null(status)) status <- 0

    if (isTRUE(verbose)) message(paste(out, collapse = "\\n"))

    if (!identical(status, 0)) {
      stop("Failed to install pip with ensurepip.")
    }
  }

  packages <- c(
    "pydantic",
    "python-dotenv",
    "pypdf",
    "pandas",
    "faiss-cpu",
    "sentence-transformers"
  )

  if (install_providers) {
    packages <- c(
      packages,
      "groq",
      "openai",
      "google-genai",
      "anthropic"
    )
  }

  if (isTRUE(verbose)) message("Installing/checking AIGRA Python packages via python -m pip...")

  out <- system2(
    python_path,
    args = c("-m", "pip", "install", packages),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(out, "status")
  if (is.null(status)) status <- 0

  if (isTRUE(verbose)) message(paste(out, collapse = "\\n"))

  if (!identical(status, 0)) {
    if (isTRUE(verbose)) message("pip output:")
    if (isTRUE(verbose)) message(paste(out, collapse = "\\n"))
    stop("Failed to install AIGRA Python packages with python -m pip.")
  }

  info <- aigra_python_info(backend_path = backend_path)

  if (isTRUE(verbose)) message("AIGRA Python environment ready.")

  info
}
