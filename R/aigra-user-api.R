#' Set 'AIGRA' API Keys
#'
#' Sets API keys for supported providers from within R. These environment
#' variables are inherited by the external 'Python' backend when generation
#' functions are called.
#'
#' @param gemini.API Optional 'Gemini' API key.
#' @param openai.API Optional 'OpenAI' API key.
#' @param groq.API Optional 'Groq' API key.
#' @param anthropic.API Optional 'Anthropic' API key.
#'
#' @return A data frame or list returned by the AIGRA tabular generation backend.
#' @examples
#' keys <- aigra_set_api_keys()
#' is.logical(keys)
#' @export
aigra_set_api_keys <- function(
    gemini.API = NULL,
    openai.API = NULL,
    groq.API = NULL,
    anthropic.API = NULL
) {
  set_if_given <- function(value, env_name) {
    if (!is.null(value) && length(value) == 1 && nzchar(value)) {
      do.call(Sys.setenv, stats::setNames(list(value), env_name))
    }
  }

  set_if_given(gemini.API, "GEMINI_API_KEY")
  set_if_given(openai.API, "OPENAI_API_KEY")
  set_if_given(groq.API, "GROQ_API_KEY")
  set_if_given(anthropic.API, "ANTHROPIC_API_KEY")

  invisible(c(
    GEMINI_API_KEY = nzchar(Sys.getenv("GEMINI_API_KEY")),
    OPENAI_API_KEY = nzchar(Sys.getenv("OPENAI_API_KEY")),
    GROQ_API_KEY = nzchar(Sys.getenv("GROQ_API_KEY")),
    ANTHROPIC_API_KEY = nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
  ))
}


.aigra_has_key <- function(x) {
  !is.null(x) && length(x) == 1 && nzchar(x)
}


.aigra_detect_provider <- function(
    provider = "auto",
    model = NULL,
    gemini.API = NULL,
    openai.API = NULL,
    groq.API = NULL,
    anthropic.API = NULL
) {
  if (!is.null(provider) && provider != "auto") {
    return(provider)
  }

  model_lower <- tolower(ifelse(is.null(model), "", model))

  if (.aigra_has_key(gemini.API) || grepl("gemini", model_lower)) {
    return("gemini")
  }

  if (.aigra_has_key(anthropic.API) ||
      grepl("claude|sonnet|opus|haiku|anthropic", model_lower)) {
    return("anthropic")
  }

  if (.aigra_has_key(openai.API) ||
      grepl("gpt|openai|o1|o3|o4", model_lower)) {
    return("openai")
  }

  if (.aigra_has_key(groq.API) ||
      grepl("llama|mixtral|gemma|deepseek|qwen|groq", model_lower)) {
    return("groq")
  }

  stop(
    "Could not determine the provider automatically. ",
    "Please set provider = 'gemini', 'anthropic', 'openai', or 'groq', ",
    "or supply one of gemini.API, anthropic.API, openai.API, or groq.API.",
    call. = FALSE
  )
}


.aigra_normalize_model <- function(model, provider) {
  if (is.null(model) || !nzchar(model)) {
    if (provider == "gemini") return("gemini-3.1-pro-preview")
    if (provider == "anthropic") return("claude-sonnet-4-6")
    if (provider == "openai") return("gpt-4o")
    if (provider == "groq") return("llama-3.3-70b-versatile")
  }

  model_lower <- tolower(model)

  if (provider == "anthropic") {
    if (model_lower == "sonnet") return("claude-sonnet-4-6")
    if (model_lower == "opus") return("claude-opus-4-8")
    if (model_lower == "haiku") return("claude-haiku-4-5-20251001")
    if (model_lower == "claude") return("claude-sonnet-4-6")
  }

  if (provider == "gemini") {
    if (model_lower == "gemini") return("gemini-3.1-pro-preview")
  }

  if (provider == "openai") {
    if (model_lower == "gpt4o") return("gpt-4o")
  }

  if (provider == "groq") {
    if (model_lower == "llama") return("llama-3.3-70b-versatile")
  }

  model
}


.aigra_check_backend_message <- function(backend_path = NULL) {
  if (!is.null(backend_path) && nzchar(backend_path)) {
    Sys.setenv(AIGRA_BACKEND_PATH = backend_path)
    try(aigra_set_backend(backend_path), silent = TRUE)
  }

  path <- Sys.getenv("AIGRA_BACKEND_PATH", unset = "")

  if (!nzchar(path)) {
    stop(
      "The 'AIGRA' Python backend is not configured.\n\n",
      "The CRAN package provides the R interface, but full LLM-based ",
      "generation requires the external 'AIGRA_BACKEND' folder.\n\n",
      "Set it with:\n",
      "  Sys.setenv(AIGRA_BACKEND_PATH = 'path/to/AIGRA_BACKEND')\n",
      "  aigra_set_backend('path/to/AIGRA_BACKEND')\n\n",
      "API keys can be supplied directly in R using arguments such as ",
      "gemini.API, anthropic.API, openai.API, or groq.API.",
      call. = FALSE
    )
  }

  if (!dir.exists(path)) {
    stop(
      "The configured 'AIGRA_BACKEND_PATH' does not exist:\n",
      path,
      "\n\nPlease check the path or pass backend_path explicitly.",
      call. = FALSE
    )
  }

  invisible(path)
}


#' Show 'AIGRA' Backend Setup Help
#'
#' Prints a short guide for configuring the external 'AIGRA' backend.
#'
#' @return A data frame or list returned by the AIGRA tabular generation backend.
#' @examples
#' aigra_backend_help()
#' @export
aigra_backend_help <- function() {
  message(
    "AIGRA backend setup\n",
    "===================\n\n",
    "The CRAN package provides the R interface. Full LLM-based generation\n",
    "requires the external Python backend folder, usually named AIGRA_BACKEND.\n\n",
    "Example setup:\n\n",
    "  library(AIGRA)\n",
    "  Sys.setenv(AIGRA_BACKEND_PATH = 'path/to/AIGRA_BACKEND')\n",
    "  aigra_set_backend('path/to/AIGRA_BACKEND')\n",
    "  aigra_status()\n\n",
    "API keys can be supplied either with Sys.setenv(), for example:\n\n",
    "  Sys.setenv(GEMINI_API_KEY = 'your_key')\n",
    "  Sys.setenv(ANTHROPIC_API_KEY = 'your_key')\n\n",
    "or directly in aigra_generate_items(), for example:\n\n",
    "  aigra_generate_items(file_path = template_file, model = 'sonnet', anthropic.API = 'your_key')\n"
  )

  invisible(NULL)
}


#' Generate AIGRA items from a data frame
#'
#' This user-facing wrapper accepts an AIGRA tabular item bank that has already
#' been loaded into R as a data frame. To use CSV or XLSX files, first call
#' `aigra_read_template()` and pass the resulting data frame to this function.
#'
#' @param data A data frame with the standard AIGRA tabular columns.
#' @param model Model name to use.
#' @param provider LLM provider. Use `"auto"`, `"gemini"`, `"openai"`,
#'   `"groq"`, or `"anthropic"`, depending on the backend configuration.
#' @param gemini.API Optional Gemini API key.
#' @param openai.API Optional OpenAI API key.
#' @param groq.API Optional Groq API key.
#' @param anthropic.API Optional Anthropic API key.
#' @param backend_path Optional path to the AIGRA Python backend.
#' @param ... Additional arguments passed to [aigra_generate_tabular_items()].
#'
#' @return A data frame or list returned by the AIGRA tabular generation backend.
#' @export
#'
#' @examples
#' \dontrun{
#' template <- aigra_read_template("template.xlsx")
#'
#' result <- aigra_generate_items(
#'   data = template,
#'   provider = "gemini",
#'   model = "gemini-2.5-flash",
#'   source_language = "English",
#'   target_language = "English",
#'   n_clones = 1,
#'   max_items = 1
#' )
#' }
aigra_generate_items <- function(data,
                                 model = "gemini-3.1-pro-preview",
                                 provider = "auto",
                                 gemini.API = NULL,
                                 openai.API = NULL,
                                 groq.API = NULL,
                                 anthropic.API = NULL,
                                 backend_path = NULL,
                                 ...) {

  if (missing(data) || is.null(data)) {
    stop(
      "Please supply `data` as a data frame. Use aigra_read_template() to read CSV or XLSX files first.",
      call. = FALSE
    )
  }

  data <- .aigra_validate_tabular_data(data)

  file_path <- .aigra_write_temp_utf8_csv(data)
  on.exit(unlink(file_path), add = TRUE)

  if (!is.null(backend_path)) {
    Sys.setenv(AIGRA_BACKEND_PATH = backend_path)

    if (exists("aigra_set_backend", mode = "function")) {
      try(aigra_set_backend(backend_path), silent = TRUE)
    }
  }

  aigra_set_api_keys(
    gemini.API = gemini.API,
    openai.API = openai.API,
    groq.API = groq.API,
    anthropic.API = anthropic.API
  )

  detected_provider <- .aigra_detect_provider(
    provider = provider,
    model = model,
    gemini.API = gemini.API,
    openai.API = openai.API,
    groq.API = groq.API,
    anthropic.API = anthropic.API
  )

  normalized_model <- .aigra_normalize_model(
    model = model,
    provider = detected_provider
  )

  .aigra_check_backend_message(backend_path = backend_path)

  message("AIGRA received data frame with ", nrow(data), " row(s).")
  message("Temporary UTF-8 generation file: ", file_path)

  extra_args <- list(...)
  extra_args$data <- NULL
  extra_args$file_path <- NULL
  extra_args$backend_path <- NULL

  call_args <- c(
    list(
      file_path = file_path,
      provider = detected_provider,
      model = normalized_model
    ),
    extra_args
  )

  do.call(aigra_generate_tabular_items, call_args)
}
