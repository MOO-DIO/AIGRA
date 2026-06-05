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
#' @return Invisibly returns a named logical vector showing which keys are set.
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
#' @return Invisibly returns NULL.
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


#' Generate Assessment Item Clones with Simplified API-Key Handling
#'
#' A user-friendly wrapper around [aigra_generate_tabular_items()] that allows
#' users to provide provider API keys directly in R.
#'
#' @param file_path Path to the item-bank template file.
#' @param model Model name or alias.
#' @param provider Provider name. Use `"auto"` to infer from model or API key.
#' @param gemini.API Optional 'Gemini' API key.
#' @param openai.API Optional 'OpenAI' API key.
#' @param groq.API Optional 'Groq' API key.
#' @param anthropic.API Optional 'Anthropic' API key.
#' @param backend_path Optional path to the external 'AIGRA_BACKEND' folder.
#' @param ... Additional arguments passed to [aigra_generate_tabular_items()],
#'   such as `source_language`, `target_language`, `subject`, `exam`,
#'   `n_clones`, and `max_items`.
#'
#' @return A data frame of generated items and review information.
#' @examples
#' if (interactive()) {
#'   out <- aigra_generate_items(
#'     file_path = "items.xlsx",
#'     model = "sonnet",
#'     anthropic.API = "your_key",
#'     backend_path = "path/to/AIGRA_BACKEND",
#'     source_language = "English",
#'     target_language = "English",
#'     subject = "Mathematics",
#'     exam = "Demo",
#'     n_clones = 1,
#'     max_items = 2
#'   )
#' }
#' @export
aigra_generate_items <- function(
    file_path,
    model = "gemini-3.1-pro-preview",
    provider = "auto",
    gemini.API = NULL,
    openai.API = NULL,
    groq.API = NULL,
    anthropic.API = NULL,
    backend_path = NULL,
    ...
) {
  aigra_set_api_keys(
    gemini.API = gemini.API,
    openai.API = openai.API,
    groq.API = groq.API,
    anthropic.API = anthropic.API
  )

  provider <- .aigra_detect_provider(
    provider = provider,
    model = model,
    gemini.API = gemini.API,
    openai.API = openai.API,
    groq.API = groq.API,
    anthropic.API = anthropic.API
  )

  model <- .aigra_normalize_model(model = model, provider = provider)

  .aigra_check_backend_message(backend_path = backend_path)

  aigra_generate_tabular_items(
    file_path = file_path,
    provider = provider,
    model = model,
    ...
  )
}
