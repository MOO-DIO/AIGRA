#' Set API keys for AIGRA providers
#'
#' Stores API keys in the current R session so that the Python backend can use them.
#'
#' @param gemini.API Optional Gemini API key.
#' @param openai.API Optional OpenAI API key.
#' @param groq.API Optional Groq API key.
#' @param anthropic.API Optional Anthropic API key.
#'
#' @return Invisibly returns TRUE.
#' @export
aigra_set_api_keys <- function(gemini.API = NULL,
                               openai.API = NULL,
                               groq.API = NULL,
                               anthropic.API = NULL) {

  if (!is.null(gemini.API) && nzchar(gemini.API)) {
    Sys.setenv(GEMINI_API_KEY = gemini.API)
  }

  if (!is.null(openai.API) && nzchar(openai.API)) {
    Sys.setenv(OPENAI_API_KEY = openai.API)
  }

  if (!is.null(groq.API) && nzchar(groq.API)) {
    Sys.setenv(GROQ_API_KEY = groq.API)
  }

  if (!is.null(anthropic.API) && nzchar(anthropic.API)) {
    Sys.setenv(ANTHROPIC_API_KEY = anthropic.API)
  }

  invisible(TRUE)
}
