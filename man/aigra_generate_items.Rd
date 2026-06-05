\name{aigra_generate_items}
\alias{aigra_generate_items}
\title{Generate Assessment Item Clones with Simplified API-Key Handling}
\usage{
aigra_generate_items(
  file_path,
  model = "gemini-3.1-pro-preview",
  provider = "auto",
  gemini.API = NULL,
  openai.API = NULL,
  groq.API = NULL,
  anthropic.API = NULL,
  backend_path = NULL,
  ...
)
}
\arguments{
\item{file_path}{Path to the item-bank template file.}
\item{model}{Model name or alias. For example, \code{"gemini"}, \code{"sonnet"}, \code{"gpt4o"}, or \code{"llama"}.}
\item{provider}{Provider name. Use \code{"auto"} to infer the provider from the model name or supplied API key.}
\item{gemini.API}{Optional 'Gemini' API key.}
\item{openai.API}{Optional 'OpenAI' API key.}
\item{groq.API}{Optional 'Groq' API key.}
\item{anthropic.API}{Optional 'Anthropic' API key.}
\item{backend_path}{Optional path to the external 'AIGRA_BACKEND' folder.}
\item{...}{Additional arguments passed to \code{aigra_generate_tabular_items()}, such as \code{source_language}, \code{target_language}, \code{subject}, \code{exam}, \code{n_clones}, and \code{max_items}.}
}
\value{
A data frame of generated items and review information.
}
\description{
A user-friendly wrapper around \code{aigra_generate_tabular_items()} that allows users to provide provider API keys directly in R.
}
\examples{
if (interactive()) {
  out <- aigra_generate_items(
    file_path = "items.xlsx",
    model = "sonnet",
    anthropic.API = "your_key",
    backend_path = "path/to/AIGRA_BACKEND",
    source_language = "English",
    target_language = "English",
    subject = "Mathematics",
    exam = "Demo",
    n_clones = 1,
    max_items = 2
  )
}
}
\keyword{utilities}
