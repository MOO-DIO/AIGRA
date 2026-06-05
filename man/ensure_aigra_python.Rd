\name{ensure_aigra_python}
\alias{ensure_aigra_python}
\title{Ensure 'AIGRA' 'Python' Environment}
\usage{
ensure_aigra_python(
  backend_path = NULL,
  python_version = "3.11",
  force = FALSE,
  install_providers = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{backend_path}{Path to the 'AIGRA' backend folder. If \code{NULL}, the \code{AIGRA_BACKEND_PATH} environment variable is used.}
\item{python_version}{'Python' version to use with 'uv'.}
\item{force}{If \code{TRUE}, recreate the virtual environment.}
\item{install_providers}{If \code{TRUE}, install provider packages.}
\item{verbose}{If \code{TRUE}, show progress messages.}
}
\value{
A list with 'Python' environment information.
}
\description{
Creates or repairs the 'Python' virtual environment used by the 'AIGRA' backend.
}
\examples{
# This function creates or repairs a backend environment,
# so the example only inspects the function interface.
names(formals(ensure_aigra_python))
}
