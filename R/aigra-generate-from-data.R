#' Generate AIGRA items from an R data frame
#'
#' Alias for [aigra_generate_items()]. This function is kept for readability.
#'
#' @param data A data frame with the standard AIGRA tabular columns.
#' @param ... Additional arguments passed to [aigra_generate_items()].
#'
#' @return The result returned by [aigra_generate_items()].
#' @export
#'
#' @examples
#' \dontrun{
#' template <- aigra_read_template("template.csv")
#' result <- aigra_generate_from_data(data = template, provider = "gemini")
#' }
aigra_generate_from_data <- function(data, ...) {
  aigra_generate_items(data = data, ...)
}
