
#' Get path to bundled AIGRA example item bank
#'
#' @return Path to the bundled example CSV item bank.
#' @export
aigra_example_item_bank <- function() {
  path <- system.file(
    "extdata",
    "aigra_example_items.csv",
    package = "AIGRA"
  )

  if (!nzchar(path)) {
    stop("Bundled example item bank not found.")
  }

  path
}

