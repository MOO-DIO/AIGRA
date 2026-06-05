
#' Plot AIGRA output quality summary
#'
#' Creates a simple bar chart of item review statuses in an AIGRA output.
#'
#' @param data Optional AIGRA output data frame. If NULL, reads the latest CSV output.
#'
#' @return Invisibly returns the status table used for plotting.
#' @importFrom graphics barplot
#' @export
aigra_plot_summary <- function(data = NULL) {
  if (is.null(data)) {
    data <- aigra_read_latest_output()
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame or NULL.")
  }

  if (is.null(data$status)) {
    stop("The data frame does not contain a status column.")
  }

  status <- as.character(data$status)
  status[is.na(status) | !nzchar(trimws(status))] <- "missing"

  status_levels <- c(
    "ok",
    "edited",
    "reject",
    "pipeline_failed",
    "missing"
  )

  counts <- table(factor(status, levels = status_levels))

  graphics::barplot(
    counts,
    main = "AIGRA Output Quality Summary",
    xlab = "Review status",
    ylab = "Count",
    las = 2
  )

  invisible(counts)
}

