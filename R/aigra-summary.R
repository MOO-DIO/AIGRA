
#' Summarise an AIGRA output
#'
#' Summarises the latest or supplied AIGRA CSV output.
#'
#' @param data Optional AIGRA output data frame. If NULL, reads the latest CSV output.
#'
#' @return A list with quality counts and simple rates.
#' @export
aigra_summarise_output <- function(data = NULL) {
  if (is.null(data)) {
    data <- aigra_read_latest_output()
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame or NULL.")
  }

  n_rows <- nrow(data)

  status <- data$status
  if (is.null(status)) {
    status <- rep(NA_character_, n_rows)
  }

  solver_matches <- data$solver_matches_key
  if (is.null(solver_matches)) {
    solver_matches <- rep(NA, n_rows)
  }

  clone_stem <- data$clone_stem
  if (is.null(clone_stem)) {
    clone_stem <- rep(NA_character_, n_rows)
  }

  generated <- !is.na(clone_stem) & nzchar(trimws(as.character(clone_stem)))

  total_generated <- sum(generated, na.rm = TRUE)
  total_ok <- sum(status == "ok", na.rm = TRUE)
  total_edited <- sum(status == "edited", na.rm = TRUE)
  total_rejected <- sum(status == "reject", na.rm = TRUE)
  total_pipeline_failed <- sum(status == "pipeline_failed", na.rm = TRUE)

  solver_mismatch <- sum(
    generated & !is.na(solver_matches) & solver_matches %in% c(FALSE, "False", "FALSE", "false", 0, "0"),
    na.rm = TRUE
  )

  acceptance_rate <- if (total_generated > 0) {
    total_ok / total_generated
  } else {
    NA_real_
  }

  rejection_rate <- if (total_generated > 0) {
    total_rejected / total_generated
  } else {
    NA_real_
  }

  list(
    total_rows = n_rows,
    total_generated = total_generated,
    total_ok = total_ok,
    total_edited = total_edited,
    total_rejected = total_rejected,
    total_pipeline_failed = total_pipeline_failed,
    solver_key_mismatches = solver_mismatch,
    acceptance_rate = acceptance_rate,
    rejection_rate = rejection_rate
  )
}


#' Print an AIGRA quality summary
#'
#' Prints a compact quality summary for an AIGRA output.
#'
#' @param data Optional AIGRA output data frame. If NULL, reads the latest CSV output.
#'
#' @return Invisibly returns the summary list.
#' @export
aigra_print_summary <- function(data = NULL) {
  s <- aigra_summarise_output(data)

  message("AIGRA quality summary")
  message("=====================")
  message("Total rows:            ", s$total_rows)
  message("Generated clones:      ", s$total_generated)
  message("OK:                    ", s$total_ok)
  message("Edited:                ", s$total_edited)
  message("Rejected:              ", s$total_rejected)
  message("Pipeline failed:       ", s$total_pipeline_failed)
  message("Solver-key mismatches: ", s$solver_key_mismatches)
  message("Acceptance rate:       ", round(100 * s$acceptance_rate, 1), "%")
  message("Rejection rate:        ", round(100 * s$rejection_rate, 1), "%")

  invisible(s)
}

