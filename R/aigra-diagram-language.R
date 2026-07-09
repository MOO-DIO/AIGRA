
#' Localize AIGRA diagram prompts
#'
#' Adds a language instruction to diagram prompts so generated diagrams use
#' the target language for visible labels.
#'
#' @param result A data frame returned by AIGRA.
#'
#' @param target_language Target language for localized diagram prompts.
#' @return Updated result data frame.
#' @export
aigra_localize_diagram_prompts <- function(
  result,
  target_language = 'English'
) {
  if (!is.data.frame(result)) {
    stop('result must be a data frame.')
  }

  if (!'diagram_prompt' %in% names(result)) {
    result$diagram_prompt <- NA_character_
  }

  instruction <- paste(
    'LANGUAGE REQUIREMENT FOR THE DIAGRAM:',
    paste0('All visible words, labels, titles, annotations, and explanatory text in the diagram must be in ', target_language, '.'),
    'Translate labels from the source language into the target language.',
    'Keep standard scientific symbols, variables, and units unchanged where appropriate, such as mg, N, V, \u03A9, m/s, F, 2F, kg, s, and \u00B0.',
    'Do not use the source language for visible diagram labels unless the target language is the same.',
    sep = ' '
  )

  for (i in seq_len(nrow(result))) {
    prompt <- result$diagram_prompt[i]

    if (!is.na(prompt) && nzchar(trimws(as.character(prompt)))) {
      if (!grepl('LANGUAGE REQUIREMENT FOR THE DIAGRAM', prompt, fixed = TRUE)) {
        result$diagram_prompt[i] <- paste(prompt, instruction)
      }
    }
  }

  result
}

