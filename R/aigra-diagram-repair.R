
#' Repair diagram prompts for diagram-dependent AIGRA results
#'
#' Strengthens diagram prompts when a generated item requires a figure but
#' the prompt is too vague for solving/review.
#'
#' @param result A data frame returned by AIGRA.
#'
#' @return Updated result data frame.
#' @export
aigra_repair_diagram_prompts <- function(result) {
  if (!is.data.frame(result)) {
    stop('result must be a data frame.')
  }

  if (!'diagram_required' %in% names(result)) {
    result$diagram_required <- FALSE
  }

  if (!'diagram_prompt' %in% names(result)) {
    result$diagram_prompt <- NA_character_
  }

  for (i in seq_len(nrow(result))) {
    stem <- if ('clone_stem' %in% names(result)) result$clone_stem[i] else ''
    topic <- if ('original_topic' %in% names(result)) result$original_topic[i] else ''
    objective <- if ('original_objective' %in% names(result)) result$original_objective[i] else ''

    text <- tolower(paste(stem, topic, objective, collapse = ' '))

    needs_diagram <- aigra_detect_diagram_required(
      stem = stem,
      topic = topic,
      objective = objective,
      options = paste(
        if ('opt_A' %in% names(result)) result$opt_A[i] else '',
        if ('opt_B' %in% names(result)) result$opt_B[i] else '',
        if ('opt_C' %in% names(result)) result$opt_C[i] else '',
        if ('opt_D' %in% names(result)) result$opt_D[i] else ''
      )
    )

    prompt <- as.character(result$diagram_prompt[i])
    prompt_missing <- is.na(prompt) || !nzchar(trimws(prompt))
    prompt_too_vague <- grepl('diagram below|figure below|shown below', text) &&
      nchar(prompt, type = 'chars', allowNA = TRUE, keepNA = FALSE) < 250

    if (needs_diagram || prompt_missing || prompt_too_vague) {
      result$diagram_required[i] <- TRUE

      base_prompt <- aigra_build_diagram_prompt_from_row(result, i)

      if (grepl('standing wave|wave', text)) {
        extra <- paste(
          'For this wave diagram, include the full standing-wave pattern needed to solve the item.',
          'Clearly show fixed ends, nodes, antinodes, total string length, and the number of loops.',
          'If the item asks for wavelength, the diagram must make the wavelength inferable.'
        )
      } else if (grepl('inclined|plane|force|block|mass', text)) {
        extra <- paste(
          'For this mechanics diagram, clearly show the inclined plane, angle, block or object, mass label,',
          'and all relevant force arrows such as weight mg, normal reaction N, friction if present,',
          'and any resolved components needed for the item.'
        )
      } else if (grepl('graph|velocity|time|axis|axes', text)) {
        extra <- paste(
          'For this graph diagram, clearly show axis labels, units, key coordinates, curve or line shape,',
          'and any marked values needed to solve the item.'
        )
      } else if (grepl('circuit|battery|resistor|ohm|voltage|current', text)) {
        extra <- paste(
          'For this circuit diagram, clearly show the battery voltage, resistors/components, labels,',
          'and whether the components are in series or parallel.'
        )
      } else {
        extra <- 'Include all visual details required to solve the item.'
      }

      result$diagram_prompt[i] <- paste(base_prompt, extra)
    }
  }

  result
}

