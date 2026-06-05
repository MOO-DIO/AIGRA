#' Detect whether an item likely requires a diagram
#'
#' Uses rule-based visual triggers in the item stem, topic, objective, and options.
#'
#' @param stem Item stem.
#' @param topic Optional topic.
#' @param objective Optional objective.
#' @param options Optional option text.
#'
#' @return TRUE or FALSE.
#' @export
aigra_detect_diagram_required <- function(
  stem,
  topic = "",
  objective = "",
  options = ""
) {
  text <- paste(stem, topic, objective, options, collapse = " ")
  text <- tolower(text)

  triggers <- c(
    "diagram",
    "figure",
    "fig\\.",
    "shown",
    "shown below",
    "shown above",
    "depicted",
    "illustrated",
    "picture",
    "image",
    "sketch",
    "draw",
    "graph",
    "plot",
    "chart",
    "coordinate",
    "axis",
    "axes",
    "angle",
    "triangle",
    "circle",
    "curve",
    "ray diagram",
    "lens",
    "mirror",
    "prism",
    "circuit",
    "resistor",
    "battery",
    "ammeter",
    "voltmeter",
    "switch",
    "wire",
    "inclined plane",
    "ramp",
    "pulley",
    "lever",
    "spring",
    "pendulum",
    "free-body",
    "free body",
    "force diagram",
    "vector",
    "trajectory",
    "projectile",
    "block",
    "cart"
  )

  pattern <- paste(triggers, collapse = "|")

  grepl(pattern, text, perl = TRUE)
}


aigra_diagram_agent_trueish <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "t", "1", "yes", "y")
}


#' Build a diagram prompt from a result row
#'
#' @param result A data frame returned by AIGRA.
#' @param row Row number.
#'
#' @return A diagram prompt.
#' @export
aigra_build_diagram_prompt_from_row <- function(result, row = 1) {
  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
  }

  if (row < 1 || row > nrow(result)) {
    stop("row is out of range.")
  }

  stem <- if ("clone_stem" %in% names(result)) {
    result$clone_stem[row]
  } else if ("stem" %in% names(result)) {
    result$stem[row]
  } else {
    ""
  }

  topic <- if ("original_topic" %in% names(result)) {
    result$original_topic[row]
  } else if ("topic" %in% names(result)) {
    result$topic[row]
  } else {
    ""
  }

  objective <- if ("original_objective" %in% names(result)) {
    result$original_objective[row]
  } else if ("objective" %in% names(result)) {
    result$objective[row]
  } else {
    ""
  }

  opt_A <- if ("opt_A" %in% names(result)) result$opt_A[row] else ""
  opt_B <- if ("opt_B" %in% names(result)) result$opt_B[row] else ""
  opt_C <- if ("opt_C" %in% names(result)) result$opt_C[row] else ""
  opt_D <- if ("opt_D" %in% names(result)) result$opt_D[row] else ""

  paste(
    "Create a clean educational assessment diagram in simple black-and-white textbook line-art style.",
    "The diagram should support the item without revealing the correct answer.",
    "If the stem refers to a figure, diagram, graph, circuit, object shown below, or physical setup, create the missing visual implied by the stem.",
    "Use only necessary labels, arrows, objects, measurements, axes, angles, and physical quantities.",
    "Do not include answer-choice labels such as A, B, C, or D.",
    "Do not include the correct answer or any explanation.",
    "Avoid decorative art. Keep the diagram simple, clear, and exam-ready.",
    paste("Item stem:", stem),
    paste("Topic:", topic),
    paste("Objective:", objective),
    paste(
      "Options for context only, not to be displayed as answer choices:",
      paste(c(opt_A, opt_B, opt_C, opt_D), collapse = "; ")
    )
  )
}


#' Apply AIGRA Diagram Agent to generated results
#'
#' Inspects generated items and automatically sets diagram_required and
#' diagram_prompt when a visual diagram is likely needed.
#'
#' @param result A data frame returned by AIGRA.
#' @param overwrite_prompt If TRUE, replace existing diagram prompts.
#'
#' @return Updated result data frame.
#' @export
aigra_apply_diagram_agent <- function(
  result,
  overwrite_prompt = FALSE
) {
  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
  }

  if (!"diagram_required" %in% names(result)) {
    result$diagram_required <- FALSE
  }

  if (!"diagram_prompt" %in% names(result)) {
    result$diagram_prompt <- NA_character_
  }

  if (!"diagram_agent_detected" %in% names(result)) {
    result$diagram_agent_detected <- FALSE
  }

  if (!"diagram_reason" %in% names(result)) {
    result$diagram_reason <- NA_character_
  }

  for (i in seq_len(nrow(result))) {
    stem <- if ("clone_stem" %in% names(result)) {
      result$clone_stem[i]
    } else if ("stem" %in% names(result)) {
      result$stem[i]
    } else {
      ""
    }

    topic <- if ("original_topic" %in% names(result)) {
      result$original_topic[i]
    } else if ("topic" %in% names(result)) {
      result$topic[i]
    } else {
      ""
    }

    objective <- if ("original_objective" %in% names(result)) {
      result$original_objective[i]
    } else if ("objective" %in% names(result)) {
      result$objective[i]
    } else {
      ""
    }

    options <- paste(
      if ("opt_A" %in% names(result)) result$opt_A[i] else "",
      if ("opt_B" %in% names(result)) result$opt_B[i] else "",
      if ("opt_C" %in% names(result)) result$opt_C[i] else "",
      if ("opt_D" %in% names(result)) result$opt_D[i] else "",
      collapse = " "
    )

    current_required <- aigra_diagram_agent_trueish(result$diagram_required[i])

    detected <- aigra_detect_diagram_required(
      stem = stem,
      topic = topic,
      objective = objective,
      options = options
    )

    if (current_required || detected) {
      result$diagram_required[i] <- TRUE
      result$diagram_agent_detected[i] <- detected

      if (detected && !current_required) {
        result$diagram_reason[i] <- "Diagram Agent detected visual trigger words in the item."
      } else if (current_required) {
        result$diagram_reason[i] <- "Diagram was already marked as required."
      }

      prompt_missing <- is.na(result$diagram_prompt[i]) ||
        !nzchar(trimws(as.character(result$diagram_prompt[i])))

      if (prompt_missing || overwrite_prompt) {
        result$diagram_prompt[i] <- aigra_build_diagram_prompt_from_row(
          result,
          row = i
        )
      }
    }
  }

  result
}


#' Generate diagrams using the AIGRA Diagram Agent
#'
#' Applies the Diagram Agent, then generates diagrams for rows marked as requiring diagrams.
#'
#' @param result A data frame returned by AIGRA.
#' @param provider Image provider.
#' @param model Image model.
#' @param rows Optional row numbers to process.
#' @param max_images Maximum number of images to generate.
#' @param overwrite If TRUE, overwrite existing image files.
#'
#' @return Updated result data frame with diagram paths.
#' @export
aigra_generate_result_diagrams_auto <- function(
  result,
  provider = "gemini",
  model = "gemini-3-pro-image-preview",
  rows = NULL,
  max_images = 3,
  overwrite = FALSE
) {
  result <- aigra_apply_diagram_agent(result)

  aigra_generate_result_diagrams(
    result = result,
    provider = provider,
    model = model,
    rows = rows,
    only_required = TRUE,
    force = FALSE,
    max_images = max_images,
    overwrite = overwrite
  )
}
