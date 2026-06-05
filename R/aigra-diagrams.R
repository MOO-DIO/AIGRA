#' Build a safe diagram prompt from an item stem
#'
#' @param stem Item stem.
#' @param option_A Option A.
#' @param option_B Option B.
#' @param option_C Option C.
#' @param option_D Option D.
#'
#' @return A diagram-generation prompt.
#' @export
aigra_build_diagram_prompt <- function(
  stem,
  option_A = "",
  option_B = "",
  option_C = "",
  option_D = ""
) {
  paste(
    "Create a clean educational assessment diagram in simple black-and-white textbook line-art style.",
    "The diagram should support the item without revealing the answer.",
    "Use only necessary labels, arrows, objects, and measurements.",
    "Avoid decorative art and avoid extra explanation.",
    paste("Item stem:", stem),
    paste(
      "Options are for context only and should not be displayed as answer choices:",
      paste(
        c(option_A, option_B, option_C, option_D),
        collapse = "; "
      )
    )
  )
}


#' Generate a single AIGRA diagram from a prompt
#'
#' @param prompt Diagram prompt.
#' @param output_path Output PNG path.
#' @param provider Image provider.
#' @param model Image model.
#' @param size Image size.
#'
#' @return Output path.
#' @export
aigra_generate_diagram <- function(
  prompt,
  output_path,
  provider = "gemini",
  model = "gemini-2.5-flash-image",
  size = "1024x1024"
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)

  image_clients <- reticulate::import("aigra_backend.image_clients")

  client <- image_clients$AigraImageClient(
    provider = provider,
    model = model
  )

  saved <- client$generate_image(
    prompt = prompt,
    output_path = output_path,
    size = size
  )

  reticulate::py_to_r(saved)
}


#' Plot an AIGRA diagram image
#'
#' @param image_path Path to a PNG image.
#'
#' @return Invisibly returns the image path.
#' @export
aigra_plot_diagram <- function(image_path) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required. Install it with install.packages('png').")
  }

  image_path <- normalizePath(image_path, winslash = "/", mustWork = TRUE)

  img <- png::readPNG(image_path)

  grid::grid.newpage()
  grid::grid.raster(img)

  invisible(image_path)
}


aigra_is_trueish <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "t", "1", "yes", "y")
}


#' Generate diagrams for rows in an AIGRA result
#'
#' Generates diagram images for result rows and returns the updated data frame
#' with diagram_path, diagram_prompt, image_provider, and image_model columns.
#'
#' @param result A data frame returned by AIGRA.
#' @param output_dir Directory for generated images.
#' @param provider Image provider.
#' @param model Image model.
#' @param rows Optional row numbers to process.
#' @param only_required If TRUE, process only rows where diagram_required is true.
#' @param force If TRUE, generate even when diagram_required is false or missing.
#' @param max_images Maximum number of images to generate.
#' @param overwrite If TRUE, overwrite existing image files.
#'
#' @return Updated result data frame.
#' @export
aigra_generate_result_diagrams <- function(
  result,
  output_dir = NULL,
  provider = "gemini",
  model = "gemini-2.5-flash-image",
  rows = NULL,
  only_required = TRUE,
  force = FALSE,
  max_images = 1,
  overwrite = FALSE
) {
  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
  }

  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  if (is.null(output_dir)) {
    output_dir <- file.path(.aigra_env$backend_path, "outputs", "diagrams")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  needed <- c("diagram_prompt", "diagram_path", "image_provider", "image_model")
  for (col in needed) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_character_
    }
  }

  if (!"diagram_required" %in% names(result)) {
    result$diagram_required <- FALSE
  }

  if (is.null(rows)) {
    rows <- seq_len(nrow(result))
  }

  generated <- 0

  for (i in rows) {
    if (generated >= max_images) {
      break
    }

    required <- aigra_is_trueish(result$diagram_required[i])

    if (only_required && !required && !force) {
      next
    }

    stem <- if ("clone_stem" %in% names(result)) result$clone_stem[i] else ""

    if (is.na(stem) || !nzchar(stem)) {
      next
    }

    prompt <- result$diagram_prompt[i]

    if (is.na(prompt) || !nzchar(prompt)) {
      prompt <- aigra_build_diagram_prompt(
        stem = stem,
        option_A = if ("opt_A" %in% names(result)) result$opt_A[i] else "",
        option_B = if ("opt_B" %in% names(result)) result$opt_B[i] else "",
        option_C = if ("opt_C" %in% names(result)) result$opt_C[i] else "",
        option_D = if ("opt_D" %in% names(result)) result$opt_D[i] else ""
      )

      result$diagram_prompt[i] <- prompt
    }

    safe <- gsub("[^A-Za-z0-9_-]+", "_", substr(stem, 1, 60))
    safe <- gsub("^_+|_+$", "", safe)

    if (!nzchar(safe)) {
      safe <- paste0("diagram_row_", i)
    }

    output_path <- file.path(output_dir, paste0(safe, ".png"))

    if (file.exists(output_path) && !overwrite) {
      result$diagram_path[i] <- normalizePath(output_path, winslash = "/", mustWork = TRUE)
      result$image_provider[i] <- provider
      result$image_model[i] <- model
      generated <- generated + 1
      next
    }

    saved <- aigra_generate_diagram(
      prompt = prompt,
      output_path = output_path,
      provider = provider,
      model = model
    )

    result$diagram_path[i] <- saved
    result$image_provider[i] <- provider
    result$image_model[i] <- model

    generated <- generated + 1
  }

  result
}


#' Show a diagram from an AIGRA result row
#'
#' @param result A data frame returned by AIGRA.
#' @param row Row number to display.
#'
#' @return Invisibly returns the image path.
#' @export
aigra_show_result_diagram <- function(result, row = 1) {
  if (!is.data.frame(result)) {
    stop("result must be a data frame.")
  }

  if (!"diagram_path" %in% names(result)) {
    stop("result does not contain a diagram_path column.")
  }

  if (row < 1 || row > nrow(result)) {
    stop("row is out of range.")
  }

  path <- result$diagram_path[row]

  if (is.na(path) || !nzchar(path)) {
    stop("No diagram path found for this row.")
  }

  aigra_plot_diagram(path)
}

#' List supported AIGRA image models
#'
#' Returns the image model registry known to AIGRA. This function is
#' CRAN-safe and does not call external APIs or require the Python backend.
#'
#' @param provider Optional image provider, such as "gemini" or "openai".
#'
#' @return Supported image models.
#' @export
aigra_image_models <- function(provider = NULL) {
  registry <- list(
    gemini = c(
      "gemini-2.5-flash-image",
      "gemini-3.1-flash-image-preview",
      "gemini-3-pro-image-preview"
    ),
    openai = c(
      "gpt-image-1"
    )
  )

  if (is.null(provider)) {
    return(registry)
  }

  provider <- tolower(trimws(as.character(provider)))

  if (!provider %in% names(registry)) {
    return(character())
  }

  registry[[provider]]
}


#' Generate a diagram with fallback image models
#'
#' Tries multiple image-generation models in order until one succeeds.
#'
#' @param prompt Diagram prompt.
#' @param output_path Output PNG path.
#' @param provider Image provider.
#' @param models Character vector of model names to try in order.
#' @param size Image size.
#'
#' @return Output path.
#' @export
aigra_generate_diagram_fallback <- function(
  prompt,
  output_path,
  provider = "gemini",
  models = c(
    "gemini-2.5-flash-image",
    "gemini-3.1-flash-image-preview",
    "gemini-3-pro-image-preview"
  ),
  size = "1024x1024"
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)

  image_clients <- reticulate::import("aigra_backend.image_clients")

  saved <- image_clients$generate_image_with_fallback(
    prompt = prompt,
    output_path = output_path,
    provider = provider,
    models = as.list(models),
    size = size
  )

  reticulate::py_to_r(saved)
}
