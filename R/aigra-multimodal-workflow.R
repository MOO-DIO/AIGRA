
#' Generate multimodal items from a tabular item bank
#'
#' Runs item generation and diagram generation in one workflow.
#'
#' Text-only items are generated normally. Items that require diagrams are
#' processed through the Source Diagram Agent and Clone Diagram Agent.
#'
#' @param file_path Path to CSV or Excel item bank.
#' @param provider Text-generation provider.
#' @param model Text-generation model.
#' @param image_provider Image-generation provider.
#' @param image_model Image-generation model.
#' @param max_images Maximum diagrams to generate. Use NULL for all generated rows.
#' @param write_reports If TRUE, writes quality and administration HTML reports.
#' @param include_key If TRUE, include answer key in administration HTML.
#' @param only_accepted If TRUE, administration HTML includes only ok/edited rows.
#'
#' @param source_language Source language of the input item bank.
#' @param target_language Target language for generated items.
#' @param subject Subject area for item generation.
#' @param exam Examination, assessment, or project name.
#' @param n_clones Number of clones to generate per source item.
#' @param max_items Maximum number of source items to process.
#' @return A list containing result, result_with_diagrams, report_path, admin_file, and student_file.
#' @export
aigra_generate_multimodal_tabular_items <- function(
  file_path,
  provider = "gemini",
  model = "gemini-3.1-pro-preview",
  image_provider = "gemini",
  image_model = "gemini-3-pro-image-preview",
  source_language = "English",
  target_language = "English",
  subject = "General",
  exam = "AIGRA Multimodal Item Bank",
  n_clones = 1,
  max_items = NULL,
  max_images = NULL,
  write_reports = TRUE,
  include_key = TRUE,
  only_accepted = TRUE
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_set_backend()
  }

  file_path <- normalizePath(file_path, winslash = "/", mustWork = TRUE)

  message("AIGRA multimodal workflow starting")
  message("File: ", file_path)
  message("Text provider/model: ", provider, " / ", model)
  message("Image provider/model: ", image_provider, " / ", image_model)

  # 1. Validate/inspect item bank
  validation <- aigra_validate_tabular_items(file_path)
  aigra_print_validation(validation)

  # 2. Generate text clones
  result <- aigra_generate_tabular_items(
    file_path = file_path,
    provider = provider,
    model = model,
    source_language = source_language,
    target_language = target_language,
    subject = subject,
    exam = exam,
    n_clones = n_clones,
    max_items = max_items
  )

  # 3. Apply diagram agent and repair prompts
  result_fixed <- aigra_apply_diagram_agent(result)
  result_fixed <- aigra_repair_diagram_prompts(result_fixed)

  if (is.null(max_images)) {
    max_images <- nrow(result_fixed)
  }

  # 4. Generate diagrams only where needed
  result_with_diagrams <- aigra_generate_result_diagrams_auto(
    result_fixed,
    provider = image_provider,
    model = image_model,
    max_images = max_images,
    overwrite = TRUE
  )

  # 5. Write reports
  report_path <- NULL
  admin_file <- NULL
  student_file <- NULL

  if (isTRUE(write_reports)) {
    report_path <- aigra_write_report(result_with_diagrams)

    admin_file <- aigra_write_admin_html(
      result_with_diagrams,
      title = paste(exam, "- Teacher/Admin Version"),
      include_key = include_key,
      include_metadata = TRUE,
      only_accepted = only_accepted
    )

    student_file <- aigra_write_admin_html(
      result_with_diagrams,
      title = paste(exam, "- Student Version"),
      include_key = FALSE,
      include_metadata = FALSE,
      only_accepted = only_accepted
    )
  }

  message("AIGRA multimodal workflow complete")

  list(
    validation = validation,
    result = result,
    result_with_diagrams = result_with_diagrams,
    report_path = report_path,
    admin_file = admin_file,
    student_file = student_file
  )
}

