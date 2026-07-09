
#' Translate/localize AIGRA diagram prompts using an LLM
#'
#' This rewrites diagram prompts into the target language before image generation.
#' Scientific symbols and units are preserved.
#'
#' @param result A data frame returned by AIGRA.
#' @param provider Text LLM provider.
#' @param model Text LLM model.
#'
#' @param target_language Target language for translated diagram prompts.
#' @return Updated result data frame.
#' @export
aigra_translate_diagram_prompts <- function(
  result,
  target_language = 'English',
  provider = 'gemini',
  model = 'gemini-3.1-pro-preview'
) {
  if (!is.data.frame(result)) {
    stop('result must be a data frame.')
  }

  if (!'diagram_prompt' %in% names(result)) {
    result$diagram_prompt <- NA_character_
  }

  if (tolower(target_language) == 'english') {
    return(result)
  }

  if (is.null(.aigra_env$backend_path)) {
    aigra_set_backend()
  }

  llm_mod <- reticulate::import('aigra_backend.llm_clients')
  client <- llm_mod$AigraLLMClient(
    provider = provider,
    model = model
  )

  system_prompt <- paste(
    'You are the Diagram Translation Agent for AIGRA.',
    'Your task is to rewrite diagram-generation prompts into the target language.',
    'Translate all visible text labels, titles, annotations, and explanatory words.',
    'Preserve scientific symbols and units such as mg, N, V, \u03A9, m/s, kg, s, F, 2F, \u00B0.',
    'Do not translate answer options A, B, C, D.',
    'Do not add explanations.',
    'Return only the translated diagram prompt.'
  )

  for (i in seq_len(nrow(result))) {
    prompt <- result$diagram_prompt[i]

    if (is.na(prompt) || !nzchar(trimws(as.character(prompt)))) {
      next
    }

    user_prompt <- paste(
      'Target language:', target_language,
      '',
      'Rewrite the following diagram prompt so that every visible word in the generated diagram is in the target language.',
      'Keep mathematical/scientific symbols and units unchanged.',
      'The image model must not use English labels unless the target language is English.',
      '',
      'Diagram prompt:',
      prompt,
      sep = '\n'
    )

    translated <- client$generate(
      system_prompt = system_prompt,
      user_prompt = user_prompt,
      temperature = 0,
      max_tokens = 1500
    )

    translated <- trimws(as.character(translated))

    if (nzchar(translated)) {
      result$diagram_prompt[i] <- paste(
        translated,
        paste(
          'STRICT IMAGE TEXT RULE:',
          paste0('All visible words in the diagram must be in ', target_language, '.'),
          'Do not include English words in titles, axis labels, object labels, or annotations.',
          'Keep only scientific symbols and units unchanged.'
        )
      )
    }
  }

  result
}

