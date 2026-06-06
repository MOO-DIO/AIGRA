# AIGRA 0.1.2

* Added `aigra_generate_items()` as a simplified generation wrapper around `aigra_generate_tabular_items()`.
* Added direct API-key arguments: `gemini.API`, `openai.API`, `groq.API`, and `anthropic.API`.
* Added `aigra_set_api_keys()` so provider keys can be supplied directly from R.
* Added `aigra_backend_help()` to provide clearer guidance for configuring the external `AIGRA_BACKEND` folder.
* Improved first-time user guidance for backend configuration and API-key handling.
* Fixed documentation and namespace issues related to the new user-facing functions.
