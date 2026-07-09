# AIGRA 0.2.0

## User-facing change

* `aigra_generate_items()` now uses a data-frame-only interface.
  Users should load CSV or XLSX templates into R first and pass the resulting
  data frame through `data = ...`.

* Added `aigra_read_template()` to read AIGRA CSV and XLSX templates into R.

* Added `aigra_generate_from_data()` as an alias for data-frame generation.

* Added internal UTF-8 temporary CSV handling before calling the Python backend.
  This reduces common encoding problems for non-English item banks.

* Added validation for required AIGRA tabular columns before backend execution.

## Breaking change

* The direct `file_path` argument has been removed from `aigra_generate_items()`.
  Use `aigra_read_template()` first, then call `aigra_generate_items(data = ...)`.

# AIGRA 0.1.2

* Added `aigra_generate_items()` as a simplified generation wrapper around `aigra_generate_tabular_items()`.
* Added direct API-key arguments: `gemini.API`, `openai.API`, `groq.API`, and `anthropic.API`.
* Added `aigra_set_api_keys()` so provider keys can be supplied directly from R.
* Added `aigra_backend_help()` to provide clearer guidance for configuring the external `AIGRA_BACKEND` folder.
* Improved first-time user guidance for backend configuration and API-key handling.
* Fixed documentation and namespace issues related to the new user-facing functions.
