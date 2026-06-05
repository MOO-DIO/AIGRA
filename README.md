# AIGRA: Agentic Item Generation, Review, and Analysis

`AIGRA` is an R package for agentic assessment item generation, review, and reporting. It supports structured item-bank templates, clone generation, automated review, diagram-aware item workflows, and HTML reporting.

The package is designed for researchers, assessment developers, psychometricians, and educators who want to generate and review assessment item clones from structured source items.

## Installation

Install the stable version from CRAN:

```r
install.packages("AIGRA")
library(AIGRA)
```

The development version can be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("MOO-DIO/AIGRA")
library(AIGRA)
```

## Important note about the backend

The CRAN package provides the R interface. Full LLM-based generation currently requires the external Python backend, usually named `AIGRA_BACKEND`.

A typical local setup is:

```r
library(AIGRA)

Sys.setenv(AIGRA_BACKEND_PATH = "C:/AIGRA_BACKEND")
Sys.setenv(RETICULATE_PYTHON = "C:/AIGRA_BACKEND/.venv/Scripts/python.exe")

aigra_set_backend("C:/AIGRA_BACKEND")
aigra_status()
```

In version `0.1.2` and later, users can also call:

```r
aigra_backend_help()
```

to see backend setup guidance.

## API keys

API keys can be supplied from R. For example:

```r
Sys.setenv(GEMINI_API_KEY = "your_gemini_key")
Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_key")
Sys.setenv(OPENAI_API_KEY = "your_openai_key")
Sys.setenv(GROQ_API_KEY = "your_groq_key")
```

In version `0.1.2` and later, keys can also be supplied directly in the generation wrapper:

```r
out <- aigra_generate_items(
  file_path = template_file,
  model = "sonnet",
  anthropic.API = "your_anthropic_key",
  backend_path = "C:/AIGRA_BACKEND",
  source_language = "English",
  target_language = "English",
  subject = "Mathematics",
  exam = "AIGRA demonstration",
  n_clones = 1,
  max_items = 2
)
```

Do not share real API keys in scripts, screenshots, examples, or public repositories.

## Preparing an item template

`AIGRA` uses a tabular item-bank template. Each row represents one original source item. A basic template should contain columns such as:

```text
item_id
stem
option_A
option_B
option_C
option_D
correct_answer
difficulty
grade
section
topic
objective
subject
exam
source_language
target_language
source_diagram_required
source_diagram_path
source_diagram_type
```

For clone generation, the source item should provide enough information for the generated item to preserve:

```text
same assessed skill
same item format
same reasoning pattern
same difficulty level
same response structure
same diagram dependency, if applicable
same language requirements
```

## Minimal workflow

```r
library(AIGRA)

template_file <- file.choose()

items <- aigra_parse_tabular_items(template_file)

nrow(items)

out <- aigra_generate_tabular_items(
  file_path = template_file,
  provider = "gemini",
  model = "gemini-3.1-pro-preview",
  source_language = "English",
  target_language = "English",
  subject = "Mathematics",
  exam = "AIGRA trial item bank",
  n_clones = 1,
  max_items = 2
)

out
```

## Claude example

```r
out_claude <- aigra_generate_items(
  file_path = template_file,
  model = "sonnet",
  anthropic.API = "your_anthropic_key",
  backend_path = "C:/AIGRA_BACKEND",
  source_language = "English",
  target_language = "English",
  subject = "Mathematics",
  exam = "AIGRA Claude demonstration",
  n_clones = 1,
  max_items = 2
)
```

## Gemini example

```r
out_gemini <- aigra_generate_items(
  file_path = template_file,
  model = "gemini",
  gemini.API = "your_gemini_key",
  backend_path = "C:/AIGRA_BACKEND",
  source_language = "English",
  target_language = "English",
  subject = "Physics",
  exam = "AIGRA Gemini demonstration",
  n_clones = 1,
  max_items = 2
)
```

## Creating reports

After item generation, create a review report:

```r
report_path <- aigra_write_report(out)
browseURL(report_path)
```

Create an administration HTML file:

```r
admin_file <- aigra_write_admin_html(
  out,
  title = "AIGRA Generated Assessment Items",
  include_key = TRUE,
  include_metadata = TRUE,
  only_accepted = FALSE
)

browseURL(admin_file)
```

## Diagram-based items

For items that require diagrams:

```r
result_fixed <- aigra_apply_diagram_agent(out)
result_fixed <- aigra_repair_diagram_prompts(result_fixed)

result2 <- aigra_generate_result_diagrams_auto(
  result_fixed,
  provider = "gemini",
  model = "gemini-3-pro-image-preview",
  max_images = 2,
  overwrite = TRUE
)
```

At present, Claude can be used for item text generation, while Gemini can be used for image generation.

## Quality review

Generated items should be reviewed before use. Check:

```text
Is the clone faithful to the source item?
Is the key correct?
Are the distractors plausible?
Is the language appropriate?
Is the diagram accurate and necessary?
Does the solver answer match the key?
Is the item free from prompt leakage?
```

## Citation

If you use `AIGRA`, please cite the package and related methodological work. A formal citation entry will be added in a future release.
