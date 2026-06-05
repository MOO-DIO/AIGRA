# AIGRA

**AIGRA** stands for **Agentic Item Generation, Review, and Analysis**.

AIGRA is an R package interface to a Python backend for generating, solving, reviewing, and exporting assessment items using retrieval-augmented multi-agent AI workflows.

## Core workflow

AIGRA supports the following pipeline:

1. Parse source assessment items from a PDF, CSV, or Excel item bank.
2. Build a retrieval-augmented item store.
3. Retrieve similar examples by topic, difficulty, objective, and style.
4. Generate new clone items in a target language.
5. Solve generated items independently.
6. Review generated items with a critic agent.
7. Apply rule-based safety checks.
8. Export results to CSV and JSONL.
9. Summarise, plot, and report output quality.

## Backend

The current Python backend lives at:

```r
"C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND"
```

## Development setup

```r
devtools::load_all()

aigra_set_backend(
  backend_path = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND"
)

aigra_status()
```

## Ensure Python backend

```r
ensure_aigra_python(
  backend_path = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND"
)

aigra_python_info(
  backend_path = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND"
)
```

## Parse PDF source items

```r
items <- aigra_parse_items(
  source_language = "Russian",
  subject = "Physics",
  exam = "Kazakhstan UNT"
)

nrow(items)
head(items)
```

## Create CSV or Excel item-bank templates

```r
template <- aigra_template_items()
template

csv_path <- aigra_write_template_csv(
  file = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND/data/aigra_item_template.csv",
  overwrite = TRUE
)

xlsx_path <- aigra_write_template_excel(
  file = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND/data/aigra_item_template.xlsx",
  overwrite = TRUE
)
```

## Required tabular item-bank columns

CSV and Excel item banks should include these columns:

- item_id
- stem
- option_A
- option_B
- option_C
- option_D
- correct_answer
- difficulty
- grade
- section
- topic
- objective
- source_language
- subject
- exam

Required minimum columns are:

- stem
- option_A
- option_B
- option_C
- option_D
- correct_answer

## Parse CSV or Excel item banks

```r
tab_items <- aigra_parse_tabular_items(
  file_path = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND/data/aigra_item_template.csv",
  source_language = "English",
  subject = "Physics",
  exam = "Demo Item Bank"
)

nrow(tab_items)
head(tab_items)
```

## Generate from PDF

Set your provider API key first.

```r
Sys.setenv(OPENAI_API_KEY = "your_openai_key_here")

result <- aigra_generate_items(
  provider = "openai",
  model = "gpt-4.1",
  target_language = "English",
  n_clones = 1,
  max_items = 1
)

View(result)
```

## Generate from CSV or Excel

```r
result <- aigra_generate_tabular_items(
  file_path = "C:/Users/OMOPEKUNOLA MOSES O/AIGRA_BACKEND/data/aigra_item_template.csv",
  provider = "openai",
  model = "gpt-4.1",
  source_language = "English",
  target_language = "English",
  subject = "Physics",
  exam = "Demo Item Bank",
  n_clones = 1,
  max_items = 1
)

View(result)
```

## Supported providers

AIGRA currently supports:

- OpenAI
- Gemini
- Groq
- Anthropic/Claude, through the provider-flexible backend client

Example provider keys:

```r
Sys.setenv(OPENAI_API_KEY = "your_openai_key_here")
Sys.setenv(GEMINI_API_KEY = "your_gemini_key_here")
Sys.setenv(GROQ_API_KEY = "your_groq_key_here")
Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_key_here")
```

## Read outputs

```r
aigra_list_outputs()

latest <- aigra_read_latest_output()
View(latest)
```

## Summarise output quality

```r
aigra_print_summary()
aigra_plot_summary()

report_path <- aigra_write_report()
browseURL(report_path)
```

## Development status

AIGRA is currently a research prototype. Generated assessment items should be reviewed by subject matter experts before operational use.
