# Shein Data Quality Pipeline

## Project overview

This repository contains an R based data quality workflow for the Shein assignment. The pipeline is set up to read the raw SHEIN CSV file, validate the structure of the imported data, generate an exploratory diagnostics workbook that documents issues found in scope, and produce a structurally cleaned output ready for downstream missing value imputation. The current workflow is built around four core scripts: `00_config.R`, `01_ingest.R`, `02_diagnosis.R`, and `03_clean.R`. These scripts define the project paths and constants, ingest the raw CSV with `fread()`, create a multi sheet Excel diagnostics workbook in the `Output` folder, and apply systematic structural cleaning to produce a single analysis ready file.

## Project objective

The purpose of this project is to build a clear, reproducible data quality pipeline that can:

- read the raw dataset safely
- validate that the input file exists and loads correctly
- identify missingness, duplicates, parsing issues, range problems, text issues, URL issues, and extracted description attributes
- apply systematic structural cleaning decisions with explicit rationale for each
- produce a single cleaned file preserving all size system columns so that imputation algorithms can group and operate on the full dataset without requiring a reshape step
- save the diagnostics into a workbook that can support the written report and cleaning decisions

## Current workflow

The current pipeline is organised into four scripts.

### `00_config.R`

This script stores the project paths, constants, and startup checks. It defines the raw file path as `Raw_data/shein_sample.csv`, the output directory as `Output`, the diagnostics workbook path as `Output/shein_exploratory_diagnostics.xlsx`, and the report file path as `Output/shein_data_quality_report.html`. It also defines runtime constants such as the Excel sample row limit, the IQR multiplier for outlier detection, the long text threshold, the set of messy missing values used in numeric parsing, and the accepted date parsing orders. It stops execution if the raw CSV is not found.

### `01_ingest.R`

This script loads the required libraries, reads the raw CSV with `data.table::fread()`, converts the result to a `data.frame`, validates that the data has rows and columns, and prints a quick structural review to the console. It is designed to expose `raw_file` for downstream scripts.

### `02_diagnosis.R`

This script creates the exploratory diagnostics workbook. It includes helper functions for Excel safe text cleaning, automatic column detection, and workbook writing. It builds summaries for overview metrics, detected columns, description key value extraction, variable types, missingness, duplicate checks, image URL duplication, numeric range checks, numeric parsing issues, price and discount checks, URL checks, datetime checks, text quality, level summaries, and flagged problem rows. The workbook currently writes twenty sheets to `Output/shein_exploratory_diagnostics.xlsx`.

### `03_clean.R`

This script applies systematic structural cleaning to produce a single wide `data.frame` named `df_clean`, one row per product SKU, ready for missing value imputation. It depends on `raw_file` from `01_ingest.R` and the `detected` column map from `02_diagnosis.R`, sourcing them automatically if they are not already in the environment. It maintains a `clean_drop_log` recording every column removed or replaced with explicit rationale, which is printed to the console at the end of every run. The cleaned dataset is exported to `Output/shein_cleaned.csv`.

The cleaning is organised into labelled sections A through J. Each section is described below.

## Cleaning decisions in `03_clean.R`

Each section below corresponds directly to a labelled section of the script. Rationale is stated explicitly because every structural decision affects imputation grouping downstream.

### Section A — Working copy

`raw_file` is converted to a `data.table` immediately using `as.data.table()` and `setDT()`. This ensures all subsequent `:=` assignment operations use `data.table` reference semantics without triggering shallow copy warnings.

### Section B — Drop log initialisation

A `clean_drop_log` `data.frame` is initialised with three columns: `column`, `action`, and `reason`. A helper function `log_drop()` appends one row per cleaning action throughout the script. The log is printed at the end of every run and is available in the environment for inspection. This ensures that no column is silently removed and that every structural decision is documented for the report.

### Section C — SKU

The raw SKU field contains a literal prefix of the form `SKU:` before the actual identifier. This is stripped with `str_remove()` and `str_trim()`. Deduplication is then applied by SKU using `data.table::unique(by = "sku")`, retaining the first occurrence of each product across scrape sessions. Row counts before and after deduplication are printed to the console.

### Section D — Price

Raw price values are stored as character strings such as `$7.25`. The dollar sign and any other non-numeric characters are stripped with `str_replace_all()` and the result is coerced to numeric. A companion flag column `price_flag` is added with four levels:

- `missing` — price could not be parsed or is `NA`
- `invalid` — price is zero or negative
- `outlier_high` — price exceeds $10,000
- `ok` — passes all checks

The original price column is replaced in place and renamed to `price`. The intermediate `price_clean` column is removed. The flag column is retained in the final output because downstream imputation models may want to exclude or weight flagged rows differently.

### Section E — Brand

The raw brand field contains mojibake caused by a corrupted character embedded by the scraper between the actual brand name and a category breadcrumb. For example, a value such as `QingSang\x3fAccessories Apparel Accessories` contains only `QingSang` as the true brand. The script applies `stri_trans_general()` with the `latin-ascii` transform to normalise the encoding, then uses `str_extract()` to take only the portion of the string before the first corrupted or unknown character. Empty strings and unrecoverable values are set to `NA_character_`.

### Section F — Color (extracted from description)

The `description` column holds a serialised Python-style list of attribute dictionaries such as `[{'Color': 'Black'}, {'Material': 'Woven Fabric'}]`. Rather than expanding all attributes into separate columns, only the `Color` value is extracted using a regex lookbehind pattern. This decision was made because `Color` is the only attribute with sufficient coverage to be analytically useful for imputation grouping. All other attributes, including `Material`, `Style`, and similar fields, fall below a meaningful coverage threshold when assessed across the full dataset.

After extraction, the `description` column is dropped entirely and recorded in the drop log. The `color` column is `NA` for products where no `Color` attribute is present in the description string.

### Section G — Size

The raw `size` field is a single comma delimited string that can represent fundamentally different measurement systems depending on the product type. A direct imputation on this field would conflate incompatible domains. The cleaning step classifies each row into one of the following size systems using `fcase()`:

- `one_size` — literal value is `one-size`
- `dimensions` — contains a dimension pattern such as `135*200` (bedding and home products)
- `bra` — contains a bra size pattern such as `34B(75B)`
- `kids` — contains age or inch-based size markers such as `4Y` or `IN)`
- `jeans` — contains waist-inseam patterns such as `W28 L30`
- `shoe_intl` — contains `CN` or `EUR` numeric size markers
- `shoe_us` — contains `US` numeric size markers without `CN` or `EUR`
- `petite` — contains the word `Petite`
- `tall` — contains the word `Tall`
- `plus` — contains extended size markers such as `0XL` through `6XL`
- `eu_clothing` — contains European clothing size patterns such as `XS(34)`
- `us_letter` — starts with a digit, indicating a US numeric size code
- `volume_length` — contains `ml`, `inch`, or similar volume or length markers
- `standard` — contains standard letter sizes such as `XS`, `S`, `M`, `L`, `XL`, `XXL`
- `unknown` — does not match any of the above

From the classified size string, three additional columns are derived:

- `size_labels` — the letter label portion of each size option, for example `XS, S, M, L` extracted from a `standard` or `eu_clothing` string, or the raw string for `shoe_us`
- `size_us` — the US numeric size extracted from parentheses for clothing systems, or from the `US` marker in `shoe_intl` strings
- `size_count` — the number of distinct size options available, set to `1` for `one_size` and `NA` for systems where counting options is not meaningful

Bedding and home products classified as `dimensions` populate `dimensions_raw` with the raw dimension string and leave `size_labels`, `size_us`, and `size_count` as `NA`. All other product types do the inverse: they populate the clothing size columns and leave `dimensions_raw` as `NA`. All size system columns are present in every row of the final output regardless of applicability. The original `size` column is dropped after extraction and recorded in the drop log.

This design decision was made deliberately so that the final file remains a single wide table. Imputation algorithms can group rows by `size_system` and operate on the appropriate column for each group without requiring a join or reshape step.

### Section H — Final assembly

After all cleaning sections have run, the handled columns are listed explicitly. Any remaining columns not addressed by sections C through G are identified and carried forward to avoid accidental loss. Priority columns are ordered first: `sku`, `url`, `name`, `price`, `price_flag`, `brand`, `color`, `size_system`, `size_count`, `size_labels`, `images`, `size_us`, and `dimensions_raw`. Remaining unhandled columns follow. The result is converted back to a `data.frame` named `df_clean`.

Note: the `images` column is carried forward in its raw form in the current version of this script. Dedicated image URL deduplication and count extraction is deferred to a subsequent cleaning stage.

### Section I — Summary and drop log

A console summary is printed on every run showing row count, column count, all column names, missing value counts per column, size system distribution, price flag distribution, and the full drop log. This makes every run self documenting and allows the analyst to verify structural outcomes without opening the output file.

### Section J — Export

The cleaned dataset is written to `Output/shein_cleaned.csv` using `write.csv()` with `na = ""` so that `NA` values are represented as empty cells rather than the literal string `NA`. This format is compatible with most imputation libraries in both R and Python.

## Repository structure

```text
Shein_Data_Cleaning_Assignment/
├── 00_config.R
├── 01_ingest.R
├── 02_diagnosis.R
├── 03_clean.R
├── README.md
├── readme.rmd
├── Raw_data/
│   └── shein_sample.csv
└── Output/
    ├── shein_exploratory_diagnostics.xlsx
    ├── shein_cleaned.csv
    └── shein_data_quality_report.html
```

## Required packages

The scripts currently rely on the following R packages:

- `data.table`
- `dplyr`
- `readr`
- `stringr`
- `stringi`
- `tidyr`
- `lubridate`
- `purrr`
- `scales`
- `openxlsx`

You can install them with:

```r
install.packages(c(
  "data.table",
  "dplyr",
  "readr",
  "stringr",
  "stringi",
  "tidyr",
  "lubridate",
  "purrr",
  "scales",
  "openxlsx"
))
```

## Input data requirements

The pipeline currently expects the raw dataset to be stored at:

```r
"Raw_data/shein_sample.csv"
```

The scripts assume the file is present before the workflow is run. If it is missing, `00_config.R` stops with a clear error message telling the user where the file should be placed.

## How to run the project

### Option 1. Run script by script

```r
source("00_config.R")
source("01_ingest.R")
source("02_diagnosis.R")
source("03_clean.R")
```

### Option 2. Run from the cleaning script

Since `03_clean.R` checks whether `raw_file` and `detected` already exist and sources the upstream scripts if needed, you can also run:

```r
source("00_config.R")
source("03_clean.R")
```

## Main output

The main outputs currently produced by the project are:

### Diagnostics workbook

```r
Output/shein_exploratory_diagnostics.xlsx
```

The workbook includes sheets for:

1. overview
2. detected columns
3. variable types
4. missingness by variable
5. missingness by row
6. duplicate summary
7. exact duplicate examples
8. duplicate URL examples
9. image URL duplication summary
10. range checks
11. numeric parse issues
12. price and discount checks
13. URL checks
14. datetime checks
15. text quality
16. level summary
17. description attribute names
18. description attribute values
19. description name value counts
20. problem rows

These sheets are explicitly created in the workbook writing section of `02_diagnosis.R`.

### Cleaned dataset

```r
Output/shein_cleaned.csv
```

A single wide CSV, one row per product SKU, containing the following columns:

| Column | Description |
|---|---|
| `sku` | Deduplicated product identifier with `SKU:` prefix stripped |
| `url` | Product page URL carried forward unchanged |
| `name` | Product name carried forward unchanged |
| `price` | Numeric price with currency symbol removed |
| `price_flag` | Quality flag: `ok`, `missing`, `invalid`, or `outlier_high` |
| `brand` | Cleaned brand name; `NA` where unrecoverable |
| `color` | Color attribute extracted from description; `NA` where absent |
| `size_system` | Classified size domain for this product |
| `size_count` | Number of distinct size options available |
| `size_labels` | Letter labels for clothing sizes; `NA` for non-clothing |
| `size_us` | US numeric sizes; `NA` for non-clothing |
| `dimensions_raw` | Raw dimension string for bedding products; `NA` for clothing |

## Diagnostics currently covered

The project currently checks the following areas.

### 1. Overview and structure

The workbook records total rows, total columns, total cells, exact duplicate rows, rows with at least one missing value, and columns with at least one missing value.

### 2. Column detection

A helper function searches column names and attempts to identify fields such as product name, price, discount, color, size, category, material, product URL, image URL, and scraped timestamp. These are heuristic matches based on column name patterns.

### 3. Description key value extraction

The workflow extracts attribute name and attribute value pairs from the `description` column using a regex pattern that looks for text of the form `'Field': 'Value'`. It then summarises unique attribute names, unique attribute values, and attribute name plus value combinations.

### 4. Missingness

Missing values are summarised both by variable and by row. Blank strings are also treated as missing for these checks.

### 5. Duplicates

The project checks for exact duplicate rows, duplicate product URLs, duplicate product names, duplicate image URLs, and records that contain repeated image URLs within exploded image lists.

### 6. Numeric parsing and range checks

Numeric like columns are identified by type and name patterns. The workflow parses numbers, records failed parses, calculates summary statistics, and flags IQR based outliers, negatives, and zeros. It also keeps a list of numeric parse issues. The scripts treat values such as `undefined`, `null`, and similar strings as messy missing values.

### 7. Price and discount checks

Where relevant columns are detected, the pipeline checks for prices that could not be parsed, prices below zero, prices equal to zero, discounts below zero, and discounts above one hundred.

### 8. URL checks

Product and image URLs are checked for basic web format validity using `http://` or `https://` prefixes, and duplicates are counted.

### 9. Datetime checks

If a scraped timestamp column is found, the pipeline attempts to parse the values using predefined date orders and reports parsed counts, unparsed counts, earliest date, and latest date.

### 10. Text quality checks

The workflow performs encoding safe text cleaning and then checks blank strings, leading or trailing spaces, repeated spaces, newline characters, very long text, maximum text length, and unique counts for character columns.

### 11. Problem rows

The project creates an issue flag table and then exports a sample of rows with one or more detected issues. These rows help with manual review and evidence collection for the diagnostics log and cleaning report.

## Progress made so far

Based on the current state of the scripts, the following work has been completed:

- central configuration file drafted
- raw data ingestion script drafted and validated
- exploratory diagnostics workflow built
- description field parsing and summarisation added
- workbook output for diagnostics created
- support for messy missing values added
- UTF 8 safe text cleaning added for Excel export and text quality checks
- image URL duplication review added
- problem row flagging added
- structural cleaning script `03_clean.R` drafted and completed covering SKU, Price, Brand, Color, and Size
- size system classification across fifteen categories implemented
- single file wide output design implemented so imputation can operate without a reshape step
- drop log implemented to document every column removal or replacement with explicit rationale
- cleaned CSV exported to `Output/shein_cleaned.csv`
- a simple progress report has already been prepared separately

## Suggested next steps

The project has moved from diagnostics into active cleaning. The next logical steps are:

1. add image URL deduplication and image count extraction as an extension to `03_clean.R`
2. review `unknown` size system rows manually to determine whether additional classification rules are warranted
3. create a diagnostics log and problem inventory for the report
4. produce a final report in HTML, Word, or both using `04_report.Rmd`
5. add a reproducible master script such as `run_pipeline.R`

## Suggested future repository structure

```text
Shein_Data_Cleaning_Assignment/
├── 00_config.R
├── 01_ingest.R
├── 02_diagnosis.R
├── 03_clean.R
├── 04_report.Rmd
├── run_pipeline.R
├── README.md
├── readme.rmd
├── Raw_data/
│   └── shein_sample.csv
└── Output/
    ├── shein_exploratory_diagnostics.xlsx
    ├── shein_cleaned.csv
    ├── diagnostics_log.xlsx
    ├── problem_inventory.txt
    └── shein_data_quality_report.html
```

## Reproducibility notes

To make the project easy for another user to run:

- keep all paths relative
- store the raw CSV in the `Raw_data` folder
- commit the scripts and README files
- include package installation instructions
- if the CSV is too large for regular Git, use Git LFS or provide a clear download instruction in this README

## Troubleshooting

### Error: raw data file not found

Make sure the file is stored at:

```text
Raw_data/shein_sample.csv
```

### Error: more columns than column names

This usually means the CSV contains irregular rows or messy quoting. The current pipeline uses `fread()` with `fill = TRUE` to handle this more safely than base `read.csv()`.

### Invalid UTF 8 byte sequence errors

These can happen when scraped text contains broken encoding. The current workflow uses `stringi::stri_enc_toutf8()` and removes control characters before Excel export and text based checks.

### Parsing failures such as `undefined`

The pipeline treats several strings as messy missing values during numeric parsing. You can expand `MESSY_MISSING_VALUES` in `00_config.R` if new placeholders appear.

### Size system classified as `unknown`

If a product's size string does not match any of the fifteen classification rules in Section G of `03_clean.R`, it is assigned the `unknown` category and its derived size columns are set to `NA`. Review the raw size values in this group to determine whether additional `fcase()` rules are warranted.

## Author notes

This README reflects the current project scripts and outputs already drafted in the repository and can be updated again once the reporting stage is added. It is designed to work as both assignment documentation and a setup guide for anyone cloning the repository.
