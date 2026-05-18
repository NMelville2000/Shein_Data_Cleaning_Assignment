# Shein Data Quality Pipeline

## Project overview

This repository contains an R based data quality workflow for the Shein assignment. The pipeline reads the raw SHEIN CSV file, validates the structure of the imported data, generates an exploratory diagnostics workbook, and produces a structurally cleaned output file ready for downstream missing value imputation. The workflow is consolidated into a single master script `pipeline.r` which runs three logical stages sequentially: configuration and ingestion (`00_config` and `01_ingest`), exploratory diagnostics (`02_diagnosis`), and structural cleaning (`03_clean`).

## Project objective

The purpose of this project is to build a clear, reproducible data quality pipeline that can:

- read the raw dataset safely and validate the input file
- identify missingness, duplicates, parsing issues, range problems, text issues, URL issues, and extracted description attributes
- save the diagnostics into a workbook that supports the written report and cleaning decisions
- clean and restructure the raw data into a single wide output file suitable for imputation workflows

## Current workflow

The pipeline is organised as a single file `pipeline.r` containing three logical stages that run top to bottom. Run the entire pipeline smoothly in `run_pipeline.r`. 

### Stage 1 — Configuration and ingestion (`00_config` + `01_ingest`)

This stage defines project paths and runtime constants, creates the output directory, validates that the raw CSV exists, loads all required libraries, and reads the raw CSV with `data.table::fread()`. The data is kept as a `data.table` throughout to avoid unnecessary type conversions. Runtime constants include the Excel sample row limit (capped at 1,000 for readable workbook sheets), the IQR multiplier for outlier detection, the long text threshold, the set of messy missing values used in numeric parsing, and the accepted date parsing orders.

### Stage 2 — Exploratory diagnostics (`02_diagnosis`)

This stage converts the raw data to a `data.frame` locally for dplyr based diagnostics work. It includes helper functions for Excel safe text cleaning, automatic column detection, and workbook writing. It builds summaries for overview metrics, detected columns, description key value extraction, variable types, missingness (computed once and reused across summaries), duplicate checks, image URL within-record duplication, numeric range checks, numeric parsing issues, price and discount checks, URL checks, datetime checks, text quality, level summaries, and flagged problem rows. The workbook writes twenty sheets to `Output/shein_exploratory_diagnostics.xlsx`.

### Stage 3 — Structural cleaning (`03_clean`)

This stage operates on a `copy()` of the raw `data.table` and performs the following cleaning steps:

**Section A–B: Working copy and drop log.** Creates an independent copy of the data and initialises a drop log that records every column removed or replaced with the action taken and the reason.

**Section C: SKU.** Strips the `SKU:` prefix and deduplicates rows by SKU.

**Section D: Price.** Strips currency symbols, coerces to numeric, and adds a `price_flag` column (missing, invalid, outlier_high, or ok). Includes a guard against renaming the column to itself if the detected column is already named `price`.

**Section E: Brand.** Fixes mojibake encoding using `stringi::stri_trans_general()`, extracts the brand name before corrupted characters, and sets empty or unrecoverable brands to `NA`.

**Section F: Color.** Extracts the Color value from the serialised description field using `str_match()` with a capturing group that handles zero or more spaces after the colon. Drops the description column entirely; only Color is retained for imputation use. All other description attributes (Material, Style, etc.) are dropped below coverage threshold.

**Section G: Size.** This is the most complex cleaning step and handles multiple size systems in a single column. The process follows five sub-steps:

1. *Spacing normalisation* — collapses internal spaces and normalises parenthesis and comma spacing so that variants like `XS (2),S (4)` and `XS(2),S(4)` are treated identically before any classification.

2. *Modifier flags* — extracts `has_petite`, `has_tall`, and `has_plus` as boolean flags before classification, so multi-fit strings that span petite plus standard or petite plus plus are correctly described.

3. *Revised classification* — applies a priority ordered `fcase()` that classifies each row into one of: `one_size`, `missing`, `dimensions`, `shoe_intl`, `shoe_us`, `bra`, `kids`, `jeans`, `volume_length`, `plus`, `us_clothing`, `eu_clothing`, `standard`, `us_letter`, or `unknown`. Key fixes from supervisor review include: anchoring `shoe_us` with `^US` to avoid matching waist sizes, splitting `shoe_intl` to handle CN and EUR separately so EUR alone does not match EU clothing sizes, moving `volume_length` above `us_letter` so strings like `1.5M` classify correctly, extending bra cup patterns to include DD and AA, and using word boundary matching for Tall.

4. *Vectorised column extraction* — uses `data.table` subset-assign per system instead of row-by-row `mapply()`. Produces `size_labels`, `size_us`, `size_eu`, `dimensions_raw`, `waist`, `inseam` as applicable per system with `NA` for non-applicable systems.

5. *Size count fix* — marks range-notation rows (containing a hyphen but not an asterisk) as `NA` for count rather than silently undercounting.

**Section H: Images.** Extracts and deduplicates URLs from the raw scraped image string, producing `images_clean` (pipe-separated deduplicated URLs) and `images_n` (count of unique images per product). Drops the raw `images` column.

**Sections I–K: Assembly, summary, and export.** Assembles the final cleaned data frame with a defined column priority order, prints summary diagnostics to the console (column names, missing value counts, size system distribution, price flag distribution, and the drop log), and exports to `Output/shein_cleaned.csv` using `fwrite()`.

## Final cleaned output columns

The cleaned output `shein_cleaned.csv` contains the following columns in priority order:

- `sku` — deduplicated product identifier
- `url` — product URL
- `name` — product name
- `price` — numeric price (currency symbol stripped)
- `price_flag` — missing, invalid, outlier_high, or ok
- `brand` — cleaned brand name (mojibake fixed, empty set to NA)
- `color` — extracted from description field
- `size_system` — classified size system (one_size, us_clothing, standard, shoe_intl, dimensions, etc.)
- `size_count` — number of size options (NA for range notation and non-countable systems)
- `size_labels` — letter or label component of sizes
- `size_us` — US numeric size equivalents
- `size_eu` — EU numeric size equivalents (shoe_intl and eu_clothing only)
- `dimensions_raw` — raw dimension string (bedding products only)
- `waist` — waist measurement (jeans only)
- `inseam` — inseam measurement (jeans only)
- `has_petite` — boolean modifier flag
- `has_tall` — boolean modifier flag
- `has_plus` — boolean modifier flag
- `images_n` — count of unique image URLs per product
- `images_clean` — deduplicated pipe-separated image URLs

Columns for non-applicable size systems are left as `NA` intentionally to preserve the one-file structure required for imputation grouping.

## Repository structure

```text
Shein_Data_Cleaning_Assignment/
├── pipeline.r
├── 00_config.R
├── 01_ingest.R
├── 02_diagnosis.R
├── 03_clean.R
├── README.md
├── Shein_Assignment_Progress_Report_1.docx
├── Raw_data/
│   └── shein_sample.csv
└── Output/
    ├── shein_exploratory_diagnostics.xlsx
    └── shein_cleaned.csv
```

The standalone scripts (`00_config.R`, `01_ingest.R`, `02_diagnosis.R`, `03_clean.R`) are earlier development versions. The consolidated `pipeline.r` is the current master script and should be used for running the full workflow.

## Required packages

The pipeline relies on the following R packages:

- `data.table` — fast CSV ingestion with `fread()`, in-memory cleaning, and export with `fwrite()`
- `dplyr` — data manipulation in the diagnostics stage
- `readr` — `parse_number()` and type helpers
- `stringr` — string utilities and regex extraction
- `stringi` — encoding safe string operations and mojibake fixes
- `tidyr` — reshape helpers in diagnostics
- `lubridate` — date parsing
- `purrr` — `map_dfr()` and `map2_dfr()` functional helpers
- `scales` — formatting for output
- `openxlsx` — Excel workbook creation

Install with:

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

The pipeline expects the raw dataset to be stored at:

```text
Raw_data/shein_sample.csv
```

If the file is missing, the pipeline stops with a clear error message telling the user where to place it.

## How to run the project

Run the consolidated pipeline from start to finish:

```r
source("pipeline.r")
```

This executes all three stages sequentially: configuration and ingestion, diagnostics, and cleaning. The pipeline produces both the diagnostics workbook and the cleaned CSV in the `Output` folder.

## Main outputs

The pipeline produces two outputs:

**Diagnostics workbook** — `Output/shein_exploratory_diagnostics.xlsx` containing twenty sheets covering dataset overview, detected columns, variable types, missingness by variable and row, duplicate checks, image URL duplication summary, numeric range checks, numeric parse issues, price and discount checks, URL checks, datetime checks, text quality, level summaries, description attribute extraction, and problem rows. Excel sheets are capped at 1,000 rows for readability.

**Cleaned dataset** — `Output/shein_cleaned.csv` containing one row per unique SKU with all cleaning transformations applied. This file is structured for downstream missing value imputation with all size system columns present regardless of applicability.

## Diagnostics currently covered

### 1. Overview and structure

Total rows, total columns, total cells, exact duplicate rows, rows with at least one missing value, and columns with at least one missing value.

### 2. Column detection

A helper function searches column names with heuristic patterns to identify fields such as product name, price, discount, color, size, category, material, product URL, image URL, and scraped timestamp.

### 3. Description key value extraction

Extracts attribute name and attribute value pairs from the description column using a regex pattern for `'Field': 'Value'` syntax. Summarises unique attribute names, unique attribute values, and attribute name plus value combinations.

### 4. Missingness

Missing values are computed once and reused across both the variable types summary and the missingness summary. Blank strings are treated as missing. Row level missingness is also summarised.

### 5. Duplicates

Checks for exact duplicate rows, duplicate product URLs, duplicate product names, duplicate image URLs, and records containing repeated image URLs within their image lists. The within-record image check uses a vectorised `vapply()` approach.

### 6. Numeric parsing and range checks

Numeric and numeric-like columns are identified by type and name patterns. The workflow parses numbers, records failed parses, calculates summary statistics, and flags IQR based outliers, negatives, and zeros. Values such as `undefined`, `null`, and similar strings are treated as messy missing values.

### 7. Price and discount checks

Where relevant columns are detected, checks for prices that could not be parsed, prices below zero, prices equal to zero, discounts below zero, and discounts above one hundred.

### 8. URL checks

Product and image URLs are checked for basic web format validity using `http://` or `https://` prefixes, and duplicates are counted.

### 9. Datetime checks

If a scraped timestamp column is found, attempts to parse values using predefined date orders and reports parsed counts, unparsed counts, earliest date, and latest date.

### 10. Text quality checks

Performs encoding safe text cleaning and checks blank strings, leading or trailing spaces, repeated spaces, newline characters, very long text, maximum text length, and unique counts for character columns.

### 11. Problem rows

Creates an issue flag table and exports a sample of rows with one or more detected issues for manual review and evidence collection.

## Cleaning transformations applied

The cleaning stage (`03_clean`) applies the following transformations, each logged in the drop log:

1. **SKU** — prefix stripped, rows deduplicated by SKU
2. **Price** — currency symbol stripped, coerced to numeric, flagged for missing/invalid/outlier values
3. **Brand** — mojibake fixed via latin-ascii transliteration, brand name extracted before corrupted characters, empty values set to NA
4. **Color** — extracted from serialised description field using a capturing group regex, description column dropped
5. **Size** — spacing normalised, modifier flags extracted, classified into size systems, structured columns derived per system using vectorised data.table operations, range-notation counts marked as NA
6. **Images** — URLs extracted and deduplicated, count computed, raw column replaced

## Performance notes

Several optimisations were applied during the review process:

- Raw data is kept as a `data.table` from `fread()` through to cleaning, avoiding an unnecessary `data.table` to `data.frame` to `data.table` round-trip
- Missing counts are computed once and reused across multiple diagnostic summaries
- The within-record image URL duplication check uses `vapply()` instead of `rowwise()` which was extremely slow on 111k rows
- An unused `separate_rows()` image explosion that computed results never written to the workbook was removed
- Export uses `fwrite()` instead of `write.csv()` for faster output
- Size column extraction uses vectorised `data.table` subset-assign per system instead of row-by-row `mapply()`

## Troubleshooting

### Error: raw data file not found

Make sure the file is stored at:

```text
Raw_data/shein_sample.csv
```

### Error: more columns than column names

This usually means the CSV contains irregular rows or messy quoting. The pipeline uses `fread()` with `fill = TRUE` to handle this more safely than base `read.csv()`.

### Invalid UTF 8 byte sequence errors

These can happen when scraped text contains broken encoding. The workflow uses `stringi::stri_enc_toutf8()` and removes control characters before Excel export and text based checks.

### Parsing failures such as `undefined`

The pipeline treats several strings as messy missing values during numeric parsing. You can expand `MESSY_MISSING_VALUES` in the config section if new placeholders appear.

## Author notes

This README reflects the current state of `pipeline.r` after supervisor review and corrections. It is designed to work as both assignment documentation and a setup guide for anyone cloning the repository.
