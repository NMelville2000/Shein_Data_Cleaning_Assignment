# Shein Data Quality Pipeline

## Project overview

This repository contains an R based data quality workflow for the Shein assignment. The pipeline is set up to read the raw SHEIN CSV file, validate the structure of the imported data, and generate an exploratory diagnostics workbook that documents issues found in scope. The current workflow is built around three core scripts: `00_config.R`, `01_ingest.R`, and `02_diagnosis.R`. These scripts define the project paths and constants, ingest the raw CSV with `fread()`, and create a multi sheet Excel diagnostics workbook in the `Output` folder.

## Project objective

The purpose of this project is to build a clear, reproducible data quality pipeline that can:

- read the raw dataset safely
- validate that the input file exists and loads correctly
- identify missingness, duplicates, parsing issues, range problems, text issues, URL issues, and extracted description attributes
- save the diagnostics into a workbook that can support the written report and cleaning decisions

## Current workflow

The current pipeline is organised into three scripts.

### `00_config.R`

This script stores the project paths, constants, and startup checks. It defines the raw file path as `Raw_data/shein_sample.csv`, the output directory as `Output`, the diagnostics workbook path as `Output/shein_exploratory_diagnostics.xlsx`, and the report file path as `Output/shein_data_quality_report.html`. It also defines runtime constants such as the Excel sample row limit, the IQR multiplier for outlier detection, the long text threshold, the set of messy missing values used in numeric parsing, and the accepted date parsing orders. It stops execution if the raw CSV is not found.

### `01_ingest.R`

This script loads the required libraries, reads the raw CSV with `data.table::fread()`, converts the result to a `data.frame`, validates that the data has rows and columns, and prints a quick structural review to the console. It is designed to expose `raw_file` for downstream scripts.

### `02_diagnosis.R`

This script creates the exploratory diagnostics workbook. It includes helper functions for Excel safe text cleaning, automatic column detection, and workbook writing. It builds summaries for overview metrics, detected columns, description key value extraction, variable types, missingness, duplicate checks, image URL duplication, numeric range checks, numeric parsing issues, price and discount checks, URL checks, datetime checks, text quality, level summaries, and flagged problem rows. The workbook currently writes twenty sheets to `Output/shein_exploratory_diagnostics.xlsx`.

## Repository structure

```text
Shein_Data_Cleaning_Assignment/
├── 00_config.R
├── 01_ingest.R
├── 02_diagnosis.R
├── README.md
├── readme.rmd
├── Raw_data/
│   └── shein_sample.csv
└── Output/
    ├── shein_exploratory_diagnostics.xlsx
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
```

### Option 2. Run from the diagnosis script

Since `02_diagnosis.R` checks whether `raw_file` already exists and sources `01_ingest.R` if needed, you can also run:

```r
source("00_config.R")
source("02_diagnosis.R")
```

## Main output

The main output currently produced by the project is:

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
- a simple progress report has already been prepared separately

## Suggested next steps

The project is already in a strong exploratory stage. The next logical steps are:

1. create a dedicated cleaning script such as `03_cleaning.R`
2. standardise missing values, URLs, parsed numeric fields, and text formatting
3. define the final cleaned variables required by the assignment scope
4. create a diagnostics log and problem inventory for the report
5. produce a final report in HTML, Word, or both
6. add a reproducible master script such as `run_pipeline.R`

## Suggested future repository structure

```text
Shein_Data_Cleaning_Assignment/
├── 00_config.R
├── 01_ingest.R
├── 02_diagnosis.R
├── 03_cleaning.R
├── 04_report.Rmd
├── run_pipeline.R
├── README.md
├── readme.rmd
├── Raw_data/
│   └── shein_sample.csv
└── Output/
    ├── shein_exploratory_diagnostics.xlsx
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

## Author notes

This README reflects the current project scripts and outputs already drafted in the repository and can be updated again once the cleaning and reporting stages are added. It is designed to work as both assignment documentation and a setup guide for anyone cloning the repository.
