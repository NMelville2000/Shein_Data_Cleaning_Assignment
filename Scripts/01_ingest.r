# =============================================================================
# 01_ingest.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Load the raw CSV, run basic structural validation, and expose
#           `raw_file` (data.frame) to downstream scripts.
# Depends:  00_config.R  (must be sourced first)
# =============================================================================

#source("00_config.R")

# -----------------------------------------------------------------------------
# 1. Load required libraries
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
        library(data.table)   # fread — fast CSV ingestion
        library(dplyr)        # data manipulation
        library(readr)        # parse_number, type helpers
        library(stringr)      # string utilities
        library(stringi)      # encoding-safe string ops
        library(tidyr)        # separate_rows, pivot helpers
        library(lubridate)    # date parsing
        library(purrr)        # map / map2 functional helpers
        library(scales)       # formatting for output
        library(openxlsx)     # Excel workbook creation
        
})

# -----------------------------------------------------------------------------
# 2. Read raw data
# -----------------------------------------------------------------------------

message("[ingest] Reading raw file: ", RAW_FILE_PATH)

raw_file <- fread(
        RAW_FILE_PATH,
        encoding = "UTF-8",
        fill     = TRUE,
        quote    = "\""
)

raw_file <- as.data.frame(raw_file)

# -----------------------------------------------------------------------------
# 3. Structural validation
# -----------------------------------------------------------------------------

stopifnot(
        "Ingested object is not a data.frame" = is.data.frame(raw_file),
        "Ingested data.frame has no rows"     = nrow(raw_file) > 0,
        "Ingested data.frame has no columns"  = ncol(raw_file) > 0
)

message("[ingest] Raw data loaded successfully.")
message("         Rows    : ", formatC(nrow(raw_file), big.mark = ","))
message("         Columns : ", ncol(raw_file))
message("         Column names:")
message(paste0("           ", names(raw_file), collapse = "\n"))

# -----------------------------------------------------------------------------
# 4. Quick structural summary (written to console for immediate review)
# -----------------------------------------------------------------------------

cat("\n--- head() ---\n");  print(head(raw_file, 3))
cat("\n--- str() ---\n");   str(raw_file)
cat("\n--- summary() ---\n"); print(summary(raw_file))


