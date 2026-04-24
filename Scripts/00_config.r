# =============================================================================
# 00_config.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Central configuration — paths, constants, and startup validation
# =============================================================================

# -----------------------------------------------------------------------------
# 1. File paths
# -----------------------------------------------------------------------------

RAW_FILE_PATH    <- "Raw_data/shein_sample.csv"
OUTPUT_DIR       <- "Output"
DIAGNOSTICS_FILE <- file.path(OUTPUT_DIR, "shein_exploratory_diagnostics.xlsx")
REPORT_FILE      <- file.path(OUTPUT_DIR, "shein_data_quality_report.html")

# -----------------------------------------------------------------------------
# 2. Runtime constants
# -----------------------------------------------------------------------------

# Maximum rows written to Excel example sheets (keeps workbook manageable)
EXCEL_SAMPLE_ROWS <- 50

# IQR multiplier used for outlier flagging
IQR_MULTIPLIER <- 1.5

# Character-column threshold for "very long text" flag (characters)
LONG_TEXT_THRESHOLD <- 200

# Strings that should be treated as missing values when parsing numbers
MESSY_MISSING_VALUES <- c(
        "", "NA", "N/A", "na", "n/a",
        "null", "NULL", "undefined", "Undefined", "UNDEFINED"
)

# Date-time format orders passed to lubridate::parse_date_time()
DATE_ORDERS <- c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy"
)

# -----------------------------------------------------------------------------
# 3. Startup validation
# -----------------------------------------------------------------------------

if (!file.exists(RAW_FILE_PATH)) {
        stop(
                "Raw data file not found.\n",
                "Expected location: ", RAW_FILE_PATH, "\n",
                "Please place shein_sample.csv inside the Raw_data/ folder and re-run."
        )
}

message("[config] Configuration loaded successfully.")
