# =============================================================================
# 06_report.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Render the automated Word report from 06_report.rmd.
#           Sources all upstream pipeline scripts to ensure the required
#           objects exist, then calls rmarkdown::render().
#
# Output:   Output/shein_data_quality_report.docx
# Depends:  00_config.R → 01_ingest.R → 02_diagnosis.R → 03_clean.R →
#           04_impute.R → data_dictionary.R
#           06_report.rmd (the R Markdown template)
# =============================================================================

# ── Source the full pipeline ────────────────────────────────────────────────
# Each script guards itself with if (!exists(...)) checks, so sourcing
# 04_impute.r cascades through the chain automatically.

if (!exists("df_imputed")) source("04_impute.r")
if (!exists("dict_df"))    source("data_dictionary.r")

suppressPackageStartupMessages({
        library(rmarkdown)
        library(data.table)
        library(ggplot2)
        library(knitr)
        library(scales)
        library(stringr)
        library(stringi)
})

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[report] Starting 06_report.r ...")

# ── Verify required objects ─────────────────────────────────────────────────

required_objects <- c("raw_file", "df_clean", "df_imputed",
                      "clean_drop_log", "impute_log", "dict_df",
                      "RAW_FILE_PATH", "OUTPUT_DIR")

missing_obj <- required_objects[!sapply(required_objects, exists)]

if (length(missing_obj) > 0) {
        stop("[report] Missing required pipeline objects: ",
             paste(missing_obj, collapse = ", "),
             "\n  Please run the full pipeline before rendering the report.")
}

# ── Render ──────────────────────────────────────────────────────────────────

REPORT_RMD  <- "06_report.rmd"
REPORT_DOCX <- file.path(OUTPUT_DIR, "shein_data_quality_report.docx")

message("[report] Rendering ", REPORT_RMD, " → ", REPORT_DOCX, " ...")

rmarkdown::render(
        input       = REPORT_RMD,
        output_file = basename(REPORT_DOCX),
        output_dir  = OUTPUT_DIR,
        envir       = globalenv(),    # use the current environment with all pipeline objects
        quiet       = FALSE
)

message("[report] Report saved → ", REPORT_DOCX)
message("[report] 06_report.r complete.")
