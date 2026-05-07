# =============================================================================
# data_dictionary.r
# Project:  Shein Data Quality Pipeline
# Purpose:  Build a one-row-per-variable data dictionary from the pipeline
#           objects (raw_file, df_clean, df_imputed).
#           Columns: name, type, label, valid_range_or_values, pct_missing_raw,
#                    pct_missing_after, imputation_treatment, notes.
# Output:   • "Data Dictionary" sheet appended to the diagnostics workbook
#           • Output/data_dictionary.csv
# Depends:  00_config.R → 01_ingest.R → 02_diagnosis.R → 03_clean.R → 04_impute.R
# =============================================================================

if (!exists("df_imputed")) source("04_impute.r")

suppressPackageStartupMessages({
        library(data.table)
        library(openxlsx)
        library(stringi)
})

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[dict] Starting data_dictionary.r ...")

# =============================================================================
# SECTION A — Helper: compute % missing (NA or "")
# =============================================================================

pct_miss <- function(x) {
        round(sum(is.na(x) | as.character(x) == "") / length(x) * 100, 1)
}

# =============================================================================
# SECTION B — Raw-data missingness (from raw_file)
# =============================================================================
# Map each final column back to its raw source to get the "before" rate.

raw_dt  <- as.data.table(raw_file)
imp_dt  <- as.data.table(df_imputed)

# Raw source mapping:  final_col → raw_col
# Columns derived from 'description' use description's raw missingness.
# Columns derived from 'size' use size's raw missingness.
# price_flag has no raw equivalent (it was created during cleaning).

raw_source <- c(
        sku            = "sku",
        url            = "url",
        name           = "name",
        price          = "price",
        price_flag     = NA_character_,
        brand          = "brand",
        color          = "description",
        size_system    = "size",
        size_count     = "size",
        size_labels    = "size",
        size_us        = "size",
        dimensions_raw = "size",
        images         = "images"
)

# =============================================================================
# SECTION C — Build the dictionary table
# =============================================================================

message("[dict] Building dictionary rows ...")

# ── One row per variable in df_imputed, in column order ─────────────────────

final_cols <- names(imp_dt)

dict_rows <- lapply(final_cols, function(col) {
        
        vals <- imp_dt[[col]]
        
        # ── type ────────────────────────────────────────────────────────────
        r_type <- paste(class(vals), collapse = ", ")
        
        # ── % missing raw ──────────────────────────────────────────────────
        raw_col <- raw_source[col]
        if (!is.na(raw_col) && raw_col %in% names(raw_dt)) {
                miss_raw <- pct_miss(raw_dt[[raw_col]])
        } else {
                miss_raw <- NA_real_   # column didn't exist in raw data
        }
        
        # ── % missing after cleaning + imputation ──────────────────────────
        miss_after <- pct_miss(vals)
        
        # ── valid range / value set ─────────────────────────────────────────
        # Numeric: show min–max.  Character with ≤ 20 unique: list values.
        # Otherwise: describe briefly.
        if (is.numeric(vals)) {
                non_na <- vals[!is.na(vals)]
                if (length(non_na) > 0) {
                        valid_range <- paste0(round(min(non_na), 2), " – ",
                                              round(max(non_na), 2))
                } else {
                        valid_range <- "(all NA)"
                }
        } else if (is.integer(vals)) {
                non_na <- vals[!is.na(vals)]
                if (length(non_na) > 0) {
                        valid_range <- paste0(min(non_na), " – ", max(non_na))
                } else {
                        valid_range <- "(all NA)"
                }
        } else {
                unique_vals <- unique(vals[!is.na(vals) & vals != ""])
                if (length(unique_vals) <= 20) {
                        valid_range <- paste(sort(unique_vals), collapse = " | ")
                } else {
                        valid_range <- paste0(length(unique_vals), " unique values (free text)")
                }
        }
        
        data.frame(
                name        = col,
                type        = r_type,
                valid_range = valid_range,
                miss_raw    = miss_raw,
                miss_after  = miss_after,
                stringsAsFactors = FALSE
        )
})

dict_df <- rbindlist(dict_rows)

# ── Add label, imputation_treatment, notes by hand (pipeline-specific) ──────

labels <- c(
        sku            = "Unique product identifier (primary key)",
        url            = "Product page URL",
        name           = "Product display name",
        price          = "Product price in USD",
        price_flag     = "Price quality flag",
        brand          = "Brand or seller name",
        color          = "Product colour",
        size_system    = "Size system classification",
        size_count     = "Number of size options offered",
        size_labels    = "Letter/label size codes",
        size_us        = "US numeric size equivalents",
        dimensions_raw = "Raw dimension string (bedding/home)",
        images         = "Image URL list (raw)"
)

imputation <- c(
        sku            = "None required",
        url            = "None — left as NA",
        name           = "None — left as NA",
        price          = "Median imputation grouped by size_system (all missing were one_size)",
        price_flag     = "Not applicable — derived flag column",
        brand          = "Left as NA — high-cardinality categorical; missingness is informative (most SHEIN products are unbranded)",
        color          = "Left as NA — structurally absent where description lacked a Color key",
        size_system    = "Defaults to 'unknown' if no pattern matched; not imputed",
        size_count     = "NA for non-countable systems (dimensions, volume_length, unknown); not imputed",
        size_labels    = "Structural NA for non-clothing systems; not imputed",
        size_us        = "Structural NA for non-clothing and shoe_us systems; not imputed",
        dimensions_raw = "Structural NA for all non-dimensions products; not imputed",
        images         = "None — carried forward unchanged"
)

notes <- c(
        sku            = "Prefix 'SKU: ' stripped; rows deduplicated by this column",
        url            = "Carried forward unchanged",
        name           = "Carried forward unchanged",
        price          = "Currency symbol stripped; coerced to numeric. Imputed rows flagged in price_flag",
        price_flag     = "Values: ok | missing | invalid | outlier_high | imputed. Created in 03_clean, updated in 04_impute",
        brand          = "Mojibake fixed with stri_trans_general(); category breadcrumb after corrupted char removed",
        color          = "Extracted from description field via regex; description column then dropped",
        size_system    = "Classified by priority-ordered regex in fcase(). 15 possible codes — see size system reference",
        size_count     = "one_size hardcoded to 1; others counted from comma-separated tokens",
        size_labels    = "Extracted via system-specific regex; comma-separated",
        size_us        = "Extracted from parenthesised values; comma-separated",
        dimensions_raw = "Verbatim copy of raw size for bedding products only",
        images         = "Cleaning deferred to later pipeline stage; contains Python-style list syntax"
)

dict_df[, label                := labels[name]]
dict_df[, imputation_treatment := imputation[name]]
dict_df[, notes                := notes[name]]

# ── Rename for final output ─────────────────────────────────────────────────

setnames(dict_df, c("miss_raw", "miss_after", "valid_range"),
         c("pct_missing_raw", "pct_missing_after", "valid_range_or_values"))

# ── Reorder columns to match the specification ──────────────────────────────

dict_df <- dict_df[, .(name, type, label, valid_range_or_values,
                       pct_missing_raw, pct_missing_after,
                       imputation_treatment, notes)]

# =============================================================================
# SECTION D — Display
# =============================================================================

cat("\n=== [05_data_dictionary] Data Dictionary ===\n")
print(dict_df, nrows = 20)

# =============================================================================
# SECTION E — Export as CSV
# =============================================================================

DICT_CSV <- file.path(OUTPUT_DIR, "data_dictionary.csv")
fwrite(dict_df, DICT_CSV)
message("[dict] CSV saved → ", DICT_CSV)

# =============================================================================
# SECTION F — Append as sheet to diagnostics workbook
# =============================================================================

message("[dict] Adding sheet to diagnostics workbook ...")

clean_for_excel <- function(x) {
        if (is.character(x)) {
                x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
                x <- stri_replace_all_regex(x, "\\p{C}", "")
        }
        x
}

if (file.exists(DIAGNOSTICS_FILE)) {
        wb <- loadWorkbook(DIAGNOSTICS_FILE)
} else {
        wb <- createWorkbook()
}

sheet_name <- "Data Dictionary"

# Remove existing sheet if re-running
if (sheet_name %in% names(wb)) {
        removeWorksheet(wb, sheet_name)
}

addWorksheet(wb, sheet_name)

dict_excel <- as.data.frame(lapply(dict_df, clean_for_excel),
                            stringsAsFactors = FALSE)

# ── Header style ────────────────────────────────────────────────────────────
header_style <- createStyle(
        fontName   = "Arial",
        fontSize   = 10,
        fontColour = "#FFFFFF",
        fgFill     = "#3D405B",
        halign     = "center",
        valign     = "center",
        textDecoration = "bold",
        wrapText   = TRUE
)

# ── Body style ──────────────────────────────────────────────────────────────
body_style <- createStyle(
        fontName = "Arial",
        fontSize = 9.5,
        valign   = "top",
        wrapText = TRUE,
        border   = "Bottom",
        borderColour = "#DDDDDD",
        borderStyle  = "thin"
)

# ── Percentage style for the two % columns ─────────────────────────────────
pct_style <- createStyle(
        fontName   = "Arial",
        fontSize   = 9.5,
        valign     = "top",
        halign     = "right",
        numFmt     = "0.0",
        border     = "Bottom",
        borderColour = "#DDDDDD",
        borderStyle  = "thin"
)

writeData(wb, sheet_name, dict_excel)

addStyle(wb, sheet_name, header_style,
         rows = 1, cols = seq_len(ncol(dict_excel)), gridExpand = TRUE)

addStyle(wb, sheet_name, body_style,
         rows = 2:(nrow(dict_excel) + 1),
         cols = seq_len(ncol(dict_excel)),
         gridExpand = TRUE)

# Apply percentage formatting to pct columns (cols 5 & 6)
addStyle(wb, sheet_name, pct_style,
         rows = 2:(nrow(dict_excel) + 1),
         cols = 5:6,
         gridExpand = TRUE)

# ── Column widths ───────────────────────────────────────────────────────────
setColWidths(wb, sheet_name, cols = 1, widths = 16)   # name
setColWidths(wb, sheet_name, cols = 2, widths = 14)   # type
setColWidths(wb, sheet_name, cols = 3, widths = 32)   # label
setColWidths(wb, sheet_name, cols = 4, widths = 40)   # valid_range_or_values
setColWidths(wb, sheet_name, cols = 5, widths = 14)   # pct_missing_raw
setColWidths(wb, sheet_name, cols = 6, widths = 14)   # pct_missing_after
setColWidths(wb, sheet_name, cols = 7, widths = 45)   # imputation_treatment
setColWidths(wb, sheet_name, cols = 8, widths = 50)   # notes

# ── Freeze top row ──────────────────────────────────────────────────────────
freezePane(wb, sheet_name, firstRow = TRUE)

saveWorkbook(wb, DIAGNOSTICS_FILE, overwrite = TRUE)

message("[dict] Sheet '", sheet_name, "' added → ", DIAGNOSTICS_FILE)
message("[dict] data_dictionary.r complete.")

