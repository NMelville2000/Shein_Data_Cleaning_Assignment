# =============================================================================
# 05_export.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Export the cleaned + imputed dataset in all required formats:
#             • .sav   (SPSS)   — haven::write_sav()  with variable & value labels
#             • .dta   (Stata)  — haven::write_dta()   with variable & value labels
#             • .xlsx  (Excel)  — openxlsx             data + data dictionary sheet
#             • .rds   (R)      — saveRDS()            preserves haven labelled vectors
#           Python users read .sav and .dta via pyreadstat — no separate export needed.
# Output:   Output/shein_final.sav
#           Output/shein_final.dta
#           Output/shein_final.xlsx
#           Output/shein_final.rds
# Depends:  00_config.R → … → 04_impute.R  (df_imputed must exist)
#           data_dictionary.R               (dict_df must exist)
# =============================================================================

if (!exists("df_imputed")) source("04_impute.r")
if (!exists("dict_df"))    source("data_dictionary.r")

suppressPackageStartupMessages({
        library(data.table)
        library(haven)
        library(openxlsx)
        library(stringi)
})

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[export] Starting 05_export.r ...")

# =============================================================================
# SECTION A — Prepare labelled copy
# =============================================================================
# haven's write_sav() and write_dta() support:
#   • variable labels  — short description attached to each column
#   • value labels     — named factor-like mappings for categorical columns
# We apply both here so the .sav and .dta files are self-documenting.

message("[export] A — Applying variable and value labels ...")

df_out <- as.data.frame(df_imputed, stringsAsFactors = FALSE)

# ── Variable labels ─────────────────────────────────────────────────────────

var_labels <- c(
        sku            = "Unique product identifier (primary key)",
        url            = "Product page URL",
        name           = "Product display name",
        price          = "Product price in USD",
        price_flag     = "Price quality flag (ok / missing / invalid / outlier_high / imputed)",
        brand          = "Brand or seller name",
        color          = "Product colour",
        size_system    = "Size system classification code",
        size_count     = "Number of size options offered",
        size_labels    = "Letter/label size codes (comma-separated)",
        size_us        = "US numeric size equivalents (comma-separated)",
        dimensions_raw = "Raw dimension string for bedding/home products",
        images         = "Image URL list (raw, uncleaned)"
)

for (col in names(var_labels)) {
        if (col %in% names(df_out)) {
                attr(df_out[[col]], "label") <- var_labels[[col]]
        }
}

# ── Value labels (categorical columns with closed value sets) ───────────────

# price_flag — labelled integer for SPSS/Stata compatibility
if ("price_flag" %in% names(df_out)) {
        pf_levels <- c(
                "ok"           = 1L,
                "missing"      = 2L,
                "invalid"      = 3L,
                "outlier_high" = 4L,
                "imputed"      = 5L
        )
        
        pf_labels <- c(
                `1` = "ok",
                `2` = "missing",
                `3` = "invalid",
                `4` = "outlier_high",
                `5` = "imputed"
        )
        
        # Map character → integer with value labels
        df_out$price_flag_num <- pf_levels[df_out$price_flag]
        df_out$price_flag_num <- haven::labelled(
                df_out$price_flag_num,
                labels = pf_levels,
                label  = "Price quality flag"
        )
}

# size_system — labelled integer for SPSS/Stata
if ("size_system" %in% names(df_out)) {
        ss_unique <- sort(unique(df_out$size_system[!is.na(df_out$size_system)]))
        ss_levels <- setNames(seq_along(ss_unique), ss_unique)
        
        df_out$size_system_num <- ss_levels[df_out$size_system]
        df_out$size_system_num <- haven::labelled(
                df_out$size_system_num,
                labels = ss_levels,
                label  = "Size system classification code"
        )
}

# =============================================================================
# SECTION B — Export .sav (SPSS)
# =============================================================================

message("[export] B — Writing .sav (SPSS) ...")

# For .sav: use labelled integer versions of categorical columns;
# keep character versions too so nothing is lost.

sav_path <- file.path(OUTPUT_DIR, "shein_final.sav")

# Stata/SPSS variable names must be ≤ 32 chars — ours are fine.
# Truncate any string columns > 32767 chars (SPSS limit).
df_sav <- df_out
for (col in names(df_sav)) {
        if (is.character(df_sav[[col]])) {
                df_sav[[col]] <- stri_enc_toutf8(df_sav[[col]], is_unknown_8bit = TRUE)
                df_sav[[col]] <- strtrim(df_sav[[col]], 32767)
        }
}

write_sav(df_sav, sav_path)
message("[export]   Saved → ", sav_path)

# =============================================================================
# SECTION C — Export .dta (Stata)
# =============================================================================

message("[export] C — Writing .dta (Stata v15+) ...")

dta_path <- file.path(OUTPUT_DIR, "shein_final.dta")

# Stata variable names: ≤ 32 chars, no spaces, no special chars.
# Our names already satisfy this.
# Stata v15+ supports Unicode — use version = 15.

df_dta <- df_out

# Stata string limit is 2045 for str# types; longer goes to strL.
# haven handles this automatically with version ≥ 13.

write_dta(df_dta, dta_path, version = 15)
message("[export]   Saved → ", dta_path)

# =============================================================================
# SECTION D — Export .xlsx (Excel) — data + data dictionary sheet
# =============================================================================

message("[export] D — Writing .xlsx (data + data dictionary) ...")

xlsx_path <- file.path(OUTPUT_DIR, "shein_final.xlsx")

# ── Excel-safe text cleaner ─────────────────────────────────────────────────

clean_for_excel <- function(x) {
        if (is.character(x)) {
                x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
                x <- stri_replace_all_regex(x, "\\p{C}", "")
        }
        x
}

wb <- createWorkbook()

# ── Sheet 1: Data ──────────────────────────────────────────────────────────

addWorksheet(wb, "Data")

# Export a plain data.frame (drop haven labelled columns for Excel)
df_xlsx <- df_out[, !(names(df_out) %in% c("price_flag_num", "size_system_num"))]
df_xlsx <- as.data.frame(lapply(df_xlsx, function(x) {
        x <- clean_for_excel(x)
        if (inherits(x, "haven_labelled")) as.character(x) else x
}), stringsAsFactors = FALSE)

writeData(wb, "Data", df_xlsx)

# Header style
data_header <- createStyle(
        fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
        fgFill = "#3D405B", halign = "center", textDecoration = "bold",
        wrapText = TRUE
)
addStyle(wb, "Data", data_header,
         rows = 1, cols = seq_len(ncol(df_xlsx)), gridExpand = TRUE)

setColWidths(wb, "Data", cols = seq_len(ncol(df_xlsx)), widths = "auto")
freezePane(wb, "Data", firstRow = TRUE)

# ── Sheet 2: Data Dictionary ──────────────────────────────────────────────

addWorksheet(wb, "Data Dictionary")

dict_excel <- as.data.frame(lapply(dict_df, clean_for_excel),
                            stringsAsFactors = FALSE)

writeData(wb, "Data Dictionary", dict_excel)

dict_header <- createStyle(
        fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
        fgFill = "#3D405B", halign = "center", textDecoration = "bold",
        wrapText = TRUE
)
dict_body <- createStyle(
        fontName = "Arial", fontSize = 9.5, valign = "top",
        wrapText = TRUE, border = "Bottom",
        borderColour = "#DDDDDD", borderStyle = "thin"
)

addStyle(wb, "Data Dictionary", dict_header,
         rows = 1, cols = seq_len(ncol(dict_excel)), gridExpand = TRUE)
addStyle(wb, "Data Dictionary", dict_body,
         rows = 2:(nrow(dict_excel) + 1),
         cols = seq_len(ncol(dict_excel)), gridExpand = TRUE)

setColWidths(wb, "Data Dictionary", cols = 1, widths = 16)
setColWidths(wb, "Data Dictionary", cols = 2, widths = 14)
setColWidths(wb, "Data Dictionary", cols = 3, widths = 32)
setColWidths(wb, "Data Dictionary", cols = 4, widths = 40)
setColWidths(wb, "Data Dictionary", cols = 5, widths = 14)
setColWidths(wb, "Data Dictionary", cols = 6, widths = 14)
setColWidths(wb, "Data Dictionary", cols = 7, widths = 45)
setColWidths(wb, "Data Dictionary", cols = 8, widths = 50)
freezePane(wb, "Data Dictionary", firstRow = TRUE)

saveWorkbook(wb, xlsx_path, overwrite = TRUE)
message("[export]   Saved → ", xlsx_path)

# =============================================================================
# SECTION E — Export .rds (Native R)
# =============================================================================

message("[export] E — Writing .rds (native R) ...")

rds_path <- file.path(OUTPUT_DIR, "shein_final.rds")

# Save the full labelled data.frame — preserves haven_labelled vectors,
# attributes, and all metadata.
saveRDS(df_out, rds_path)
message("[export]   Saved → ", rds_path)

# =============================================================================
# SECTION F — Summary
# =============================================================================

cat("\n=== [05_export] Export Summary ===\n")

export_files <- c(sav_path, dta_path, xlsx_path, rds_path)
for (f in export_files) {
        size_kb <- round(file.info(f)$size / 1024, 1)
        cat(sprintf("  %-40s  %7.1f KB\n", f, size_kb))
}

cat("\nNote: Python users can read .sav and .dta via pyreadstat.\n")
cat("      No separate Python export is required.\n\n")

message("[export] 05_export.r complete.")

