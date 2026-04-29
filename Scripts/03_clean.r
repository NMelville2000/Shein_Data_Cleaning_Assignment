#03_clean.r

# =============================================================================
# 03_clean.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Structural cleaning of raw_file produced by 01_ingest.R.
#           Covers: SKU · Price · Brand · Color · Size
#           Images deferred to a later cleaning stage.
# Output:   df_clean        — cleaned wide data.frame (one row per product)
#           clean_drop_log  — data.frame recording every column dropped + reason
#           Output/shein_cleaned.csv
# Depends:  00_config.R + 01_ingest.R (raw_file must exist in environment)
#           02_diagnosis.R is optional but recommended first — its `detected`
#           list is reused here if present.
# =============================================================================

if (!exists("raw_file"))  source("01_ingest.r")
if (!exists("detected"))  source("02_diagnosis.r")

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[clean] Starting 03_clean.r ...")

# =============================================================================
# SECTION A — Working copy
# =============================================================================

dc <- as.data.table(raw_file)
setDT(dc)   # assert data.table class immediately,before any := operations

# =============================================================================
# SECTION B — Drop-log initialisation
# =============================================================================
# Every column that is removed or replaced must be recorded here.
# Appended throughout the script via rbind().

clean_drop_log <- data.frame(
        column  = character(),
        action  = character(),
        reason  = character(),
        stringsAsFactors = FALSE
)

log_drop <- function(column, action, reason) {
        clean_drop_log <<- rbind(
                clean_drop_log,
                data.frame(column = column, action = action,
                           reason = reason, stringsAsFactors = FALSE)
        )
}

# =============================================================================
# SECTION C — SKU
# =============================================================================

message("[clean] C — SKU: stripping prefix and deduplicating ...")

if ("sku" %in% names(dc)) {
        dc[, sku := str_trim(str_remove(sku, "^SKU:\\s*"))]
} else {
        warning("[clean] No 'sku' column found — skipping SKU cleaning.")
}

n_before <- nrow(dc)
dc <- unique(dc, by = "sku")
setDT(dc)
n_after  <- nrow(dc)

message("[clean]   Rows before dedup: ", formatC(n_before, big.mark = ","))
message("[clean]   Rows after  dedup: ", formatC(n_after,  big.mark = ","))
message("[clean]   Duplicate rows removed: ", formatC(n_before - n_after, big.mark = ","))
# =============================================================================
# SECTION D — Price
# =============================================================================
# Raw value:  "$7.25"  or  "7.25"
# Goal:       numeric column price_clean; original replaced
# Flag:       price_flag — missing | invalid | outlier_high | ok

message("[clean] D — Price: parsing and flagging ...")

price_col <- detected$price   # column name detected in 02_diagnosis

if (!is.na(price_col) && price_col %in% names(dc)) {
        
        dc[, price_clean := as.numeric(str_replace_all(
                get(price_col), "[^0-9.]", ""
        ))]
        
        dc[, price_flag := fcase(
                is.na(price_clean),      "missing",
                price_clean <= 0,        "invalid",
                price_clean > 10000,     "outlier_high",
                default =                "ok"
        )]
        
        n_price_issues <- dc[price_flag != "ok", .N]
        message("[clean]   Price issues flagged: ", n_price_issues,
                " (see price_flag column)")
        
        # Replace original price column with cleaned numeric; log the action
        dc[, (price_col) := price_clean]
        setnames(dc, price_col, "price")
        dc[, price_clean := NULL]
        
        log_drop(
                column = price_col,
                action = "replaced",
                reason = "Stripped currency symbol; coerced to numeric as 'price'; price_flag added for missing/invalid/outlier rows"
        )
        
} else {
        warning("[clean] Price column not found — skipping price cleaning.")
}

# =============================================================================
# SECTION E — Brand
# =============================================================================
# Raw value:  "QingSang\x3fAccessories Apparel Accessories"  (mojibake +
#             category breadcrumb appended by scraper after a corrupted char)
# Goal:       clean brand name only; empty → NA

message("[clean] E — Brand: fixing mojibake and extracting brand name ...")

if ("brand" %in% names(dc)) {
        
        dc[, brand := stri_trans_general(brand, "latin-ascii")]
        dc[, brand := str_trim(str_extract(brand, "^[^?�]+"))]
        dc[brand == "" | is.na(brand), brand := NA_character_]
        
        n_brand_na <- dc[is.na(brand), .N]
        message("[clean]   Brands set to NA (empty or unrecoverable): ", n_brand_na)
        
} else {
        warning("[clean] No 'brand' column found — skipping brand cleaning.")
}

# =============================================================================
# SECTION F — Color  (extracted from description)
# =============================================================================
# Raw value:  description = "[{'Color': 'Black'}, {'Material': 'Woven Fabric'}]"
# Goal:       standalone color column extracted from description key-value string;
#             description column dropped entirely (only Color retained for imputation)

message("[clean] F — Color: extracting from description field ...")

desc_col <- "description"   # confirmed present in raw_file from 02_diagnosis

if (desc_col %in% names(dc)) {
        
        dc[, color := str_extract(
                get(desc_col),
                "(?<='Color':\\s')[^']+"
        )]
        
        n_color_found   <- dc[!is.na(color), .N]
        n_color_missing <- dc[is.na(color),  .N]
        message("[clean]   Color extracted — present: ", n_color_found,
                " | missing (NA): ", n_color_missing)
        
        # Drop description — all other attributes discarded, only Color retained
        dc[, (desc_col) := NULL]
        
        log_drop(
                column = desc_col,
                action = "dropped",
                reason = paste0(
                        "Serialised key-value string. Only 'Color' value retained ",
                        "as standalone column for missing data imputation. ",
                        "All other attributes (Material, Style, etc.) dropped — ",
                        "below coverage threshold for imputation use."
                )
        )
        
} else {
        warning("[clean] No 'description' column found — skipping color extraction.")
}

# =============================================================================
# SECTION G — Size
# =============================================================================
# Raw value:  "XS(2),S(4),M(6),L(8/10)"  /  "CN35(US5.5),CN36(US6)"  /
#             "135*200,160*220"  /  "one-size"  / etc.
# Goal:       classify size system; extract system-specific columns;
#             every system gets its own column, others left NA so the
#             full dataset remains one file suitable for imputation.

message("[clean] G — Size: classifying systems and extracting columns ...")

if ("size" %in% names(dc)) {
        
        # ── DETECT SIZE SYSTEM ──────────────────────────────────────────────
        dc[, size_system := fcase(
                size == "one-size",                                              "one_size",
                str_detect(size, "\\d+\\*\\d+"),                                "dimensions",
                str_detect(size, "\\d{2}[A-G]\\(\\d{2}[A-G]"),                 "bra",
                str_detect(size, "\\dY\\(|\\dY,|\\d{2}Y\\(|\\d{2}Y,|IN\\)"),  "kids",
                str_detect(size, "W\\d+ L\\d+"),                                "jeans",
                str_detect(size, "CN\\d+|EUR\\d+"),                             "shoe_intl",
                str_detect(size, "US\\d+\\.?\\d*") &
                        !str_detect(size, "CN|EUR"),                            "shoe_us",
                str_detect(size, "Petite"),                                     "petite",
                str_detect(size, "Tall "),                                      "tall",
                str_detect(size, "0XL|1XL|2XL|3XL|4XL|5XL|6XL"),              "plus",
                str_detect(size, "[XSML]\\(\\d{2}\\)"),                        "eu_clothing",
                str_detect(size, "^\\d"),                                       "us_letter",
                str_detect(size, "(?i)ml|inch| m,| m$"),                       "volume_length",
                str_detect(size, "\\bXXS\\b|\\bXS\\b|\\bS\\b|\\bM\\b|\\bL\\b|\\bXL\\b|\\bXXL\\b"),
                "standard",
                default =                                                        "unknown"
        )]
        
        # ── SIZE COLUMNS ────────────────────────────────────────────────────
        
        dc[, dimensions_raw := fifelse(size_system == "dimensions", size, NA_character_)]
        
        NON_CLOTHING <- c("one_size", "dimensions", "bra", "kids", "jeans",
                          "volume_length", "unknown")
        
        extract_labels <- function(size_str, system) {
                if (system %in% NON_CLOTHING) return(NA_character_)
                if (system == "shoe_us") return(size_str)
                if (system == "us_letter") {
                        labels <- str_extract_all(size_str, "(?<=\\()[A-Za-z/ ]+(?=\\))")[[1]]
                } else {
                        labels <- str_extract_all(
                                size_str,
                                "[A-Za-z][A-Za-z ]*(?=\\s*\\(|,|$)"
                        )[[1]]
                }
                if (length(labels) == 0) return(NA_character_)
                paste(str_trim(labels), collapse = ", ")
        }
        
        extract_us_sizes <- function(size_str, system) {
                if (system %in% c(NON_CLOTHING, "shoe_us")) return(NA_character_)
                if (system == "shoe_intl") {
                        us <- str_extract_all(size_str, "(?<=US)[0-9.]+")[[1]]
                        return(if (length(us) == 0) NA_character_ else paste(us, collapse = ", "))
                }
                nums <- str_extract_all(size_str, "(?<=\\()[0-9/]+(?=\\))")[[1]]
                if (length(nums) == 0) return(NA_character_)
                paste(nums, collapse = ", ")
        }
        
        dc[, size_labels := mapply(extract_labels,  size, size_system)]
        dc[, size_us     := mapply(extract_us_sizes, size, size_system)]
        
        dc[, size_count := fcase(
                size_system == "one_size",                              1L,
                size_system %in% c("dimensions", "unknown",
                                   "volume_length"),                    NA_integer_,
                default = as.integer(lengths(str_split(size, ",")))
        )]
        
        dc[, size := NULL]
        
        log_drop(
                column = "size",
                action = "replaced",
                reason = paste0(
                        "Multi-value string spanning multiple size systems. ",
                        "Replaced by: size_system (classification), size_labels ",
                        "(letter labels), size_us (US numeric), dimensions_raw ",
                        "(bedding), size_count (option count). ",
                        "Columns for non-applicable systems left NA intentionally ",
                        "to preserve one-file structure for imputation."
                )
        )
        
} else {
        warning("[clean] No 'size' column found — skipping size cleaning.")
}

# =============================================================================
# SECTION H — Assemble final cleaned data.frame
# =============================================================================

message("[clean] H — Assembling final cleaned dataset ...")

# Identify any remaining columns not yet handled (images etc.) — carry forward
handled <- c("sku", "url", "name", "price", "price_flag",
             "brand", "color",
             "size_system", "size_labels", "size_us",
             "dimensions_raw", "size_count",
             "description", "size", "images")

remaining <- setdiff(names(dc), handled)

priority_cols <- intersect(
        c("sku", "url", "name", "price", "price_flag",
          "brand", "color",
          "size_system", "size_count", "size_labels", "images", "size_us",
          "dimensions_raw"),
        names(dc)
)

df_clean <- as.data.frame(dc[, c(priority_cols, remaining), with = FALSE])

# =============================================================================
# SECTION I — Summary & drop log
# =============================================================================

cat("\n=== [03_clean] Final Cleaned Dataset ===\n")
cat("Rows:   ", formatC(nrow(df_clean), big.mark = ","), "\n")
cat("Columns:", ncol(df_clean), "\n\n")

cat("=== Column Names ===\n")
print(names(df_clean))

cat("\n=== Missing Value Counts ===\n")
missing_summary <- sort(
        sapply(df_clean, function(x) sum(is.na(x) | x == "")),
        decreasing = TRUE
)
print(missing_summary)

cat("\n=== Size System Distribution ===\n")
if ("size_system" %in% names(df_clean)) {
        print(as.data.frame(table(df_clean$size_system, dnn = "size_system")))
}

cat("\n=== Price Flag Distribution ===\n")
if ("price_flag" %in% names(df_clean)) {
        print(as.data.frame(table(df_clean$price_flag, dnn = "price_flag")))
}

cat("\n=== Drop Log ===\n")
print(clean_drop_log)

# =============================================================================
# SECTION J — Export
# =============================================================================

CLEAN_FILE <- file.path(OUTPUT_DIR, "shein_cleaned.csv")

write.csv(df_clean, CLEAN_FILE, row.names = FALSE, na = "")

message("[clean] Cleaned data saved → ", CLEAN_FILE)

