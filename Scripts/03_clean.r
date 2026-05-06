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

stopifnot(exists("raw_file"), exists("detected"))

message("[clean] Starting 03_clean.r ...")

# =============================================================================
# SECTION A — Working copy
# =============================================================================

dc <- copy(raw_file)   # raw_file is already a data.table; copy() avoids reference aliasing

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
                dc[[price_col]], "[^0-9.]", ""
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
        if (price_col != "price") setnames(dc, price_col, "price")
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
        
        # str_match with a capturing group handles zero or more spaces after
        # the colon — the previous lookbehind (?<='Color':\\s') required
        # exactly one space and silently missed no-space variants.
        dc[, color := str_match(dc[[desc_col]], "'Color':\\s*'([^']+)")[, 2]]
        
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
        
        # ── STEP 1: NORMALISE SPACING ───────────────────────────────────────
        # "XS (2),S (4)" and "XS(2),S(4)" are the same product but produce
        # different classification results without this step.
        dc[, size := str_squish(size)]                        # collapse internal spaces
        dc[, size := str_replace_all(size, "\\s*\\(", "(")]   # "XS (2)" → "XS(2)"
        dc[, size := str_replace_all(size, "\\s*,\\s*", ",")] # normalise comma spacing
        
        # ── STEP 2: MODIFIER FLAGS ──────────────────────────────────────────
        # Multi-fit strings like "0(Petite XXS)-8/10(Petite L),12(0XL),14(1XL)"
        # span petite + plus in one string. Extract modifier flags before the
        # primary classification so the dominant system is classified correctly
        # and the modifiers sit alongside it in the output.
        dc[, has_petite := str_detect(size, "Petite")]
        dc[, has_tall   := str_detect(size, "\\bTall\\b")]   # word boundary, not trailing space
        dc[, has_plus   := str_detect(size, "0XL|1XL|2XL|3XL|4XL|5XL|6XL")]
        
        # ── STEP 3: REVISED CLASSIFICATION ORDER ────────────────────────────
        dc[, size_system := fcase(
                # 1. Exact matches first — safest
                size == "one-size",                               "one_size",
                size == "" | is.na(size),                         "missing",
                
                # 2. Unambiguous physical formats
                str_detect(size, "\\d+\\*\\d+"),                  "dimensions",
                
                # 3. Shoe systems — check before anything with digits
                str_detect(size, "CN\\d+"),                        "shoe_intl",
                str_detect(size, "EUR\\d+") &
                        !str_detect(size, "\\bXS\\b|\\bS\\b|\\bM\\b"),  "shoe_intl",
                str_detect(size, "^US\\d+\\.?\\d*(,|$)"),          "shoe_us",
                
                # 4. Bra — requires band+cup pattern, extended cups included
                str_detect(size, "\\d{2}[A-H]{1,2}\\(\\d{2}[A-H]{1,2}\\)"),  "bra",
                
                # 5. Kids
                str_detect(size, "\\dY[,(]|\\d{2}Y[,(]|IN\\)"),   "kids",
                
                # 6. Jeans
                str_detect(size, "W\\d+ L\\d+"),                   "jeans",
                
                # 7. Volume/length — BEFORE us_letter (1.5M starts with digit)
                str_detect(size, "(?i)\\d+\\s*(ml|m,|m$| m,| m$)|\\d+\\s*inch"),  "volume_length",
                
                # 8. Plus
                str_detect(size, "0XL|1XL|2XL|3XL|4XL|5XL|6XL"),  "plus",
                
                # 9. US clothing with numeric equivalents in parens
                str_detect(size, "[A-Z]+\\(\\d"),                   "us_clothing",
                
                # 10. Numeric-first reverse format: 2(XS),4(S)
                str_detect(size, "^\\d+\\s*\\([A-Z]"),              "us_clothing",
                
                # 11. EU clothing — broader: XS(34), XL(42), XXS(30), XXL(50)
                str_detect(size, "[A-Z]+\\(\\d{2}\\)"),             "eu_clothing",
                
                # 12. Standard letter only
                str_detect(size, "\\bXXS\\b|\\bXS\\b|\\bS\\b|\\bM\\b|\\bL\\b|\\bXL\\b|\\bXXL\\b"),  "standard",
                
                # 13. Starts with digit — now only reaches here if not volume/shoe/clothing
                str_detect(size, "^\\d"),                           "us_letter",
                
                default =                                           "unknown"
        )]
        
        # ── STEP 4: DERIVE STRUCTURED COLUMNS (VECTORISED) ─────────────────
        # Initialise all derived columns as NA
        dc[, `:=`(
                dimensions_raw = NA_character_,
                size_labels    = NA_character_,
                size_us        = NA_character_,
                size_eu        = NA_character_,
                waist          = NA_character_,
                inseam         = NA_character_
        )]
        
        # Dimensions: keep the raw dimension string as-is
        dc[size_system == "dimensions",
           dimensions_raw := size]
        
        # US clothing: extract letter labels and US numeric from "XS(2),S(4),M(6),L(8/10)"
        dc[size_system == "us_clothing",
           size_labels := str_extract_all(size, "[A-Z]+(?=\\()") |> sapply(paste, collapse = ", ")]
        dc[size_system == "us_clothing",
           size_us := str_extract_all(size, "(?<=\\()[0-9/]+(?=\\))") |> sapply(paste, collapse = ", ")]
        
        # Standard: letters only
        dc[size_system == "standard",
           size_labels := size]
        
        # Plus: extract XL labels and numeric
        dc[size_system == "plus",
           size_labels := str_extract_all(size, "[0-9]+XL") |> sapply(paste, collapse = ", ")]
        dc[size_system == "plus",
           size_us := str_extract_all(size, "(?<=\\()[0-9]+(?=\\))") |> sapply(paste, collapse = ", ")]
        
        # Shoe international: extract EU/CN and US equivalents
        dc[size_system == "shoe_intl",
           size_eu := str_extract_all(size, "(?<=EUR|CN)[0-9]+") |> sapply(paste, collapse = ", ")]
        dc[size_system == "shoe_intl",
           size_us := str_extract_all(size, "(?<=US)[0-9.]+") |> sapply(paste, collapse = ", ")]
        
        # Shoe US: keep raw string as labels
        dc[size_system == "shoe_us",
           size_labels := size]
        
        # EU clothing: extract letter labels and EU numeric
        dc[size_system == "eu_clothing",
           size_labels := str_extract_all(size, "[A-Z]+(?=\\()") |> sapply(paste, collapse = ", ")]
        dc[size_system == "eu_clothing",
           size_eu := str_extract_all(size, "(?<=\\()[0-9]+(?=\\))") |> sapply(paste, collapse = ", ")]
        
        # Jeans: extract waist and inseam separately
        dc[size_system == "jeans",
           waist := str_extract(size, "(?<=W)\\d+")]
        dc[size_system == "jeans",
           inseam := str_extract(size, "(?<=L)\\d+")]
        
        # Kids: extract age
        dc[size_system == "kids",
           size_labels := str_extract_all(size, "\\d+Y") |> sapply(paste, collapse = ", ")]
        
        # US letter (numeric-first reverse): extract letter from parens
        dc[size_system == "us_letter",
           size_labels := str_extract_all(size, "(?<=\\()[A-Za-z/ ]+(?=\\))") |> sapply(paste, collapse = ", ")]
        dc[size_system == "us_letter",
           size_us := str_extract_all(size, "\\d+(?=\\()") |> sapply(paste, collapse = ", ")]
        
        # ── STEP 5: SIZE COUNT FIX ──────────────────────────────────────────
        # Range notation like "S-XL" counts as 1 on comma split but covers
        # multiple sizes. Mark range-notation rows as NA rather than silently
        # undercounting.
        dc[, size_count := fcase(
                size_system == "one_size",                              1L,
                size_system == "missing",                               NA_integer_,
                size_system == "dimensions",                            NA_integer_,
                size_system == "volume_length",                         NA_integer_,
                str_detect(size, "-") & !str_detect(size, "\\*"),       NA_integer_,
                default = as.integer(lengths(str_split(size, ",")))
        )]
        
        dc[, size := NULL]
        
        log_drop(
                column = "size",
                action = "replaced",
                reason = paste0(
                        "Multi-value string spanning multiple size systems. ",
                        "Replaced by: size_system (classification), size_labels ",
                        "(letter labels), size_us (US numeric), size_eu (EU numeric), ",
                        "dimensions_raw (bedding), waist/inseam (jeans), ",
                        "size_count (option count), has_petite/has_tall/has_plus ",
                        "(modifier flags). ",
                        "Columns for non-applicable systems left NA intentionally ",
                        "to preserve one-file structure for imputation."
                )
        )
        
} else {
        warning("[clean] No 'size' column found — skipping size cleaning.")
}

# =============================================================================
# SECTION H — Images (basic cleaning — dedup URLs, count)
# =============================================================================
# The images column is a raw scraped string with brackets, quotes, and
# potential duplicate URLs. Clean it before carrying forward.

message("[clean] H — Images: deduplicating URLs and counting ...")

if ("images" %in% names(dc)) {
        
        dc[, images_clean := vapply(images, function(x) {
                urls <- str_extract_all(x, "https://[^'\"\\]]+")[[1]]
                paste(unique(urls), collapse = " | ")
        }, character(1))]
        
        dc[, images_n := vapply(images_clean, function(x) {
                length(str_split(x, " \\| ")[[1]])
        }, integer(1))]
        
        dc[, images := NULL]
        
        log_drop(
                column = "images",
                action = "replaced",
                reason = paste0(
                        "Raw scraped string with brackets/quotes. Replaced by: ",
                        "images_clean (deduplicated pipe-separated URLs), ",
                        "images_n (count of unique image URLs per product)."
                )
        )
}

# =============================================================================
# SECTION I — Assemble final cleaned data.frame
# =============================================================================

message("[clean] I — Assembling final cleaned dataset ...")

# Identify any remaining columns not yet handled — carry forward
handled <- c("sku", "url", "name", "price", "price_flag",
             "brand", "color",
             "size_system", "size_labels", "size_us", "size_eu",
             "dimensions_raw", "waist", "inseam", "size_count",
             "has_petite", "has_tall", "has_plus",
             "images_clean", "images_n",
             "description", "size", "images")

remaining <- setdiff(names(dc), handled)

priority_cols <- intersect(
        c("sku", "url", "name", "price", "price_flag",
          "brand", "color",
          "size_system", "size_count", "size_labels", "size_us", "size_eu",
          "dimensions_raw", "waist", "inseam",
          "has_petite", "has_tall", "has_plus",
          "images_n", "images_clean"),
        names(dc)
)

df_clean <- as.data.frame(dc[, c(priority_cols, remaining), with = FALSE])

# =============================================================================
# SECTION J — Summary & drop log
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
# SECTION K — Export
# =============================================================================

CLEAN_FILE <- file.path(OUTPUT_DIR, "shein_cleaned.csv")

fwrite(df_clean, CLEAN_FILE, na = "")

message("[clean] Cleaned data saved → ", CLEAN_FILE)

