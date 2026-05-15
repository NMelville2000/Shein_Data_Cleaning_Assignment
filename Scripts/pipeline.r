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
EXCEL_SAMPLE_ROWS <- 111189

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

# Create output directory once for all downstream scripts
dir.create(OUTPUT_DIR, showWarnings = FALSE)

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


# Keep as data.table — avoids unnecessary data.table → data.frame → data.table
# round-trip. Diagnosis section converts to data.frame locally where dplyr needs it.

# -----------------------------------------------------------------------------
# 3. Structural validation
# -----------------------------------------------------------------------------

stopifnot(
        "Ingested object is not a data.table" = is.data.table(raw_file),
        "Ingested data has no rows"           = nrow(raw_file) > 0,
        "Ingested data has no columns"        = ncol(raw_file) > 0
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



# =============================================================================
# 02_diagnosis.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Produce a comprehensive exploratory-diagnostics workbook covering:
#             • Dataset overview
#             • Variable types & missingness
#             • Duplicate detection
#             • Numeric range checks & outlier flags
#             • Price / discount sanity checks
#             • URL validity checks
#             • Date / scrape-timestamp parsing
#             • Text quality checks
#             • Category / color / size / material level summaries
#             • Description key-value extraction
#             • Flagged problem rows
# Output:   Output/shein_exploratory_diagnostics.xlsx
# Depends:  00_config.R + 01_ingest.R (raw_file must exist in environment)
# =============================================================================

#source("00_config.R")

if (!exists("raw_file")) source("01_ingest.R")

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# =============================================================================
# SECTION A — Helpers
# =============================================================================

# ---------------------------------------------------------------------------
# A1. Excel text sanitiser — strips control characters that break openxlsx
# ---------------------------------------------------------------------------

clean_for_excel <- function(x) {
        if (is.character(x)) {
                x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
                x <- stri_replace_all_regex(x, "\\p{C}", "")
        }
        x
}

clean_df_for_excel <- function(data) {
        as.data.frame(lapply(data, clean_for_excel), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# A2. Column auto-detector — returns the first column name matching a pattern
# ---------------------------------------------------------------------------

find_col <- function(df, pattern) {
        hits <- names(df)[str_detect(tolower(names(df)), pattern)]
        if (length(hits) == 0) NA_character_ else hits[1]
}

# ---------------------------------------------------------------------------
# A3. openxlsx helper — add a sheet with auto-width columns
# ---------------------------------------------------------------------------

add_sheet <- function(wb, sheet_name, data) {
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, clean_df_for_excel(data))
        setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
}

# =============================================================================
# SECTION B — Setup: working copy & column mapping
# =============================================================================

# Convert to data.frame locally — dplyr verbs in diagnosis need data.frame;
# raw_file stays as data.table for 03_clean downstream.
df <- as.data.frame(raw_file) %>% mutate(row_number = row_number())

# --- Detect expected columns by heuristic name patterns ---
detected <- list(
        product     = find_col(df, "product.*name|product_name|title|^name$"),
        price       = find_col(df, "^price$|sale.*price|current.*price|price"),
        discount    = find_col(df, "discount|markdown|promo"),
        color       = find_col(df, "colou?r"),
        size        = find_col(df, "^size$|sizes"),
        category    = find_col(df, "category|department|class"),
        material    = find_col(df, "material|fabric"),
        product_url = find_col(df, "product.*url|^url$|^link$"),
        image_url   = find_col(df, "image.*url|^image$|^img$"),
        scraped_at  = find_col(df, "scraped|timestamp|scraped_at")
)

# =============================================================================
# SECTION C — Diagnostics
# =============================================================================

# ---------------------------------------------------------------------------
# C1. Dataset overview
# ---------------------------------------------------------------------------

overview <- data.frame(
        metric = c(
                "Total rows",
                "Total columns",
                "Total cells",
                "Exact duplicate rows",
                "Rows with ≥1 missing value",
                "Columns with ≥1 missing value"
        ),
        value = c(
                nrow(df),
                ncol(df),
                nrow(df) * ncol(df),
                sum(duplicated(df)),
                sum(rowSums(is.na(df) | df == "") > 0),
                sum(colSums(is.na(df) | df == "") > 0)
        )
)

detected_columns_tbl <- data.frame(
        expected_field   = names(detected),
        detected_column  = unlist(detected),
        row.names        = NULL
)

# ---------------------------------------------------------------------------
# C2. Description key-value extraction
# ---------------------------------------------------------------------------

message("[diagnosis] Extracting description key-value pairs ...")

description_pairs <- map2_dfr(
        df$row_number,
        df$description,
        function(rn, txt) {
                if (is.na(txt) || txt == "") return(NULL)
                m <- str_match_all(txt, "'([^']+)'\\s*:\\s*'([^']*)'")[[1]]
                if (nrow(m) == 0) return(NULL)
                data.frame(
                        row_number      = rn,
                        attribute_name  = m[, 2],
                        attribute_value = m[, 3],
                        stringsAsFactors = FALSE
                )
        }
)

description_pairs <- description_pairs %>%
        mutate(
                attribute_name  = str_squish(attribute_name),
                attribute_value = str_squish(attribute_value)
        )

desc_attr_name_counts <- description_pairs %>%
        filter(!is.na(attribute_name), attribute_name != "") %>%
        count(attribute_name, sort = TRUE, name = "count")

desc_attr_value_counts <- description_pairs %>%
        filter(!is.na(attribute_value), attribute_value != "") %>%
        count(attribute_value, sort = TRUE, name = "count")

desc_name_value_counts <- description_pairs %>%
        filter(
                !is.na(attribute_name),  attribute_name  != "",
                !is.na(attribute_value), attribute_value != ""
        ) %>%
        count(attribute_name, attribute_value, sort = TRUE, name = "count")

# ---------------------------------------------------------------------------
# C3. Variable types & structure
# ---------------------------------------------------------------------------

# Compute missing counts once — reused in C3 and C4
.missing_counts     <- sapply(df, \(x) sum(is.na(x) | x == ""))
.non_missing_counts <- nrow(df) - .missing_counts


variable_types <- data.frame(
        variable          = names(df),
        r_type            = sapply(df, \(x) paste(class(x), collapse = ", ")),
        non_missing_count = .non_missing_counts,
        missing_count     = .missing_counts,
        unique_count      = sapply(df, \(x) length(unique(x))),
        stringsAsFactors  = FALSE
) %>%
        mutate(missing_pct = round(missing_count / nrow(df) * 100, 2)) %>%
        arrange(desc(missing_pct))

# ---------------------------------------------------------------------------
# C4. Missingness
# ---------------------------------------------------------------------------

missingness_by_variable <- data.frame(
        variable      = names(df),
        missing_count = .missing_counts,
        non_missing   = .non_missing_counts,
        missing_pct   = round(.missing_counts / nrow(df) * 100, 2),
        stringsAsFactors = FALSE
) %>%
        arrange(desc(missing_pct))

missingness_by_row <- data.frame(
        missing_values_per_row = rowSums(is.na(df) | df == "")
) %>%
        count(missing_values_per_row, name = "row_count") %>%
        arrange(desc(missing_values_per_row))

# ---------------------------------------------------------------------------
# C5. Duplicate checks
# ---------------------------------------------------------------------------

safe_dup_count <- function(col_name) {
        if (is.na(col_name)) return(NA_integer_)
        v <- df[[col_name]]
        sum(duplicated(v) & !(is.na(v) | v == ""))
}

duplicate_summary <- data.frame(
        check = c(
                "Exact duplicate rows",
                "Duplicate product URLs",
                "Duplicate product names",
                "Duplicate image URLs"
        ),
        count = c(
                sum(duplicated(df)),
                safe_dup_count(detected$product_url),
                safe_dup_count(detected$product),
                safe_dup_count(detected$image_url)
        )
)

exact_dup_examples <- head(df[duplicated(df) | duplicated(df, fromLast = TRUE), ],
                           EXCEL_SAMPLE_ROWS)

if (!is.na(detected$product_url)) {
        pu <- df[[detected$product_url]]
        dup_url_examples <- head(
                df[(duplicated(pu) | duplicated(pu, fromLast = TRUE)) & !(is.na(pu) | pu == ""), ],
                EXCEL_SAMPLE_ROWS
        )
} else {
        dup_url_examples <- data.frame(note = "No product URL column detected.")
}

# Image URL duplication — check for within-record duplicate URLs
message("[diagnosis] Checking image URL duplication ...")

# Vectorised check: does each record contain repeated image URLs?
# Replaces the previous rowwise() approach which was very slow on 111k rows.
# Also removed the separate_rows() explosion (exploded_images / image_url_counts)
# because those objects were computed but never written to the workbook.
.has_dup_images <- vapply(df$images, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        urls <- trimws(str_remove_all(unlist(strsplit(x, ",\\s*")), "[\\[\\]']"))
        length(urls) != length(unique(urls))
}, logical(1))

n_records_with_dup_images <- sum(.has_dup_images)

image_dup_summary <- data.frame(
        metric = c(
                "Records with at least one duplicate image URL",
                "Pct of total records"
        ),
        value = c(
                n_records_with_dup_images,
                paste0(round(n_records_with_dup_images / nrow(df) * 100, 2), "%")
        )
)

# ---------------------------------------------------------------------------
# C6. Numeric range checks
# ---------------------------------------------------------------------------

message("[diagnosis] Running numeric range checks ...")

numeric_cols      <- names(df)[sapply(df, is.numeric)]
numeric_like_cols <- names(df)[str_detect(
        tolower(names(df)),
        "price|cost|amount|discount|rating|review|stock|quantity|qty|count|sales|weight"
)]

range_check_cols <- unique(c(numeric_cols, numeric_like_cols))

range_checks       <- data.frame()
numeric_parse_issues <- data.frame()

for (col in range_check_cols) {
        
        x     <- df[[col]]
        x_chr <- as.character(x)
        
        parsed <- if (is.numeric(x)) x else parse_number(x_chr, na = MESSY_MISSING_VALUES)
        
        # Collect values that failed to parse
        failed_idx <- which(is.na(parsed) & !(is.na(x_chr) | x_chr %in% MESSY_MISSING_VALUES))
        if (length(failed_idx) > 0) {
                numeric_parse_issues <- bind_rows(
                        numeric_parse_issues,
                        data.frame(
                                variable       = col,
                                row_number     = failed_idx,
                                original_value = x_chr[failed_idx],
                                issue          = "Value could not be parsed as a number"
                        )
                )
        }
        
        vals <- parsed[!is.na(parsed)]
        if (length(vals) == 0) next
        
        q1  <- quantile(vals, 0.25)
        q3  <- quantile(vals, 0.75)
        iqr <- q3 - q1
        
        range_checks <- bind_rows(range_checks, data.frame(
                variable               = col,
                original_type          = paste(class(x), collapse = ", "),
                parsed_count           = sum(!is.na(parsed)),
                parsed_pct             = round(sum(!is.na(parsed)) / nrow(df) * 100, 2),
                min                    = min(vals),
                q1                     = q1,
                median                 = median(vals),
                mean                   = mean(vals),
                q3                     = q3,
                max                    = max(vals),
                negative_count         = sum(vals < 0),
                zero_count             = sum(vals == 0),
                iqr_outlier_count      = sum(parsed < (q1 - IQR_MULTIPLIER * iqr) |
                                                     parsed > (q3 + IQR_MULTIPLIER * iqr), na.rm = TRUE)
        ))
}

if (nrow(range_checks)        == 0) range_checks        <- data.frame(note = "No numeric columns found.")
if (nrow(numeric_parse_issues) == 0) numeric_parse_issues <- data.frame(note = "No numeric parse issues found.")

# ---------------------------------------------------------------------------
# C7. Price & discount sanity checks
# ---------------------------------------------------------------------------

price_discount_checks <- data.frame()

if (!is.na(detected$price)) {
        p <- parse_number(as.character(df[[detected$price]]))
        price_discount_checks <- bind_rows(price_discount_checks, data.frame(
                check = c("Price: could not be parsed", "Price: < 0", "Price: == 0"),
                count = c(
                        sum(is.na(p) & !(is.na(df[[detected$price]]) | df[[detected$price]] == "")),
                        sum(p < 0,  na.rm = TRUE),
                        sum(p == 0, na.rm = TRUE)
                )
        ))
}

if (!is.na(detected$discount)) {
        d <- parse_number(as.character(df[[detected$discount]]))
        price_discount_checks <- bind_rows(price_discount_checks, data.frame(
                check = c("Discount: could not be parsed", "Discount: < 0", "Discount: > 100"),
                count = c(
                        sum(is.na(d) & !(is.na(df[[detected$discount]]) | df[[detected$discount]] == "")),
                        sum(d < 0,   na.rm = TRUE),
                        sum(d > 100, na.rm = TRUE)
                )
        ))
}

if (nrow(price_discount_checks) == 0)
        price_discount_checks <- data.frame(note = "No price or discount column detected.")

# ---------------------------------------------------------------------------
# C8. URL validity checks
# ---------------------------------------------------------------------------

check_url_col <- function(col_name, label) {
        if (is.na(col_name)) {
                return(data.frame(
                        url_type = label, variable = NA,
                        total_non_missing = NA, valid_count = NA,
                        invalid_count = NA, duplicate_count = NA
                ))
        }
        v        <- as.character(df[[col_name]])
        present  <- !(is.na(v) | v == "")
        valid    <- str_detect(v, "^https?://")
        data.frame(
                url_type        = label,
                variable        = col_name,
                total_non_missing = sum(present),
                valid_count     = sum(valid & present),
                invalid_count   = sum(!valid & present),
                duplicate_count = sum(duplicated(v) & present)
        )
}

url_checks <- bind_rows(
        check_url_col(detected$product_url, "Product URL"),
        check_url_col(detected$image_url,   "Image URL")
)

# ---------------------------------------------------------------------------
# C9. Date / scrape-timestamp checks
# ---------------------------------------------------------------------------

if (!is.na(detected$scraped_at)) {
        raw_dates    <- as.character(df[[detected$scraped_at]])
        parsed_dates <- parse_date_time(raw_dates, orders = DATE_ORDERS, quiet = TRUE)
        non_missing  <- parsed_dates[!is.na(parsed_dates)]
        
        datetime_checks <- data.frame(
                variable          = detected$scraped_at,
                total_non_missing = sum(!(is.na(raw_dates) | raw_dates == "")),
                parsed_count      = sum(!is.na(parsed_dates)),
                unparsed_count    = sum(is.na(parsed_dates) & !(is.na(raw_dates) | raw_dates == "")),
                earliest_date     = if (length(non_missing) > 0) as.character(min(non_missing)) else NA,
                latest_date       = if (length(non_missing) > 0) as.character(max(non_missing)) else NA
        )
} else {
        datetime_checks <- data.frame(note = "No scraped date / timestamp column detected.")
}

# ---------------------------------------------------------------------------
# C10. Text quality checks
# ---------------------------------------------------------------------------

safe_text <- function(x) {
        x <- as.character(x)
        x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
        stri_replace_all_regex(x, "\\p{C}", "")
}

char_cols <- names(df)[sapply(df, is.character)]

text_quality_checks <- map_dfr(char_cols, function(col) {
        x <- safe_text(df[[col]])
        data.frame(
                variable                     = col,
                blank_count                  = sum(x == "",                        na.rm = TRUE),
                leading_trailing_space_count = sum(str_detect(x, "^\\s|\\s$"),     na.rm = TRUE),
                multiple_space_count         = sum(str_detect(x, "\\s{2,}"),       na.rm = TRUE),
                newline_count                = sum(str_detect(x, "\\n|\\r"),       na.rm = TRUE),
                very_long_text_count         = sum(str_length(x) > LONG_TEXT_THRESHOLD, na.rm = TRUE),
                max_text_length              = max(str_length(x),                  na.rm = TRUE),
                unique_count                 = length(unique(x))
        )
})

if (nrow(text_quality_checks) == 0)
        text_quality_checks <- data.frame(note = "No character columns detected.")

# ---------------------------------------------------------------------------
# C11. Category / color / size / material level summaries
# ---------------------------------------------------------------------------

cat_cols <- Filter(Negate(is.na), c(
        detected$category, detected$color, detected$size, detected$material
))

level_summary <- map_dfr(cat_cols, function(col) {
        df %>%
                transmute(value = as.character(.data[[col]])) %>%
                filter(!(is.na(value) | value == "")) %>%
                count(value, sort = TRUE, name = "count") %>%
                mutate(variable = col) %>%
                select(variable, value, count) %>%
                head(EXCEL_SAMPLE_ROWS)
})

if (nrow(level_summary) == 0)
        level_summary <- data.frame(note = "No category / color / size / material columns detected.")

# ---------------------------------------------------------------------------
# C12. Problem-row flags
# ---------------------------------------------------------------------------

issue_flags <- data.frame(row_number = seq_len(nrow(df)))

if (!is.na(detected$price)) {
        pn <- parse_number(as.character(df[[detected$price]]))
        issue_flags$price_missing_or_invalid <- is.na(pn)
        issue_flags$price_negative           <- !is.na(pn) & pn < 0
}

if (!is.na(detected$discount)) {
        dn <- parse_number(as.character(df[[detected$discount]]))
        issue_flags$discount_over_100 <- !is.na(dn) & dn > 100
        issue_flags$discount_negative <- !is.na(dn) & dn < 0
}

flag_url_invalid <- function(col_name, flag_col) {
        if (is.na(col_name)) return(NULL)
        v <- as.character(df[[col_name]])
        issue_flags[[flag_col]] <<- !str_detect(v, "^https?://") & !(is.na(v) | v == "")
}

flag_url_invalid(detected$product_url, "product_url_invalid")
flag_url_invalid(detected$image_url,   "image_url_invalid")

flag_cols <- setdiff(names(issue_flags), "row_number")
issue_flags$issue_count <- rowSums(issue_flags[flag_cols], na.rm = TRUE)

problem_rows <- head(df[issue_flags$row_number[issue_flags$issue_count > 0], ],
                     100)

if (nrow(problem_rows) == 0)
        problem_rows <- data.frame(note = "No problem rows flagged.")

# =============================================================================
# SECTION D — Write diagnostics workbook
# =============================================================================

message("[diagnosis] Writing diagnostics workbook ...")

wb <- createWorkbook()

add_sheet(wb, "01 Overview",              overview)
add_sheet(wb, "02 Detected Columns",      detected_columns_tbl)
add_sheet(wb, "03 Variable Types",        variable_types)
add_sheet(wb, "04 Missingness Variable",  missingness_by_variable)
add_sheet(wb, "05 Missingness Row",       missingness_by_row)
add_sheet(wb, "06 Duplicate Summary",     duplicate_summary)
add_sheet(wb, "07 Exact Dup Examples",    exact_dup_examples)
add_sheet(wb, "08 Duplicate URL Exs",     dup_url_examples)
add_sheet(wb, "09 Image URL Dup Summary", image_dup_summary)
add_sheet(wb, "10 Range Checks",          range_checks)
add_sheet(wb, "11 Numeric Parse Issues",  numeric_parse_issues)
add_sheet(wb, "12 Price Discount Checks", price_discount_checks)
add_sheet(wb, "13 URL Checks",            url_checks)
add_sheet(wb, "14 Datetime Checks",       datetime_checks)
add_sheet(wb, "15 Text Quality",          text_quality_checks)
add_sheet(wb, "16 Level Summary",         level_summary)
add_sheet(wb, "17 Desc Attr Names",       desc_attr_name_counts)
add_sheet(wb, "18 Desc Attr Values",      desc_attr_value_counts)
add_sheet(wb, "19 Desc Name-Value",       desc_name_value_counts)
add_sheet(wb, "20 Problem Rows",          problem_rows)

saveWorkbook(wb, DIAGNOSTICS_FILE, overwrite = TRUE)

#message("[diagnosis] Workbook saved → ", DIAGNOSTICS_FILE)

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

# =============================================================================
# 04_impute.R
# Project:  Shein Data Quality Pipeline
# Purpose:  Impute missing prices in the cleaned dataset produced by 03_clean.R.
#
# Method:   MEDIAN IMPUTATION (grouped by size_system)
#
# Justification:
#   Retail product prices are typically right-skewed — a mass of low-priced
#   items with a long tail of expensive products.  The mean is pulled upward
#   by that tail, so substituting it would systematically over-estimate the
#   "typical" price for the missing rows.  The median resists this pull and
#   gives a more representative central value.
#
#   Grouping by size_system further improves accuracy: bedding (dimensions),
#   shoes, and clothing occupy different price bands, so a global median
#   would blur those differences.  Where a size_system group has no observed
#   prices to compute a median, the script falls back to the global median
#   so that no row is left unimputed.
#
#   Hot-deck was considered but adds stochastic variation that is harder to
#   reproduce and justify in a student report.  Mode imputation is designed
#   for categorical variables, not continuous prices.  Mean imputation is
#   inappropriate given the expected skew.  Constant/flag imputation (e.g.
#   filling with 0 or "Unknown") would distort any downstream numeric
#   analysis.  Leaving prices missing was rejected because price is a core
#   analytic variable and the missingness rate, while low, would propagate
#   NAs through every price-dependent calculation.
#
# Input:    df_clean   (data.frame from 03_clean.R, must exist in environment)
# Output:   df_imputed (data.frame with price imputed)
#           impute_log (data.frame documenting every imputation action)
#           Output/shein_imputed.csv
# Depends:  00_config.R + 01_ingest.R + 02_diagnosis.R + 03_clean.R
# =============================================================================

if (!exists("df_clean")) source("03_clean.r")

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[impute] Starting 04_impute.r ...")

# =============================================================================
# SECTION A — Working copy
# =============================================================================

di <- as.data.table(df_clean)
setDT(di)

# =============================================================================
# SECTION B — Imputation log initialisation
# =============================================================================

impute_log <- data.frame(
        variable      = character(),
        method        = character(),
        group         = character(),
        fill_value    = character(),
        rows_imputed  = integer(),
        reason        = character(),
        stringsAsFactors = FALSE
)

log_impute <- function(variable, method, group, fill_value, rows_imputed, reason) {
        impute_log <<- rbind(
                impute_log,
                data.frame(
                        variable     = variable,
                        method       = method,
                        group        = group,
                        fill_value   = as.character(fill_value),
                        rows_imputed = as.integer(rows_imputed),
                        reason       = reason,
                        stringsAsFactors = FALSE
                )
        )
}

# =============================================================================
# SECTION C — Identify rows needing price imputation
# =============================================================================

message("[impute] C — Identifying missing prices ...")

# Rows flagged as "missing" by 03_clean's price_flag column are the
# imputation targets.  Rows flagged "invalid" (price <= 0) are also
# candidates — a zero or negative scraped price is not meaningful.

if ("price_flag" %in% names(di)) {
        needs_impute <- di$price_flag %in% c("missing", "invalid")
} else {
        # Fallback: treat NA prices as needing imputation
        needs_impute <- is.na(di$price)
}

n_missing <- sum(needs_impute)
message("[impute]   Rows requiring price imputation: ", n_missing,
        " out of ", nrow(di))

# =============================================================================
# SECTION D — Pre-imputation diagnostics
# =============================================================================

message("[impute] D — Pre-imputation price distribution ...")

valid_prices <- di[!needs_impute, price]
valid_prices <- valid_prices[!is.na(valid_prices) & valid_prices > 0]

if (length(valid_prices) > 0) {
        global_median <- median(valid_prices)
        global_mean   <- mean(valid_prices)
        
        cat("\n=== Pre-Imputation Price Summary (valid rows only) ===\n")
        cat("  N valid:    ", length(valid_prices), "\n")
        cat("  Min:        ", round(min(valid_prices), 2), "\n")
        cat("  Q1:         ", round(quantile(valid_prices, 0.25), 2), "\n")
        cat("  Median:     ", round(global_median, 2), "\n")
        cat("  Mean:       ", round(global_mean, 2), "\n")
        cat("  Q3:         ", round(quantile(valid_prices, 0.75), 2), "\n")
        cat("  Max:        ", round(max(valid_prices), 2), "\n")
        cat("  Mean − Median = ", round(global_mean - global_median, 2),
            "  (positive → right skew, confirming median choice)\n\n")
} else {
        global_median <- NA_real_
        warning("[impute] No valid prices found — cannot compute imputation values.")
}

# =============================================================================
# SECTION E — Grouped median imputation (by size_system)
# =============================================================================
# Strategy:
#   1. Compute median price per size_system from the valid (non-missing) rows.
#   2. For each missing-price row, fill with the group median.
#   3. If a group has zero valid prices, fall back to the global median.
#   4. Log every group's fill value and row count.

message("[impute] E — Applying grouped median imputation ...")

if (n_missing > 0 && !is.na(global_median)) {
        
        # ── Step 1: group medians ───────────────────────────────────────────
        
        group_col <- "size_system"
        
        if (group_col %in% names(di)) {
                
                group_medians <- di[!needs_impute & !is.na(price) & price > 0,
                                    .(group_median = median(price)),
                                    by = size_system]
                
                cat("=== Group Medians (by size_system) ===\n")
                print(group_medians[order(-group_median)])
                cat("\n")
                
                # ── Step 2: merge group medians onto the full table ─────────
                
                di <- merge(di, group_medians, by = "size_system", all.x = TRUE, sort = FALSE)
                
                # ── Step 3: impute — group median first, global fallback ────
                
                di[needs_impute,
                   price_imputed := fifelse(
                           !is.na(group_median),
                           group_median,
                           global_median
                   )]
                
                di[!needs_impute, price_imputed := price]
                
                # ── Step 4: log each group's contribution ───────────────────
                
                imputed_rows <- di[needs_impute]
                
                if (nrow(imputed_rows) > 0) {
                        group_counts <- imputed_rows[, .N, by = size_system]
                        
                        for (i in seq_len(nrow(group_counts))) {
                                grp  <- group_counts$size_system[i]
                                cnt  <- group_counts$N[i]
                                gmed <- group_medians[size_system == grp, group_median]
                                
                                if (length(gmed) == 0 || is.na(gmed)) {
                                        fill_val <- global_median
                                        note     <- "No valid prices in group; used global median fallback"
                                } else {
                                        fill_val <- gmed
                                        note     <- "Group median applied"
                                }
                                
                                log_impute(
                                        variable     = "price",
                                        method       = "median",
                                        group        = grp,
                                        fill_value   = round(fill_val, 2),
                                        rows_imputed = cnt,
                                        reason       = note
                                )
                        }
                }
                
                # ── Step 5: replace price with imputed values, clean up ─────
                
                di[, price := price_imputed]
                di[, c("group_median", "price_imputed") := NULL]
                
                # Update price_flag for imputed rows
                di[needs_impute, price_flag := "imputed"]
                
        } else {
                # No size_system column — fall back to ungrouped global median
                
                di[needs_impute, price := global_median]
                di[needs_impute, price_flag := "imputed"]
                
                log_impute(
                        variable     = "price",
                        method       = "median",
                        group        = "(global — no size_system column)",
                        fill_value   = round(global_median, 2),
                        rows_imputed = n_missing,
                        reason       = "Global median; size_system not available for grouping"
                )
        }
        
} else if (n_missing == 0) {
        message("[impute]   No missing prices — nothing to impute.")
        log_impute(
                variable     = "price",
                method       = "none",
                group        = "—",
                fill_value   = "—",
                rows_imputed = 0L,
                reason       = "No missing prices detected; imputation not required"
        )
} else {
        warning("[impute] Cannot impute — no valid reference prices available.")
}

# =============================================================================
# SECTION F — Post-imputation diagnostics
# =============================================================================

message("[impute] F — Post-imputation summary ...")

all_prices <- di[!is.na(price) & price > 0, price]

if (length(all_prices) > 0) {
        cat("\n=== Post-Imputation Price Summary ===\n")
        cat("  N total:    ", length(all_prices), "\n")
        cat("  Min:        ", round(min(all_prices), 2), "\n")
        cat("  Q1:         ", round(quantile(all_prices, 0.25), 2), "\n")
        cat("  Median:     ", round(median(all_prices), 2), "\n")
        cat("  Mean:       ", round(mean(all_prices), 2), "\n")
        cat("  Q3:         ", round(quantile(all_prices, 0.75), 2), "\n")
        cat("  Max:        ", round(max(all_prices), 2), "\n")
        cat("  Remaining NAs: ", sum(is.na(di$price)), "\n\n")
}

cat("=== Price Flag Distribution (post-imputation) ===\n")
if ("price_flag" %in% names(di)) {
        print(di[, .N, by = price_flag][order(-N)])
}

# =============================================================================
# SECTION G — Assemble imputed output
# =============================================================================

message("[impute] G — Assembling final imputed dataset ...")

df_imputed <- as.data.frame(di)

# =============================================================================
# SECTION H — Summary & imputation log
# =============================================================================

cat("\n=== [04_impute] Final Imputed Dataset ===\n")
cat("Rows:   ", formatC(nrow(df_imputed), big.mark = ","), "\n")
cat("Columns:", ncol(df_imputed), "\n\n")

cat("=== Imputation Log ===\n")
print(impute_log)

cat("\n=== Missing Value Counts (post-imputation) ===\n")
missing_summary <- sort(
        sapply(df_imputed, function(x) sum(is.na(x) | x == "")),
        decreasing = TRUE
)
print(missing_summary)

# =============================================================================
# SECTION I — Export
# =============================================================================

IMPUTED_FILE <- file.path(OUTPUT_DIR, "shein_imputed.csv")

write.csv(df_imputed, IMPUTED_FILE, row.names = FALSE, na = "")

message("[impute] Imputed data saved → ", IMPUTED_FILE)

# =============================================================================
# 04.1_charts.r
# Project:  Shein Data Quality Pipeline
# Purpose:  Generate four report-ready charts from the cleaned and imputed
#           dataset, using a consistent visual theme.
#
# Charts:
#   1. Missingness overview — % missing by variable, before vs. after cleaning
#   2. Price distribution   — histogram of cleaned prices (numeric variable 1)
#   3. Size count distribution — bar chart of size option counts (numeric var 2)
#   4. Observed vs. imputed — price comparison for the imputed variable
#   5. Price by size system — substantive insight chart
#
# Output:   Output/chart_1_missingness.png
#           Output/chart_2_price_distribution.png
#           Output/chart_3_size_count_distribution.png
#           Output/chart_4_observed_vs_imputed.png
#           Output/chart_5_price_by_size_system.png
# Depends:  00_config.R → 01_ingest.R → 02_diagnosis.R → 03_clean.R → 04_impute.R
#           Objects required: raw_file, df_clean, df_imputed, impute_log
# =============================================================================

if (!exists("df_imputed")) source("04_impute.r")

suppressPackageStartupMessages({
        library(ggplot2)
        library(data.table)
        library(stringr)
})

dir.create(OUTPUT_DIR, showWarnings = FALSE)

message("[charts] Starting 04.1_charts.r ...")

# =============================================================================
# THEME — consistent across all charts
# =============================================================================

theme_shein <- theme_minimal(base_size = 11, base_family = "sans") +
        theme(
                plot.title       = element_text(face = "bold", size = 13,
                                                colour = "#1a1a2e", margin = margin(b = 6)),
                plot.subtitle    = element_text(size = 9.5, colour = "#555555",
                                                margin = margin(b = 12)),
                plot.caption     = element_text(size = 7.5, colour = "#999999",
                                                hjust = 0, margin = margin(t = 10)),
                axis.title       = element_text(size = 9.5, colour = "#333333"),
                axis.text        = element_text(size = 8.5, colour = "#444444"),
                panel.grid.major = element_line(colour = "#e8e8e8", linewidth = 0.3),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#fafafa", colour = NA),
                panel.background = element_rect(fill = "#fafafa", colour = NA),
                legend.position  = "bottom",
                legend.title     = element_text(size = 9, face = "bold"),
                legend.text      = element_text(size = 8.5),
                plot.margin      = margin(15, 15, 10, 15)
        )

# Palette
pal <- c(
        before  = "#e07a5f",   # terracotta
        after   = "#3d405b",   # charcoal blue
        accent  = "#81b29a",   # sage green
        light   = "#f2cc8f",   # warm sand
        imputed = "#e07a5f",   # terracotta (reuse for imputed)
        observed = "#3d405b"   # charcoal blue (reuse for observed)
)

CHART_W <- 7
CHART_H <- 4.5
CHART_DPI <- 300

# =============================================================================
# CHART 1 — Missingness overview: % missing by variable, before & after
# =============================================================================

message("[charts] 1 — Missingness overview ...")

# ── "Before" = raw_file (pre-cleaning) ──────────────────────────────────────

raw_dt <- as.data.table(raw_file)

# Compute % missing for the raw columns that map to cleaned columns
raw_miss <- data.table(
        variable = names(raw_dt),
        missing_pct = sapply(raw_dt, function(x) {
                round(sum(is.na(x) | as.character(x) == "") / length(x) * 100, 1)
        }),
        stage = "Before cleaning"
)

# ── "After" = df_imputed (post-cleaning + imputation) ───────────────────────

imp_dt <- as.data.table(df_imputed)

after_miss <- data.table(
        variable = names(imp_dt),
        missing_pct = sapply(imp_dt, function(x) {
                round(sum(is.na(x) | as.character(x) == "") / length(x) * 100, 1)
        }),
        stage = "After cleaning"
)

# ── Align to a common set of conceptual variables ───────────────────────────
# Map raw column names → cleaned equivalents for side-by-side comparison

raw_miss[variable == "sku",         variable := "sku"]
raw_miss[variable == "url",         variable := "url"]
raw_miss[variable == "name",        variable := "name"]
raw_miss[variable == "price",       variable := "price"]
raw_miss[variable == "brand",       variable := "brand"]
raw_miss[variable == "description", variable := "color (from description)"]
raw_miss[variable == "size",        variable := "size_system (from size)"]
raw_miss[variable == "images",      variable := "images"]

after_miss[variable == "color",       variable := "color (from description)"]
after_miss[variable == "size_system", variable := "size_system (from size)"]

# Keep only the variables that appear in both stages (or are meaningful)
keep_vars <- c("sku", "url", "name", "price", "brand",
               "color (from description)", "size_system (from size)", "images")

raw_keep   <- raw_miss[variable %in% keep_vars]
after_keep <- after_miss[variable %in% keep_vars]

miss_combined <- rbind(raw_keep, after_keep)
miss_combined[, stage := factor(stage, levels = c("Before cleaning", "After cleaning"))]

# Order variables by "before" missingness (descending)
var_order <- raw_keep[order(-missing_pct), variable]
miss_combined[, variable := factor(variable, levels = rev(var_order))]

p1 <- ggplot(miss_combined, aes(x = variable, y = missing_pct, fill = stage)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        coord_flip() +
        scale_fill_manual(values = c("Before cleaning" = pal[["before"]],
                                     "After cleaning"  = pal[["after"]]),
                          name = "") +
        scale_y_continuous(labels = function(x) paste0(x, "%"),
                           expand = expansion(mult = c(0, 0.08))) +
        labs(
                title    = "Missingness by Variable — Before vs. After Cleaning",
                subtitle = "Percentage of rows with missing or empty values per field",
                x = NULL,
                y = "% Missing",
                caption  = "Source: shein_sample_data | Pipeline stage: 04.1_charts.r"
        ) +
        theme_shein +
        theme(legend.position = c(0.8, 0.2))

ggsave(file.path(OUTPUT_DIR, "chart_1_missingness.png"),
       p1, width = CHART_W, height = CHART_H, dpi = CHART_DPI)


# =============================================================================
# CHART 2 — Price distribution (numeric variable 1)
# =============================================================================

message("[charts] 2 — Price distribution ...")

prices <- imp_dt[!is.na(price) & price > 0]

p2 <- ggplot(prices, aes(x = price)) +
        geom_histogram(binwidth = 1, fill = pal[["after"]], colour = "white",
                       linewidth = 0.3, alpha = 0.9) +
        geom_vline(aes(xintercept = median(price)),
                   colour = pal[["before"]], linetype = "dashed", linewidth = 0.7) +
        annotate("text",
                 x = median(prices$price) + 0.3,
                 y = Inf, vjust = 1.8, hjust = 0,
                 label = paste0("Median = $", round(median(prices$price), 2)),
                 size = 3.2, colour = pal[["before"]], fontface = "bold") +
        scale_x_continuous(labels = scales::dollar_format(),
                           expand = expansion(mult = c(0.02, 0.05))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(
                title    = "Distribution of Product Prices",
                subtitle = "Post-cleaning histogram with median reference line",
                x = "Price (USD)",
                y = "Number of Products",
                caption  = "Source: shein_imputed | Pipeline stage: 04.1_charts.r"
        ) +
        theme_shein

ggsave(file.path(OUTPUT_DIR, "chart_2_price_distribution.png"),
       p2, width = CHART_W, height = CHART_H, dpi = CHART_DPI)


# =============================================================================
# CHART 3 — Size count distribution (numeric variable 2)
# =============================================================================

message("[charts] 3 — Size count distribution ...")

size_counts <- imp_dt[!is.na(size_count)]

p3 <- ggplot(size_counts, aes(x = factor(size_count))) +
        geom_bar(fill = pal[["accent"]], colour = "white", linewidth = 0.3,
                 alpha = 0.9) +
        geom_text(stat = "count", aes(label = after_stat(count)),
                  vjust = -0.4, size = 3, colour = "#333333") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
                title    = "Distribution of Size Option Counts per Product",
                subtitle = "Number of size variants offered (e.g. XS, S, M, L = 4)",
                x = "Number of Size Options",
                y = "Number of Products",
                caption  = "Source: shein_imputed | Pipeline stage: 04.1_charts.r"
        ) +
        theme_shein

ggsave(file.path(OUTPUT_DIR, "chart_3_size_count_distribution.png"),
       p3, width = CHART_W, height = CHART_H, dpi = CHART_DPI)


# =============================================================================
# CHART 4 — Observed vs. imputed prices
# =============================================================================

message("[charts] 4 — Observed vs. imputed comparison ...")

if ("price_flag" %in% names(imp_dt)) {
        
        # Create a comparison dataset
        obs_imp <- imp_dt[price_flag %in% c("ok", "imputed") & !is.na(price)]
        obs_imp[, status := fifelse(price_flag == "imputed", "Imputed", "Observed")]
        obs_imp[, status := factor(status, levels = c("Observed", "Imputed"))]
        
        # Strip chart (jittered dots) + boxplot overlay
        p4 <- ggplot(obs_imp, aes(x = status, y = price, fill = status)) +
                geom_boxplot(width = 0.4, alpha = 0.25, outlier.shape = NA,
                             colour = "#555555", linewidth = 0.4) +
                geom_jitter(aes(colour = status), width = 0.12, size = 2.5,
                            alpha = 0.7, shape = 16) +
                scale_fill_manual(values = c("Observed" = pal[["observed"]],
                                             "Imputed"  = pal[["imputed"]])) +
                scale_colour_manual(values = c("Observed" = pal[["observed"]],
                                               "Imputed"  = pal[["imputed"]])) +
                scale_y_continuous(labels = scales::dollar_format()) +
                labs(
                        title    = "Observed vs. Imputed Prices",
                        subtitle = "Imputed values use grouped median (one_size group)",
                        x = NULL,
                        y = "Price (USD)",
                        caption  = "Source: shein_imputed | Pipeline stage: 04.1_charts.r"
                ) +
                theme_shein +
                theme(legend.position = "none")
        
} else {
        # Fallback: empty chart with note
        p4 <- ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "price_flag column not found — cannot compare",
                         size = 4, colour = "#999999") +
                theme_void()
}

ggsave(file.path(OUTPUT_DIR, "chart_4_observed_vs_imputed.png"),
       p4, width = CHART_W, height = CHART_H, dpi = CHART_DPI)


# =============================================================================
# CHART 5 — Price by size system (substantive insight)
# =============================================================================

message("[charts] 5 — Price by size system ...")

by_system <- imp_dt[!is.na(price) & !is.na(size_system)]

# Reorder size_system by median price for readability
sys_order <- by_system[, .(med = median(price)), by = size_system][order(med), size_system]
by_system[, size_system := factor(size_system, levels = sys_order)]

p5 <- ggplot(by_system, aes(x = size_system, y = price, fill = size_system)) +
        geom_boxplot(alpha = 0.5, outlier.shape = 21, outlier.size = 1.5,
                     outlier.fill = pal[["before"]], colour = "#555555",
                     linewidth = 0.4) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.5,
                    colour = pal[["after"]], shape = 16) +
        coord_flip() +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_fill_manual(values = rep(pal[["light"]], length(sys_order))) +
        labs(
                title    = "Price Distribution by Size System",
                subtitle = "Different product categories occupy distinct price bands",
                x = NULL,
                y = "Price (USD)",
                caption  = "Source: shein_imputed | Pipeline stage: 04.1_charts.r"
        ) +
        theme_shein +
        theme(legend.position = "none")

ggsave(file.path(OUTPUT_DIR, "chart_5_price_by_size_system.png"),
       p5, width = CHART_W, height = CHART_H, dpi = CHART_DPI)

# =============================================================================
# CHART 6 — Top 10 most expensive brands by median price
# =============================================================================

message("[charts] 6 — Top 10 most expensive brands ...")

brand_dt <- imp_dt[!is.na(price) & price > 0 & !is.na(brand) & brand != ""]

# Compute median price and count per brand
brand_summary <- brand_dt[, .(
        median_price = median(price),
        n = .N
), by = brand]

# Keep only the top 10 by median price
brand_summary <- brand_summary[order(-median_price)][1:min(10, .N)]

# Factor for plot ordering (least → most expensive, bottom → top on coord_flip)
brand_summary[, brand := factor(brand, levels = brand_summary[order(median_price), brand])]

# Filter individual prices to only the top-10 brands for the dot overlay
brand_dt_top <- brand_dt[brand %in% levels(brand_summary$brand)]
brand_dt_top[, brand := factor(brand, levels = levels(brand_summary$brand))]

p6 <- ggplot() +
        # Bar for median price
        geom_col(data = brand_summary,
                 aes(x = brand, y = median_price),
                 fill = pal[["after"]], alpha = 0.85, width = 0.5) +
        # Individual price dots
        geom_jitter(data = brand_dt_top,
                    aes(x = brand, y = price),
                    colour = pal[["before"]], size = 2.5, alpha = 0.7,
                    width = 0.12, shape = 16) +
        # Annotation: median + count
        geom_text(data = brand_summary,
                  aes(x = brand, y = median_price,
                      label = sprintf("$%.2f  (n=%d)", median_price, n)),
                  hjust = -0.12, size = 3, colour = "#333333", fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = scales::dollar_format(),
                           expand = expansion(mult = c(0, 0.25))) +
        labs(
                title    = "Top 10 Most Expensive Brands by Median Price",
                subtitle = "Bars = median price \u00b7 Dots = individual product prices \u00b7 n = product count",
                x = NULL,
                y = "Price (USD)",
                caption  = "Source: shein_imputed | Pipeline stage: 04.1_charts.r"
        ) +
        theme_shein

ggsave(file.path(OUTPUT_DIR, "chart_6_brand_price_ranking.png"),
       p6, width = CHART_W, height = CHART_H, dpi = CHART_DPI)


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== [05_charts] Charts saved ===\n")
chart_files <- list.files(OUTPUT_DIR, pattern = "^chart_.*\\.png$", full.names = TRUE)
for (f in chart_files) cat("  ", f, "\n")
cat("\n")

# message("[charts] 04.1_charts.r complete.")

# =============================================================================

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



###############################################################################
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

source("06_report.r")



