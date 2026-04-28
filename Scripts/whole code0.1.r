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

df <- raw_file %>% mutate(row_number = row_number())

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

variable_types <- data.frame(
        variable          = names(df),
        r_type            = sapply(df, \(x) paste(class(x), collapse = ", ")),
        non_missing_count = sapply(df, \(x) sum(!(is.na(x) | x == ""))),
        missing_count     = sapply(df, \(x) sum(is.na(x)  | x == "")),
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
        missing_count = sapply(df, \(x) sum(is.na(x) | x == "")),
        non_missing   = sapply(df, \(x) sum(!(is.na(x) | x == ""))),
        missing_pct   = round(
                sapply(df, \(x) sum(is.na(x) | x == "")) / nrow(df) * 100, 2
        ),
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

# Image URL duplication — explode comma-separated lists
message("[diagnosis] Checking image URL duplication ...")

exploded_images <- df %>%
        separate_rows(images, sep = ",\\s*") %>%
        mutate(images = trimws(str_remove_all(images, "[\\[\\]']"))) %>%
        filter(!is.na(images), images != "")

image_url_counts <- exploded_images %>%
        count(images, sort = TRUE, name = "count")

records_with_dup_images <- df %>%
        rowwise() %>%
        mutate(
                url_list      = list(trimws(str_remove_all(unlist(strsplit(images, ",\\s*")), "[\\[\\]']"))),
                has_duplicates = length(url_list) != length(unique(url_list))
        ) %>%
        ungroup() %>%
        filter(has_duplicates)

image_dup_summary <- data.frame(
        metric = c(
                "Records with at least one duplicate image URL",
                "Pct of total records"
        ),
        value = c(
                nrow(records_with_dup_images),
                paste0(round(nrow(records_with_dup_images) / nrow(df) * 100, 2), "%")
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

message("[diagnosis] Workbook saved → ", DIAGNOSTICS_FILE)

