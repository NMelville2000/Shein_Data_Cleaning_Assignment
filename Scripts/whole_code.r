#00_config.r 

raw_file_path <- "Raw_data/shein_sample.csv"

if (!file.exists(raw_file_path)) {
        stop("The raw data file was not found. Please check that shein_sample.csv is inside the Raw_data folder.")
}

#01_ingest.r

#Load Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(tidyverse)
library(scales)
library(data.table)
library(openxlsx)
library(stringi)
library(lubridate)
library(purrr)

#Read in the raw dataset
raw_file <- fread(
        raw_file_path,
        encoding = "UTF-8",
        fill = TRUE,
        quote = "\""
)

raw_file <- as.data.frame(raw_file)

cat("Raw data loaded successfully.\n")
cat("Rows:", nrow(raw_file), "\n")
cat("Columns:", ncol(raw_file), "\n")


# View(raw_file)

#Validating the data compared to the Source Document
dim(raw_file)
head(raw_file)
names(raw_file)
class(raw_file)
str(raw_file)
summary(raw_file)


##################Creating a small excel file sample to view data######################
dir.create("small_sample", showWarnings = FALSE)

# Take the first 50 rows
sample_50 <- raw_file[1:min(111189, nrow(raw_file)), ]

# Fix invalid UTF-8 text in all character columns
sample_50 <- as.data.frame(
        lapply(sample_50, function(x) {
                if (is.character(x)) {
                        x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
                        x <- stri_replace_all_regex(x, "\\p{C}", "")
                }
                return(x)
        }),
        stringsAsFactors = FALSE
)


write.xlsx(
        sample_50,
        file = "shein_sample_50_rows.xlsx",
        overwrite = TRUE
)

cat("First 50 rows saved successfully in the Output folder.")

#02_diagnosis.r

#Exploratory Diagnostics for Full Raw Data Frame

# =====================================================
# 1. Basic setup
# =====================================================

df <- raw_file

dir.create("Output", showWarnings = FALSE)

output_file <- "Output/shein_exploratory_diagnostics.xlsx"

# Clean invalid text so Excel export does not fail
clean_text_for_excel <- function(x) {
        if (is.character(x)) {
                x <- stri_enc_toutf8(x, is_unknown_8bit = TRUE)
                x <- stri_replace_all_regex(x, "\\p{C}", "")
        }
        return(x)
}

clean_df_for_excel <- function(data) {
        as.data.frame(
                lapply(data, clean_text_for_excel),
                stringsAsFactors = FALSE
        )
}

# Helper to find expected D5 columns by name
find_col <- function(pattern) {
        hits <- names(df)[str_detect(tolower(names(df)), pattern)]
        if (length(hits) == 0) {
                return(NA_character_)
        } else {
                return(hits[1])
        }
}

product_col <- find_col("product.*name|product_name|title|name")
price_col <- find_col("^price$|price|sale.*price|current.*price")
discount_col <- find_col("discount|markdown|promo")
color_col <- find_col("color|colour")
size_col <- find_col("^size$|sizes")
category_col <- find_col("category|department|class")
material_col <- find_col("material|fabric")
product_url_col <- find_col("product.*url|url|link")
image_url_col <- find_col("image.*url|image|img")
scraped_at_col <- find_col("scraped|date|time|timestamp")



# =====================================================
# 2. Dataset overview
# =====================================================

overview <- data.frame(
        metric = c(
                "Total rows",
                "Total columns",
                "Total cells",
                "Exact duplicate rows",
                "Rows with at least one missing value",
                "Columns with at least one missing value"
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
#View(overview)

detected_columns <- data.frame(
        expected_field = c(
                "Product name",
                "Price",
                "Discount",
                "Color",
                "Size",
                "Category",
                "Material",
                "Product URL",
                "Image URL",
                "Scraped date or time"
        ),
        detected_column = c(
                product_col,
                price_col,
                discount_col,
                color_col,
                size_col,
                category_col,
                material_col,
                product_url_col,
                image_url_col,
                scraped_at_col
        )
)
#View(detected_columns)
#-------------------------------------------------------
# #===========================================================================
# #Confirming no instance of discount or dates are mentioned in "description"
# #using key value pairs and displaying every instance.
# 
# 
# ## Add row number so each extracted value can be matched back to the correct row
df <- df %>%
        mutate(row_number = row_number())

# Extract all key-value pairs from description
description_pairs <- map2_dfr(
        df$row_number,
        df$description,
        function(rn, desc_text) {
                
                if (is.na(desc_text) || desc_text == "") {
                        return(data.frame(
                                row_number = integer(0),
                                attribute_name = character(0),
                                attribute_value = character(0)
                        ))
                }
                
                matches <- str_match_all(
                        desc_text,
                        "'([^']+)'\\s*:\\s*'([^']*)'"
                )[[1]]
                
                if (nrow(matches) == 0) {
                        return(data.frame(
                                row_number = integer(0),
                                attribute_name = character(0),
                                attribute_value = character(0)
                        ))
                }
                
                data.frame(
                        row_number = rn,
                        attribute_name = matches[, 2],
                        attribute_value = matches[, 3],
                        stringsAsFactors = FALSE
                )
        }
)

# # View every extracted instance
# description_pairs

# Note, it s impossible to display all 1.38 million instances of all key-values
##############[ reached 'max' / getOption("max.print") -- omitted 1382900 rows ]


# =====================================================
# Summaries from description_pairs
# =====================================================

description_pairs <- description_pairs %>%
        mutate(
                attribute_name = stringr::str_squish(as.character(attribute_name)),
                attribute_value = stringr::str_squish(as.character(attribute_value))
        )

# Count of each unique attribute name
description_attribute_name_counts <- description_pairs %>%
        filter(!is.na(attribute_name), attribute_name != "") %>%
        count(attribute_name, sort = TRUE, name = "count")

# Count of each unique attribute value across all extracted values
description_attribute_value_counts <- description_pairs %>%
        filter(!is.na(attribute_value), attribute_value != "") %>%
        count(attribute_value, sort = TRUE, name = "count")

# Optional but very useful:
# Count of each unique attribute name + attribute value combination
description_name_value_counts <- description_pairs %>%
        filter(
                !is.na(attribute_name), attribute_name != "",
                !is.na(attribute_value), attribute_value != ""
        ) %>%
        count(attribute_name, attribute_value, sort = TRUE, name = "count")

#Confirmed no instance of discount or scraped-at exists in the description.




# =====================================================
# 3. Variable types and structure
# =====================================================

variable_types <- data.frame(
        variable = names(df),
        r_type = sapply(df, function(x) paste(class(x), collapse = ", ")),
        non_missing_count = sapply(df, function(x) sum(!(is.na(x) | x == ""))),
        missing_count = sapply(df, function(x) sum(is.na(x) | x == "")),
        unique_count = sapply(df, function(x) length(unique(x))),
        stringsAsFactors = FALSE
)

variable_types$missing_percent <- round(
        variable_types$missing_count / nrow(df) * 100,
        2
)

variable_types <- variable_types %>%
        arrange(desc(missing_percent))

# =====================================================
# 4. Missingness by variable
# =====================================================

missingness_by_variable <- data.frame(
        variable = names(df),
        missing_count = sapply(df, function(x) sum(is.na(x) | x == "")),
        non_missing_count = sapply(df, function(x) sum(!(is.na(x) | x == ""))),
        missing_percent = round(
                sapply(df, function(x) sum(is.na(x) | x == "")) / nrow(df) * 100,
                2
        ),
        stringsAsFactors = FALSE
) %>%
        arrange(desc(missing_percent))

# Missingness by row
missingness_by_row_summary <- data.frame(
        missing_values_per_row = rowSums(is.na(df) | df == ""),
        row_count = 1
) %>%
        group_by(missing_values_per_row) %>%
        summarise(row_count = sum(row_count), .groups = "drop") %>%
        arrange(desc(missing_values_per_row))

# =====================================================
# 5. Duplicate checks
# =====================================================

duplicate_summary <- data.frame(
        check = c(
                "Exact duplicate rows",
                "Duplicate product URLs",
                "Duplicate product names",
                "Duplicate image URLs"
        ),
        count = c(
                sum(duplicated(df)),
                if (!is.na(product_url_col)) sum(duplicated(df[[product_url_col]]) & !(is.na(df[[product_url_col]]) | df[[product_url_col]] == "")) else NA,
                if (!is.na(product_col)) sum(duplicated(df[[product_col]]) & !(is.na(df[[product_col]]) | df[[product_col]] == "")) else NA,
                if (!is.na(image_url_col)) sum(duplicated(df[[image_url_col]]) & !(is.na(df[[image_url_col]]) | df[[image_url_col]] == "")) else NA
        )
)

exact_duplicate_examples <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
exact_duplicate_examples <- head(exact_duplicate_examples, 50)

if (!is.na(product_url_col)) {
        duplicate_product_url_examples <- df[
                duplicated(df[[product_url_col]]) | duplicated(df[[product_url_col]], fromLast = TRUE),
        ]
        duplicate_product_url_examples <- duplicate_product_url_examples[
                !(is.na(duplicate_product_url_examples[[product_url_col]]) | duplicate_product_url_examples[[product_url_col]] == ""),
        ]
        duplicate_product_url_examples <- head(duplicate_product_url_examples, 50)
} else {
        duplicate_product_url_examples <- data.frame(note = "No product URL column detected.")
}


#Viewing dates and duplicates in images
# --- Step 1: Explode the rows (one URL per row) ---
df_exploded <- df %>%
        separate_rows(images, sep = ",\\s*")  # adjust sep if URLs are separated differently (e.g., "\\|" for pipe)

nrow(df_exploded)
# # Count of each unique URL in images
# exploded_df <- df_exploded %>%
#         filter(!is.na(images), images != "") %>%
#         count(images, sort = TRUE, name = "count")

# Count of each unique URL in images
exploded_count <- df_exploded %>%
        filter(!is.na(images), images != "") %>%
        mutate(images = str_remove_all(images, "[\\[\\]']")) %>%  # remove [ ] and '
        mutate(images = trimws(images)) %>%                        # remove any leftover whitespace
        filter(images != "") %>%                                   # drop any empty strings after cleaning
        count(images, sort = TRUE, name = "count")

View(exploded_df)
#Proves that scraping occurred at separate dates and was compiled without
#Deduplicating image URLs. Point to note to clean.

# Count records that contain duplicate image URLs
records_with_duplicates <- df %>%
        rowwise() %>%
        mutate(
                # Clean and split URLs
                url_list = list(
                        trimws(
                                str_remove_all(unlist(strsplit(images, ",\\s*")), "[\\[\\]']")
                        )
                ),
                # Check if any duplicates exist in the URL list
                has_duplicates = length(url_list) != length(unique(url_list))
        ) %>%
        ungroup() %>%
        filter(has_duplicates)

nrow(records_with_duplicates)
round(nrow(records_with_duplicates) / nrow(df) * 100, 2)


# =====================================================
# 6. Numeric and range checks
# =====================================================

numeric_columns <- names(df)[sapply(df, is.numeric)]

numeric_like_columns <- names(df)[
        str_detect(
                tolower(names(df)),
                "price|cost|amount|discount|rating|review|stock|quantity|qty|count|sales|weight"
        )
]

range_check_columns <- unique(c(numeric_columns, numeric_like_columns))

range_checks <- data.frame()
numeric_parse_issues <- data.frame()

for (col in range_check_columns) {
        
        x <- df[[col]]
        x_chr <- as.character(x)
        
        # Treat common messy missing values as NA
        messy_missing_values <- c(
                "",
                "NA",
                "N/A",
                "na",
                "n/a",
                "null",
                "NULL",
                "undefined",
                "Undefined",
                "UNDEFINED"
        )
        
        if (is.numeric(x)) {
                parsed_x <- x
        } else {
                parsed_x <- parse_number(
                        x_chr,
                        na = messy_missing_values
                )
        }
        
        # Find values that could not be parsed into numbers
        parse_issue_rows <- which(
                is.na(parsed_x) &
                        !(is.na(x_chr) | x_chr %in% messy_missing_values)
        )
        
        if (length(parse_issue_rows) > 0) {
                temp_issues <- data.frame(
                        variable = col,
                        row_number = parse_issue_rows,
                        original_value = x_chr[parse_issue_rows],
                        issue = "Value could not be parsed as a number"
                )
                
                numeric_parse_issues <- bind_rows(numeric_parse_issues, temp_issues)
        }
        
        parsed_x_non_missing <- parsed_x[!is.na(parsed_x)]
        
        if (length(parsed_x_non_missing) > 0) {
                
                q1 <- quantile(parsed_x_non_missing, 0.25, na.rm = TRUE)
                q3 <- quantile(parsed_x_non_missing, 0.75, na.rm = TRUE)
                iqr_value <- q3 - q1
                
                lower_bound <- q1 - 1.5 * iqr_value
                upper_bound <- q3 + 1.5 * iqr_value
                
                temp <- data.frame(
                        variable = col,
                        original_type = paste(class(x), collapse = ", "),
                        parsed_numeric_count = sum(!is.na(parsed_x)),
                        parsed_numeric_percent = round(sum(!is.na(parsed_x)) / nrow(df) * 100, 2),
                        min = min(parsed_x_non_missing, na.rm = TRUE),
                        q1 = q1,
                        median = median(parsed_x_non_missing, na.rm = TRUE),
                        mean = mean(parsed_x_non_missing, na.rm = TRUE),
                        q3 = q3,
                        max = max(parsed_x_non_missing, na.rm = TRUE),
                        negative_count = sum(parsed_x < 0, na.rm = TRUE),
                        zero_count = sum(parsed_x == 0, na.rm = TRUE),
                        possible_outlier_count = sum(parsed_x < lower_bound | parsed_x > upper_bound, na.rm = TRUE)
                )
                
                range_checks <- bind_rows(range_checks, temp)
        }
}

if (nrow(range_checks) == 0) {
        range_checks <- data.frame(note = "No numeric or numeric-like columns detected.")
}

if (nrow(numeric_parse_issues) == 0) {
        numeric_parse_issues <- data.frame(note = "No numeric parsing issues detected.")
}

# =====================================================
# 7. Price and discount specific checks
# =====================================================

price_discount_checks <- data.frame()

if (!is.na(price_col)) {
        
        price_num <- parse_number(as.character(df[[price_col]]))
        
        price_discount_checks <- bind_rows(
                price_discount_checks,
                data.frame(
                        check = "Price values that could not be parsed",
                        count = sum(is.na(price_num) & !(is.na(df[[price_col]]) | df[[price_col]] == ""))
                ),
                data.frame(
                        check = "Price values less than 0",
                        count = sum(price_num < 0, na.rm = TRUE)
                ),
                data.frame(
                        check = "Price values equal to 0",
                        count = sum(price_num == 0, na.rm = TRUE)
                )
        )
}

if (!is.na(discount_col)) {
        
        discount_num <- parse_number(as.character(df[[discount_col]]))
        
        price_discount_checks <- bind_rows(
                price_discount_checks,
                data.frame(
                        check = "Discount values that could not be parsed",
                        count = sum(is.na(discount_num) & !(is.na(df[[discount_col]]) | df[[discount_col]] == ""))
                ),
                data.frame(
                        check = "Discount values less than 0",
                        count = sum(discount_num < 0, na.rm = TRUE)
                ),
                data.frame(
                        check = "Discount values greater than 100",
                        count = sum(discount_num > 100, na.rm = TRUE)
                )
        )
}

if (nrow(price_discount_checks) == 0) {
        price_discount_checks <- data.frame(note = "No price or discount column detected.")
}

# =====================================================
# 8. URL checks
# =====================================================

url_checks <- data.frame()

check_url_column <- function(data, col_name, label) {
        
        if (is.na(col_name)) {
                return(data.frame(
                        url_type = label,
                        variable = NA,
                        total_non_missing = NA,
                        valid_url_count = NA,
                        invalid_url_count = NA,
                        duplicate_url_count = NA
                ))
        }
        
        url_values <- as.character(data[[col_name]])
        non_missing <- !(is.na(url_values) | url_values == "")
        valid_url <- str_detect(url_values, "^https?://")
        
        data.frame(
                url_type = label,
                variable = col_name,
                total_non_missing = sum(non_missing),
                valid_url_count = sum(valid_url & non_missing),
                invalid_url_count = sum(!valid_url & non_missing),
                duplicate_url_count = sum(duplicated(url_values) & non_missing)
        )
}

url_checks <- bind_rows(
        check_url_column(df, product_url_col, "Product URL"),
        check_url_column(df, image_url_col, "Image URL")
)

# =====================================================
# 9. Date and scrape time checks
# =====================================================

datetime_checks <- data.frame()

if (!is.na(scraped_at_col)) {
        
        scraped_values <- as.character(df[[scraped_at_col]])
        
        parsed_dates <- parse_date_time(
                scraped_values,
                orders = c(
                        "ymd HMS",
                        "ymd HM",
                        "ymd",
                        "mdy HMS",
                        "mdy HM",
                        "mdy",
                        "dmy HMS",
                        "dmy HM",
                        "dmy"
                ),
                quiet = TRUE
        )
        
        parsed_non_missing <- parsed_dates[!is.na(parsed_dates)]
        
        datetime_checks <- data.frame(
                variable = scraped_at_col,
                total_non_missing = sum(!(is.na(scraped_values) | scraped_values == "")),
                parsed_date_count = sum(!is.na(parsed_dates)),
                unparsed_date_count = sum(is.na(parsed_dates) & !(is.na(scraped_values) | scraped_values == "")),
                earliest_date = if (length(parsed_non_missing) > 0) as.character(min(parsed_non_missing)) else NA,
                latest_date = if (length(parsed_non_missing) > 0) as.character(max(parsed_non_missing)) else NA
        )
        
} else {
        datetime_checks <- data.frame(note = "No scraped date or timestamp column detected.")
}

# =====================================================
# 10. Text quality checks
# =====================================================

character_columns <- names(df)[sapply(df, is.character)]

text_quality_checks <- data.frame()

# Function to clean text before checking it
safe_text <- function(x) {
        x <- as.character(x)
        
        # Convert invalid text to UTF-8
        x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
        
        # Remove hidden/control characters that can break Excel or string checks
        x <- stringi::stri_replace_all_regex(x, "\\p{C}", "")
        
        return(x)
}

for (col in character_columns) {
        
        x <- safe_text(df[[col]])
        
        temp <- data.frame(
                variable = col,
                blank_count = sum(x == "", na.rm = TRUE),
                leading_or_trailing_space_count = sum(str_detect(x, "^\\s|\\s$"), na.rm = TRUE),
                multiple_space_count = sum(str_detect(x, "\\s{2,}"), na.rm = TRUE),
                contains_newline_count = sum(str_detect(x, "\\n|\\r"), na.rm = TRUE),
                very_long_text_count = sum(str_length(x) > 200, na.rm = TRUE),
                max_text_length = max(str_length(x), na.rm = TRUE),
                unique_count = length(unique(x))
        )
        
        text_quality_checks <- bind_rows(text_quality_checks, temp)
}

if (nrow(text_quality_checks) == 0) {
        text_quality_checks <- data.frame(note = "No character columns detected.")
}

# =====================================================
# 11. Category, color, size and material checks
# =====================================================

important_text_cols <- c(
        category_col,
        color_col,
        size_col,
        material_col
)

important_text_cols <- important_text_cols[!is.na(important_text_cols)]

level_summary <- data.frame()

for (col in important_text_cols) {
        
        temp <- data.frame(value = as.character(df[[col]])) %>%
                filter(!(is.na(value) | value == "")) %>%
                group_by(value) %>%
                summarise(count = n(), .groups = "drop") %>%
                arrange(desc(count)) %>%
                mutate(variable = col) %>%
                select(variable, value, count)
        
        temp <- head(temp, 50)
        
        level_summary <- bind_rows(level_summary, temp)
}

if (nrow(level_summary) == 0) {
        level_summary <- data.frame(note = "No category, color, size, or material columns detected.")
}

# =====================================================
# 12. Rows with possible issues
# =====================================================

issue_flags <- data.frame(row_number = seq_len(nrow(df)))

if (!is.na(price_col)) {
        price_num <- parse_number(as.character(df[[price_col]]))
        issue_flags$price_missing_or_invalid <- is.na(price_num)
        issue_flags$price_negative <- price_num < 0
}

if (!is.na(discount_col)) {
        discount_num <- parse_number(as.character(df[[discount_col]]))
        issue_flags$discount_over_100 <- discount_num > 100
        issue_flags$discount_negative <- discount_num < 0
}

if (!is.na(product_url_col)) {
        product_url_values <- as.character(df[[product_url_col]])
        issue_flags$product_url_invalid <- !str_detect(product_url_values, "^https?://") &
                !(is.na(product_url_values) | product_url_values == "")
}

if (!is.na(image_url_col)) {
        image_url_values <- as.character(df[[image_url_col]])
        issue_flags$image_url_invalid <- !str_detect(image_url_values, "^https?://") &
                !(is.na(image_url_values) | image_url_values == "")
}

issue_flags$issue_count <- rowSums(issue_flags[, setdiff(names(issue_flags), "row_number"), drop = FALSE], na.rm = TRUE)

problem_row_numbers <- issue_flags$row_number[issue_flags$issue_count > 0]

problem_rows_sample <- df[problem_row_numbers, ]
problem_rows_sample <- head(problem_rows_sample, 100)

if (nrow(problem_rows_sample) == 0) {
        problem_rows_sample <- data.frame(note = "No major problem rows flagged by the current checks.")
}

# =====================================================
# 13. Save diagnostics workbook
# =====================================================

wb <- createWorkbook()

add_sheet <- function(workbook, sheet_name, data) {
        addWorksheet(workbook, sheet_name)
        writeData(workbook, sheet_name, clean_df_for_excel(data))
        setColWidths(workbook, sheet_name, cols = 1:ncol(data), widths = "auto")
}

add_sheet(wb, "Overview", overview)
add_sheet(wb, "Detected Columns", detected_columns)
add_sheet(wb, "Description Attr Names", description_attribute_name_counts)
add_sheet(wb, "Description Attr Values", description_attribute_value_counts)
add_sheet(wb, "Description Name-Value", description_name_value_counts)
add_sheet(wb, "Variable Types", variable_types)
add_sheet(wb, "Missingness Variable", missingness_by_variable)
add_sheet(wb, "Missingness Row", missingness_by_row_summary)
add_sheet(wb, "Duplicate Summary", duplicate_summary)
add_sheet(wb, "Exact Duplicate Examples", exact_duplicate_examples)
add_sheet(wb, "Duplicate URL Examples", duplicate_product_url_examples)
add_sheet(wb, "Range Checks", range_checks)
add_sheet(wb, "Numeric Parse Issues", numeric_parse_issues)
add_sheet(wb, "Price Discount Checks", price_discount_checks)
add_sheet(wb, "URL Checks", url_checks)
add_sheet(wb, "Datetime Checks", datetime_checks)
add_sheet(wb, "Text Quality Checks", text_quality_checks)
add_sheet(wb, "Top Levels", level_summary)
add_sheet(wb, "Problem Rows Sample", problem_rows_sample)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Exploratory diagnostics created successfully.\n")
cat("Saved to:", output_file, "\n")

rm(list = ls())

