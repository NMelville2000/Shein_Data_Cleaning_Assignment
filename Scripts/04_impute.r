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

