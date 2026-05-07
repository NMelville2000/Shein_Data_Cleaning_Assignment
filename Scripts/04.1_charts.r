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

message("[charts] 04.1_charts.r complete.")

