# ============================================================
# Bank of Canada, Monetary Policy Report (October 2026), Chart 2
# "Exports expanded despite weakness in tariff-affected products"
# Percentage change from 2024, monthly data
# ------------------------------------------------------------
# Rebuilds the chart from the CIMT data in this repository. Bars are each
# product group's contribution (percentage points) to the change in goods
# exports relative to 2024; the line is the change in total goods exports
# (excluding precious metals).
#
# Differences from the Bank's chart:
#   * CIMT data here are in current dollars and NOT seasonally adjusted. By
#     default (BASE = "seasonal") each product group is seasonally adjusted
#     with simple monthly factors estimated over SEASONAL_YEARS, then compared
#     with its 2024 average, as in the Bank's chart. BASE = "same_month"
#     compares each month with the same month of 2024 instead.
#   * The Bank's series are seasonally adjusted (and may be in real terms), so
#     the swings here are larger, but the pattern is the same.
#   * "Products affected by tariffs" is approximated from the U.S. sectoral
#     (Section 232) tariffs, using the HS codes in TARIFF_HS below. The Bank's
#     exact product list is not published.
#
# Prerequisite: the prepared database (run launch.R or
# scripts/prepare_data.R once). Run from the repository root:
#   Rscript analysis/boc_mpr_2026_10_chart2.R
# Output: analysis/output/boc_mpr_2026_10_chart2.png (+ .csv of the data)
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
})

DB_PATH  <- file.path("data", "processed", "cimt.duckdb")
OUT_DIR  <- file.path("analysis", "output")
BASE     <- "seasonal"            # or "same_month"
SEASONAL_YEARS <- c(2010:2019, 2022:2024)   # skips the 2020-21 pandemic years
EXPORTS  <- "tot_exp"             # total exports (incl. re-exports); "dom_exp" for domestic
FIRST_MONTH <- as.Date("2024-11-01")

# ---- Product groups (matched on the start of the HS6 code) -------------------

# Precious metals: excluded from goods exports, as in the Bank's chart
PRECIOUS_HS <- c("7106", "7107", "7108", "7109", "7110", "7111", "7112")

# Energy products: HS chapter 27 (mineral fuels, oils, electricity)
ENERGY_HS <- c("27")

# Products hit by U.S. sectoral tariffs (approximation; edit as needed)
TARIFF_HS <- c(
  "72", "73",                                   # steel and steel products
  "76",                                         # aluminum
  "7403", "7404", "7405", "7406", "7407", "7408",
  "7409", "7410", "7411", "7412", "7413",       # semi-finished copper
  "8703", "8704", "8706", "8707", "8708",       # light vehicles and parts
  "8701",                                       # heavy trucks (tractors)
  "4407", "4409", "4418",                       # softwood lumber, wood products
  "9401", "9403"                                # upholstered furniture, cabinets
)

starts_with_any <- function(code, prefixes) {
  Reduce(`|`, lapply(prefixes, function(p) startsWith(code, p)))
}

# ---- Data --------------------------------------------------------------------

if (!file.exists(DB_PATH)) {
  stop("Prepared data not found. Run launch.R (or scripts/prepare_data.R) first.")
}
con <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
monthly <- dbGetQuery(con, "
  SELECT year, month, hs, SUM(value) AS value
  FROM trade
  WHERE flow = ? AND level = 'HS6' AND year >= ?
  GROUP BY year, month, hs",
  params = list(EXPORTS, as.integer(min(SEASONAL_YEARS, 2024))))
dbDisconnect(con, shutdown = TRUE)

monthly <- monthly |>
  mutate(
    date  = as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(month))),
    group = case_when(
      starts_with_any(hs, PRECIOUS_HS) ~ "Precious metals",
      starts_with_any(hs, ENERGY_HS)   ~ "Energy products",
      starts_with_any(hs, TARIFF_HS)   ~ "Products affected by tariffs",
      TRUE                             ~ "All other products"
    )
  ) |>
  filter(group != "Precious metals") |>
  group_by(date, group) |>
  summarise(value = sum(value), .groups = "drop")

# ---- Change from 2024 --------------------------------------------------------

monthly <- monthly |>
  mutate(year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))

if (BASE == "seasonal") {
  # Seasonal factor per group and calendar month: average ratio of the month
  # to its year's monthly mean
  factors <- monthly |>
    filter(year %in% SEASONAL_YEARS) |>
    group_by(group, year) |>
    mutate(ratio = value / mean(value)) |>
    group_by(group, month) |>
    summarise(factor = mean(ratio), .groups = "drop")
  monthly <- monthly |>
    left_join(factors, by = c("group", "month")) |>
    mutate(value = value / factor)

  base_group <- monthly |> filter(year == 2024) |>
    group_by(group) |> summarise(base = mean(value), .groups = "drop")
  base_total <- sum(base_group$base)
  chart <- monthly |>
    left_join(base_group, by = "group") |>
    mutate(base_total = base_total)
} else {
  base_2024  <- filter(monthly, year == 2024)
  base_group <- base_2024 |> select(month, group, base = value)
  base_total <- base_2024 |> group_by(month) |> summarise(base_total = sum(value), .groups = "drop")
  chart <- monthly |>
    left_join(base_group, by = c("month", "group")) |>
    left_join(base_total, by = "month")
}

chart <- chart |>
  filter(date >= FIRST_MONTH) |>
  mutate(contribution = 100 * (value - base) / base_total)

total <- chart |>
  group_by(date) |>
  summarise(change = sum(contribution), .groups = "drop")

group_levels <- c("Products affected by tariffs", "Energy products", "All other products")
chart$group <- factor(chart$group, levels = group_levels)

# ---- Plot --------------------------------------------------------------------

colours <- c("Products affected by tariffs" = "#68b5dd",
             "Energy products"              = "#ffd200",
             "All other products"           = "#12b562")

last_obs <- format(max(chart$date), "%B %Y")

p <- ggplot() +
  geom_col(data = chart, aes(date, contribution, fill = group),
           width = 25, position = position_stack()) +
  geom_line(data = total, aes(date, change, linetype = "Goods exports"),
            colour = "black", linewidth = 0.9) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  scale_fill_manual(values = colours, breaks = group_levels, name = NULL) +
  scale_linetype_manual(values = c("Goods exports" = "solid"), name = NULL) +
  scale_x_date(breaks = seq(as.Date("2025-01-01"), max(chart$date), by = "6 months"),
               date_labels = "%b %Y", expand = expansion(add = 20)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(-20, 20, 5)) +
  labs(
    title    = "Chart 2: Exports expanded despite weakness in tariff-affected products",
    subtitle = paste0("Percentage change from 2024, monthly data",
                      if (BASE == "same_month") " (vs. same month of 2024, not seasonally adjusted)"
                      else " (seasonally adjusted)"),
    x = NULL, y = NULL,
    caption  = paste0(
      "Note: Goods exports excludes precious metals products. Products affected by tariffs ",
      "is approximated from U.S. sectoral tariffs.\n",
      "Sources: Statistics Canada (CIMT, ", if (EXPORTS == "tot_exp") "total" else "domestic",
      " exports, customs basis) and calculations\nLast observation: ", last_obs)
  ) +
  guides(fill = guide_legend(ncol = 2, order = 1), linetype = guide_legend(order = 2)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", colour = "#2b2d4f"),
    plot.subtitle      = element_text(colour = "#2b2d4f"),
    plot.caption       = element_text(hjust = 0, colour = "grey45", size = 9),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.box         = "horizontal"
  )

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(OUT_DIR, "boc_mpr_2026_10_chart2.png"), p, width = 8, height = 6.5, dpi = 150, bg = "white")
write.csv(
  tidyr::pivot_wider(select(chart, date, group, contribution), names_from = group, values_from = contribution) |>
    left_join(rename(total, `Goods exports` = change), by = "date"),
  file.path(OUT_DIR, "boc_mpr_2026_10_chart2.csv"), row.names = FALSE
)
message("Saved chart to ", file.path(OUT_DIR, "boc_mpr_2026_10_chart2.png"))
