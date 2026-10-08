# ============================================================
# ANALYSIS HELPERS
# ============================================================

# ------------------------------------------------------------
# Concentration (HHI) and U.S. dependency
# ------------------------------------------------------------

concentration_level <- function(hhi) {
  factor(dplyr::case_when(
    hhi < HHI_MODERATE ~ "Low (diversified)",
    hhi < HHI_HIGH     ~ "Moderate",
    TRUE               ~ "High (concentrated)"
  ), levels = names(CONCENTRATION_COLOURS))
}

# `partner_totals` has one row per product (hs) x partner country with a
# `total`. Returns one row per product with its HHI, number of partners, top
# partner share and U.S. share ("U.S. dependency").
#
# `group_col` lets callers aggregate by something other than `hs`
# (e.g. steel categories).
summarise_concentration <- function(partner_totals, group_col = "hs") {
  partner_totals |>
    dplyr::filter(total > 0) |>
    dplyr::group_by(product = .data[[group_col]], country) |>
    dplyr::summarise(total = sum(total), .groups = "drop_last") |>
    dplyr::mutate(share = total / sum(total)) |>
    dplyr::summarise(
      hhi               = sum((share * 100)^2),
      n_partners        = dplyr::n(),
      top_partner       = country[which.max(share)],
      top_partner_share = max(share),
      us_dependency     = sum(share[country == "US"]),
      total             = sum(total),
      .groups = "drop"
    ) |>
    dplyr::mutate(concentration = concentration_level(hhi))
}

dependency_quadrant <- function(us_dep, hhi, avg_us_dep, avg_hhi) {
  dplyr::case_when(
    us_dep >  avg_us_dep & hhi >  avg_hhi ~
      "With limited options for diversification, reliance on U.S. markets remains high.",
    us_dep >  avg_us_dep & hhi <= avg_hhi ~
      "Although reliance on U.S. markets is high, opportunities for diversification remain.",
    us_dep <= avg_us_dep & hhi >  avg_hhi ~
      "Diversification may not be possible, but dependence on U.S. markets is low.",
    TRUE ~
      "There are fewer concerns with diversification or reliance on U.S. markets."
  )
}

# ------------------------------------------------------------
# Canada vs U.S. competitiveness ("Pillar 2")
# ------------------------------------------------------------

# `exp` / `imp` come from query_competitiveness_inputs().
calculate_competitiveness <- function(exp, imp) {
  bal <- dplyr::full_join(
    dplyr::select(exp, hs, ca_to_us = us_value),
    dplyr::select(imp, hs, us_to_ca = us_value),
    by = "hs"
  ) |>
    dplyr::mutate(
      ca_to_us = dplyr::coalesce(ca_to_us, 0),
      us_to_ca = dplyr::coalesce(us_to_ca, 0),
      bilateral_trade = ca_to_us + us_to_ca,
      bilateral_balance = (ca_to_us - us_to_ca) / pmax(bilateral_trade, 1),
      # 0 = Canada dominates the bilateral trade, 1 = U.S. dominates
      competitive_disadvantage = (1 - bilateral_balance) / 2
    )

  price <- dplyr::inner_join(
    dplyr::transmute(exp, hs, export_price = priced_value / priced_quantity),
    dplyr::transmute(imp, hs, import_price = priced_value / priced_quantity),
    by = "hs"
  ) |>
    dplyr::filter(is.finite(export_price), is.finite(import_price), import_price > 0) |>
    dplyr::mutate(pci = export_price / import_price)

  if (nrow(price) > 0) {
    lims <- stats::quantile(price$pci, c(0.05, 0.95), na.rm = TRUE)
    price$pci <- pmin(pmax(price$pci, lims[1]), lims[2])
  }

  dplyr::inner_join(bal, price, by = "hs")
}

competitiveness_quadrant <- function(disadv, pci, avg_disadv, avg_pci) {
  dplyr::case_when(
    disadv >  avg_disadv & pci >  avg_pci ~
      "The goods are both more expensive and have limited comparative advantage.",
    disadv >  avg_disadv & pci <= avg_pci ~
      "The goods are competitively priced but less specialized.",
    disadv <= avg_disadv & pci >  avg_pci ~
      "The goods are more expensive, but this does not prevent a strong comparative advantage.",
    TRUE ~
      "Canada's specialization in the good, combined with competitive pricing, strengthens its position."
  )
}

balance_description <- function(balance) {
  dplyr::case_when(
    balance >  0.3 ~ "Canada strongly dominates this bilateral trade",
    balance >  0.1 ~ "Canada has a moderate advantage in this bilateral trade",
    balance > -0.1 ~ "Bilateral trade between Canada and the U.S. is balanced",
    balance > -0.3 ~ "The U.S. has a moderate advantage in this bilateral trade",
    TRUE           ~ "The U.S. strongly dominates this bilateral trade"
  )
}

# ------------------------------------------------------------
# Labels
# ------------------------------------------------------------

lookup_name <- function(codes, dict) {
  out <- unname(dict[codes])
  ifelse(is.na(out) | out == "", codes, out)
}

# Adds a `partner` column describing each trade flow, according to the chosen
# breakdown. `direction` is "Exports" or "Imports" per row.
add_partner_label <- function(d, mode, lk) {
  country <- lookup_name(d$country, lk$countries)
  if (mode == "country") {
    d$partner <- country
    return(d)
  }
  prov <- ifelse(is.na(d$province), "Canada", lookup_name(d$province, lk$provinces))
  place <- country
  if (mode == "state") {
    is_state <- d$country == "US" & !is.na(d$state)
    place[is_state] <- paste0(lookup_name(d$state[is_state], lk$states), " (U.S.)")
  }
  d$partner <- ifelse(d$direction == "Exports",
                      paste(prov, "\u2192", place),
                      paste(place, "\u2192", prov))
  d
}

country_name <- function(code, lk) lookup_name(code, lk$countries)

product_label <- function(codes, lk, level) {
  p <- lk$products[lk$products$level == level, ]
  out <- p$label[match(codes, p$code)]
  ifelse(is.na(out), codes, out)
}

short_product_text <- function(codes, max_n = 3) {
  if (length(codes) <= max_n) return(paste(codes, collapse = ", "))
  paste0(paste(codes[seq_len(max_n)], collapse = ", "), " +", length(codes) - max_n, " more")
}

unit_label <- function(units, lk) {
  units <- unique(stats::na.omit(units))
  if (length(units) == 0) return("units")
  if (all(units == "KGM")) return("kg")
  if (length(units) == 1) {
    nm <- lk$units$name[match(units, lk$units$code)]
    return(if (is.na(nm)) units else tolower(nm))
  }
  "mixed units"
}

ym_label <- function(ym) {
  format(as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)), "%B %Y")
}

# Map HS2 chapter to HS section
hs_section <- function(hs) {
  n <- suppressWarnings(as.numeric(substr(hs, 1, 2)))
  dplyr::case_when(
    n >= 1  & n <= 5  ~ "I: Live animals",
    n >= 6  & n <= 14 ~ "II: Vegetable products",
    n == 15           ~ "III: Fats & oils",
    n >= 16 & n <= 24 ~ "IV: Prepared foodstuffs",
    n >= 25 & n <= 27 ~ "V: Mineral products",
    n >= 28 & n <= 38 ~ "VI: Chemicals",
    n >= 39 & n <= 40 ~ "VII: Plastics & rubber",
    n >= 41 & n <= 43 ~ "VIII: Hides & skins",
    n >= 44 & n <= 46 ~ "IX: Wood",
    n >= 47 & n <= 49 ~ "X: Pulp & paper",
    n >= 50 & n <= 63 ~ "XI: Textiles",
    n >= 64 & n <= 67 ~ "XII: Footwear & headgear",
    n >= 68 & n <= 70 ~ "XIII: Stone & glass",
    n == 71           ~ "XIV: Precious stones & metals",
    n >= 72 & n <= 83 ~ "XV: Base metals",
    n >= 84 & n <= 85 ~ "XVI: Machinery",
    n >= 86 & n <= 89 ~ "XVII: Vehicles",
    n >= 90 & n <= 92 ~ "XVIII: Instruments",
    n == 93           ~ "XIX: Arms",
    n >= 94 & n <= 96 ~ "XX: Miscellaneous",
    n == 97           ~ "XXI: Art & antiques",
    n >= 98 & n <= 99 ~ "XXII: Special",
    TRUE              ~ "Other"
  )
}

# ------------------------------------------------------------
# Time & grouping
# ------------------------------------------------------------

add_period <- function(d, freq) {
  m <- switch(freq,
    month   = d$month,
    quarter = (d$month - 1L) %/% 3L * 3L + 1L,
    year    = rep(1L, nrow(d))
  )
  d$period <- as.Date(sprintf("%d-%02d-01", d$year, m))
  d
}

period_text <- function(dates, freq) {
  switch(freq,
    month   = format(dates, "%b %Y"),
    quarter = paste0(format(dates, "%Y"), " Q", (as.integer(format(dates, "%m")) - 1) %/% 3 + 1),
    year    = format(dates, "%Y")
  )
}

# Keep the `n` largest values of `col` (by total metric) and fold the rest
# into "Other". Returns `d` with `col` as a factor ordered by size.
lump_top_n <- function(d, col, n, weight = "metric") {
  totals <- tapply(d[[weight]], d[[col]], sum, na.rm = TRUE)
  totals <- sort(totals[totals > 0], decreasing = TRUE)
  keep <- names(totals)[seq_len(min(n, length(totals)))]
  lab <- ifelse(d[[col]] %in% keep, d[[col]], "Other")
  levs <- c(keep, if (any(lab == "Other")) "Other")
  d[[col]] <- factor(lab, levels = levs)
  d
}

series_colours <- function(levels) {
  cols <- rep(SERIES_COLOURS, length.out = length(levels))
  cols[levels == "Other"] <- OTHER_COLOUR
  setNames(cols, levels)
}

# ------------------------------------------------------------
# Number formatting
# ------------------------------------------------------------

# Short human-readable numbers: 1234567 -> "1.2M" (with optional prefix "$")
fmt_short <- function(x, prefix = "") {
  a <- abs(x)
  # thresholds sit just below each power so 999,960 shows as "1.0M", not "1000.0K"
  div <- ifelse(a >= 999.95e9, 1e12,
         ifelse(a >= 999.95e6, 1e9,
         ifelse(a >= 999.95e3, 1e6,
         ifelse(a >= 999.95,   1e3, 1))))
  suffix <- c("T", "B", "M", "K", "")[match(div, c(1e12, 1e9, 1e6, 1e3, 1))]
  num <- sprintf(ifelse(div == 1, "%.0f", "%.1f"), a / div)
  ifelse(is.na(x), "\u2013", paste0(ifelse(x < 0, "-", ""), prefix, num, suffix))
}
fmt_dollars <- function(x) fmt_short(x, "$")
fmt_number  <- function(x) fmt_short(x)
fmt_metric <- function(x, metric, unit = "") {
  if (metric == "value") fmt_dollars(x) else paste(fmt_number(x), unit)
}
fmt_pct <- function(x, accuracy = 1) {
  digits <- max(0, -floor(log10(accuracy)))
  ifelse(is.na(x), "\u2013", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

# Pick a readable scale for an axis: returns divisor and a word
axis_scale <- function(x) {
  m <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  if (m >= 1e9) list(div = 1e9, word = "billions")
  else if (m >= 1e6) list(div = 1e6, word = "millions")
  else if (m >= 1e3) list(div = 1e3, word = "thousands")
  else list(div = 1, word = "")
}

metric_axis_title <- function(metric, unit, scale) {
  if (metric == "value") {
    if (nzchar(scale$word)) paste0("CAD $ (", scale$word, ")") else "CAD $"
  } else {
    if (nzchar(scale$word)) paste0(unit, " (", scale$word, ")") else unit
  }
}

with_direction <- function(d, direction) {
  d$direction <- rep(direction, nrow(d))
  d
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Shorten long product descriptions for legends
short_label <- function(x, width = 48) {
  ifelse(nchar(x) > width, paste0(substr(x, 1, width - 1), "\u2026"), x)
}
