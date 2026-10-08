# ============================================================
# DATA ACCESS
# ------------------------------------------------------------
# All reads from the DuckDB database built by scripts/prepare_data.R.
# Heavy aggregation is pushed into SQL so the app never has to hold the full
# 30-year dataset in memory.
# ============================================================

open_trade_db <- function(path = DB_PATH) {
  if (!file.exists(path)) return(NULL)
  # duckdb >= 1.5 asks where to keep its extension cache; the app needs none
  drv <- if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    duckdb::duckdb(shared_home = FALSE)
  } else {
    duckdb::duckdb()
  }
  con <- DBI::dbConnect(drv, dbdir = path, read_only = TRUE)
  ok <- tryCatch(
    nrow(DBI::dbGetQuery(con, "SELECT 1 FROM meta WHERE key = 'lookups_built'")) > 0,
    error = function(e) FALSE
  )
  if (!ok) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    return(NULL)
  }
  con
}

load_lookups <- function(con) {
  q <- function(sql) DBI::dbGetQuery(con, sql)

  products <- q("SELECT level, code, desc_en, first_year, last_year FROM products ORDER BY level, code")
  products$label <- paste0(products$code, " - ", products$desc_en)

  coverage  <- q("SELECT flow, first_ym, last_ym FROM coverage")
  countries <- q("SELECT code, name FROM countries")
  provinces <- q("SELECT code, name FROM provinces ORDER BY name")
  states    <- q("SELECT code, name FROM states")

  list(
    products  = products,
    countries = replace(setNames(countries$name, countries$code),
                        names(COUNTRY_SHORT_NAMES), COUNTRY_SHORT_NAMES),
    provinces = setNames(provinces$name, provinces$code),
    states    = setNames(states$name, states$code),
    units     = q("SELECT code, name, to_kg FROM units"),
    coverage  = coverage,
    first_year = as.integer(min(coverage$first_ym) %/% 100),
    last_year  = as.integer(max(coverage$last_ym) %/% 100),
    last_ym    = as.integer(max(coverage$last_ym))
  )
}

# ------------------------------------------------------------
# Filters
# ------------------------------------------------------------
# A filter `f` is a plain list built by the server from the sidebar inputs:
#   level            "HS2" | "HS6"
#   export_flow      "dom_exp" | "tot_exp"
#   years            c(from, to)
#   province         NULL (all of Canada) or a province code
#   exclude_internal TRUE/FALSE  (drop Canada as a partner country)
#   metric           "value" | "quantity"

sql_where <- function(con, f, flow, hs = NULL, level = f$level) {
  qs <- function(x) DBI::dbQuoteString(con, x)
  w <- c(
    sprintf("t.flow = %s", qs(flow)),
    sprintf("t.level = %s", qs(level)),
    sprintf("t.year BETWEEN %d AND %d", as.integer(f$years[1]), as.integer(f$years[2]))
  )
  # Total exports are only published for Canada as a whole
  if (!is.null(f$province) && flow != "tot_exp") {
    w <- c(w, sprintf("t.province = %s", qs(f$province)))
  }
  if (isTRUE(f$exclude_internal)) w <- c(w, "t.country <> 'CA'")
  if (!is.null(hs)) {
    if (length(hs) == 0) hs <- "__none__"
    w <- c(w, sprintf("t.hs IN (%s)", paste(qs(hs), collapse = ", ")))
  }
  if (f$metric == "quantity") w <- c(w, "t.quantity IS NOT NULL")
  paste(w, collapse = " AND ")
}

# Metric expression: value in CAD, or quantity with weights converted to kg
sql_metric <- function(f) {
  if (f$metric == "quantity") "t.quantity * COALESCE(u.to_kg, 1)" else "t.value"
}
sql_from <- "trade t LEFT JOIN units u ON t.unit = u.code"

# Total by product x partner country for every product -- the input to HHI and
# U.S.-dependency calculations.
query_partner_totals <- function(con, f, flow) {
  sql <- sprintf("
    SELECT t.hs, t.country, SUM(%s) AS total
    FROM %s
    WHERE %s
    GROUP BY t.hs, t.country",
    sql_metric(f), sql_from, sql_where(con, f, flow))
  DBI::dbGetQuery(con, sql)
}

# Per-product sums needed for the Canada-vs-U.S. competitiveness view
# (always HS6 and value-based).
query_competitiveness_inputs <- function(con, f, flow) {
  f$metric <- "value"
  sql <- sprintf("
    SELECT t.hs,
           SUM(t.value) FILTER (WHERE t.country = 'US')                      AS us_value,
           SUM(t.value) FILTER (WHERE t.quantity > 0 AND t.value > 0)        AS priced_value,
           SUM(t.quantity) FILTER (WHERE t.quantity > 0 AND t.value > 0)     AS priced_quantity
    FROM trade t
    WHERE %s
    GROUP BY t.hs",
    sql_where(con, f, flow, level = "HS6"))
  DBI::dbGetQuery(con, sql)
}

# Detailed monthly rows for a set of products.
query_detail <- function(con, f, flow, hs, level = f$level) {
  sql <- sprintf("
    SELECT t.year, t.month, t.hs, t.country, t.province, t.state,
           CASE WHEN u.to_kg IS NOT NULL THEN 'KGM' ELSE t.unit END AS metric_unit,
           SUM(t.value) AS value,
           SUM(t.quantity * COALESCE(u.to_kg, 1)) AS quantity,
           SUM(%s) AS metric
    FROM %s
    WHERE %s
    GROUP BY ALL",
    sql_metric(f), sql_from, sql_where(con, f, flow, hs = hs, level = level))
  out <- DBI::dbGetQuery(con, sql)
  out$year  <- as.integer(out$year)
  out$month <- as.integer(out$month)
  out
}
