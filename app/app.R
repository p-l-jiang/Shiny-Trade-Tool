# ============================================================
# CANADIAN TRADE EXPLORER
# ------------------------------------------------------------
# Shiny app for exploring Statistics Canada's Canadian International
# Merchandise Trade (CIMT) data.
#
# Start it with the launcher in the repository root ("Start Trade Explorer"),
# which installs packages and prepares the data first. The rest of the code
# lives in app/R/ and is loaded automatically by Shiny:
#   config.R       settings, colours, paths
#   data_access.R  queries against the prepared DuckDB database
#   analysis.R     HHI, U.S. dependency, competitiveness, labels, formatting
#   plots.R        chart builders
#   mod_*.R        tab modules (explorer, steel, other)
#   app_ui.R / app_server.R  page layout and wiring
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
})

con <- open_trade_db()

if (is.null(con)) {
  shinyApp(no_data_ui(), function(input, output, session) {})
} else {
  onStop(function() DBI::dbDisconnect(con, shutdown = TRUE))
  lk <- load_lookups(con)
  steel_categories <- load_steel_categories()
  shinyApp(app_ui(lk, steel_categories), app_server(con, lk, steel_categories))
}
