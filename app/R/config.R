# ============================================================
# CONFIGURATION
# ============================================================

APP_TITLE <- "Canadian Trade Explorer"

# Repository paths (the app runs with app/ as its working directory)
REPO_ROOT    <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
DB_PATH      <- Sys.getenv("TRADE_EXPLORER_DB",
                           file.path(REPO_ROOT, "data", "processed", "cimt.duckdb"))
STEEL_CSV    <- file.path(REPO_ROOT, "data", "reference", "steel_categories.csv")

# Default product when the app first opens
DEFAULT_PRODUCT <- c(HS2 = "72", HS6 = NA)   # 72 = Iron and steel

# Largest number of individually-coloured partners in a chart; the rest are
# grouped as "Other".
MAX_TOP_N <- 8

# Categorical series colours, assigned in this fixed order (validated for
# colour-vision deficiency). "Other" is always grey.
SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                    "#e87ba4", "#008300", "#4a3aa7", "#e34948")
OTHER_COLOUR   <- "#a3a29d"
MUTED_COLOUR   <- "#c9c8c2"
HIGHLIGHT_COLOUR <- "#2a78d6"
ACCENT_COLOUR  <- "#eb6834"

# Ordinal ramp (light -> dark blue) for low / moderate / high concentration
CONCENTRATION_COLOURS <- c("Low (diversified)"    = "#86b6ef",
                           "Moderate"             = "#2a78d6",
                           "High (concentrated)"  = "#104281")

# Herfindahl-Hirschman Index thresholds
HHI_MODERATE <- 1500
HHI_HIGH     <- 2500

# Shorter display names for partners whose official names are long
COUNTRY_SHORT_NAMES <- c(US = "United States")

FLOW_LABELS <- c(imp = "Imports", dom_exp = "Domestic exports", tot_exp = "Total exports")

PARTNER_MODES <- c("Countries"                     = "country",
                   "Countries, by province"        = "province",
                   "U.S. states, by province"      = "state")

FREQUENCIES <- c("Monthly" = "month", "Quarterly" = "quarter", "Yearly" = "year")
