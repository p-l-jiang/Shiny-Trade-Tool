# ============================================================
# PREPARE CIMT DATA
# ------------------------------------------------------------
# Turns the Statistics Canada CIMT zip archives in data/raw/ into a single
# compact DuckDB database (data/processed/cimt.duckdb) that the app reads.
#
#   * Only the HS2 and HS6 tables are kept (HS8/HS10 are not used by the app).
#   * Imports, domestic exports and total exports are all loaded.
#   * The fixed-width description files (.TXT) are parsed into lookup tables.
#   * Work is incremental: a zip is only (re)processed when it is new or has
#     changed since the last run, so dropping a new year's zip into data/raw/
#     and relaunching is all it takes to update the data.
#
# Usage (from the repository root):
#   Rscript scripts/prepare_data.R            # process new / changed zips
#   Rscript scripts/prepare_data.R --rebuild  # start again from scratch
# ============================================================

# Bump this whenever the database layout below changes; older databases are
# then rebuilt automatically on the next launch.
CIMT_SCHEMA_VERSION <- "1"

# CIMT table numbers we load. Others (014 = HS10 imports, 016/017 = HS8
# exports) are skipped.
CIMT_TABLES <- data.frame(
  table        = c("015",  "022",  "018",     "020",     "019",     "021"),
  flow         = c("imp",  "imp",  "dom_exp", "dom_exp", "tot_exp", "tot_exp"),
  level        = c("HS6",  "HS2",  "HS6",     "HS2",     "HS6",     "HS2"),
  has_province = c(TRUE,   TRUE,   TRUE,      TRUE,      FALSE,     FALSE),
  has_quantity = c(TRUE,   FALSE,  TRUE,      FALSE,     TRUE,      FALSE),
  stringsAsFactors = FALSE
)

# Archive name prefix -> flow code
CIMT_ARCHIVES <- c(Imp = "imp", Dom_Exp = "dom_exp", Tot_Exp = "tot_exp")

# Units of measure that are weights, with their factor to kilograms. Every
# other unit (number, litres, m2, ...) is left as reported.
MASS_TO_KG <- c(
  MGM = 1e-6,     # milligrams
  GRM = 1e-3,     # grams
  CTM = 2e-4,     # carats
  HGM = 0.1,      # hectograms
  KGM = 1,        # kilograms
  KNS = 1,        # kilograms of named substance
  KSD = 1,        # air-dry kilograms
  LBR = 0.453592, # pounds
  DTN = 100,      # decitons
  TNE = 1000,     # metric tonnes
  TSD = 1000,     # air-dry metric tonnes
  KTN = 1e6       # kilotons
)

prepare_data <- function(root = ".", rebuild = FALSE, verbose = TRUE) {
  stopifnot(requireNamespace("DBI", quietly = TRUE),
            requireNamespace("duckdb", quietly = TRUE))

  say <- function(...) if (verbose) message(...)

  raw_dir  <- file.path(root, "data", "raw")
  out_dir  <- file.path(root, "data", "processed")
  db_path  <- file.path(out_dir, "cimt.duckdb")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  zips <- find_cimt_zips(raw_dir)
  if (nrow(zips) == 0) {
    stop("No CIMT zip files found in ", normalizePath(raw_dir, mustWork = FALSE),
         ".\nDownload them from Statistics Canada (see README) and place them there.",
         call. = FALSE)
  }

  if (rebuild) remove_db(db_path)

  con <- open_db_for_writing(db_path, say)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  create_schema(con)

  # ---- Work out which archives need (re)processing ------------------------
  done <- DBI::dbGetQuery(con, "SELECT archive, size, mtime FROM sources")
  zips$key <- paste(zips$archive, zips$size, round(zips$mtime))
  done_key <- paste(done$archive, done$size, round(done$mtime))
  todo <- zips[!zips$key %in% done_key, , drop = FALSE]

  # Archives that were removed from data/raw/: drop their rows too
  gone <- setdiff(done$archive, zips$archive)
  for (g in gone) {
    info <- parse_archive_name(g)
    say("Removing data for deleted archive ", g)
    DBI::dbExecute(con, "DELETE FROM trade WHERE flow = ? AND year = ?",
                   params = list(info$flow, info$year))
    DBI::dbExecute(con, "DELETE FROM sources WHERE archive = ?", params = list(g))
  }

  if (nrow(todo) == 0 && length(gone) == 0 && lookups_exist(con)) {
    say("Data is up to date (", nrow(zips), " archives).")
    return(invisible(db_path))
  }

  if (nrow(todo) > 0) {
    say("")
    say("Preparing ", nrow(todo), " of ", nrow(zips), " data archive(s).")
    if (nrow(todo) > 10) {
      say("This is a one-time step and usually takes 5-15 minutes. Please leave this window open.")
    }
    say("")
  }

  work_dir <- file.path(tempdir(), "cimt_extract")
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # Process oldest first so the database is laid out chronologically
  todo <- todo[order(todo$year, todo$flow), , drop = FALSE]
  for (i in seq_len(nrow(todo))) {
    z <- todo[i, ]
    t0 <- Sys.time()
    if (verbose) {
      message(sprintf("  [%2d/%d] %-30s", i, nrow(todo), z$archive), appendLF = FALSE)
    }
    n_rows <- load_archive(con, z, work_dir)
    if (verbose) {
      message(sprintf(" %12s rows  (%.0fs)",
                      format(n_rows, big.mark = ","),
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  }

  say("")
  say("Building product, country and region lookup tables...")
  build_lookups(con, zips, work_dir)

  DBI::dbExecute(con, "CHECKPOINT")
  say("Done. Data saved to ", normalizePath(db_path))
  invisible(db_path)
}

# ------------------------------------------------------------
# Archive discovery
# ------------------------------------------------------------

parse_archive_name <- function(name) {
  m <- regmatches(name, regexec("^CIMT-CICM_(Imp|Dom_Exp|Tot_Exp)_(\\d{4})\\.zip$",
                                name, ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  kind <- names(CIMT_ARCHIVES)[match(tolower(m[2]), tolower(names(CIMT_ARCHIVES)))]
  list(flow = unname(CIMT_ARCHIVES[kind]), year = as.integer(m[3]))
}

find_cimt_zips <- function(raw_dir) {
  files <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE, ignore.case = TRUE)
  info  <- lapply(basename(files), parse_archive_name)
  keep  <- !vapply(info, is.null, logical(1))
  files <- files[keep]
  info  <- info[keep]
  fi <- file.info(files)
  data.frame(
    path    = files,
    archive = basename(files),
    flow    = vapply(info, `[[`, "", "flow"),
    year    = vapply(info, `[[`, 0L, "year"),
    size    = fi$size,
    mtime   = as.numeric(fi$mtime),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Database setup
# ------------------------------------------------------------

remove_db <- function(db_path) {
  unlink(c(db_path, paste0(db_path, ".wal")))
}

open_db_for_writing <- function(db_path, say) {
  con <- tryCatch(
    DBI::dbConnect(duckdb_driver(), dbdir = db_path),
    error = function(e) {
      # Usually a database written by a different duckdb version, or the app
      # still running. The database is fully rebuildable, so start fresh.
      if (grepl("lock", conditionMessage(e), ignore.case = TRUE)) {
        stop("The data file is in use. Close any running copy of the Trade Explorer ",
             "and try again.\n(", conditionMessage(e), ")", call. = FALSE)
      }
      say("Existing data file could not be opened; rebuilding it.")
      remove_db(db_path)
      DBI::dbConnect(duckdb_driver(), dbdir = db_path)
    }
  )

  version <- tryCatch(
    DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = 'schema_version'")$value,
    error = function(e) character(0)
  )
  if (length(version) == 1 && version != CIMT_SCHEMA_VERSION) {
    say("Data format has changed since the last run; rebuilding the database.")
    DBI::dbDisconnect(con, shutdown = TRUE)
    remove_db(db_path)
    con <- DBI::dbConnect(duckdb_driver(), dbdir = db_path)
  }
  con
}

create_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS trade (
      flow     VARCHAR,   -- imp | dom_exp | tot_exp
      level    VARCHAR,   -- HS2 | HS6
      year     SMALLINT,
      month    TINYINT,
      hs       VARCHAR,   -- HS2 or HS6 code
      country  VARCHAR,   -- ISO alpha-2 partner country
      province VARCHAR,   -- Canadian province (NULL for total exports)
      state    VARCHAR,   -- U.S. state (NULL when not applicable)
      value    DOUBLE,    -- CAD
      quantity DOUBLE,    -- in 'unit'
      unit     VARCHAR    -- unit of measure code (NULL at HS2)
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sources (
      archive VARCHAR, size DOUBLE, mtime DOUBLE, n_rows BIGINT, loaded_at TIMESTAMP
    )")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta (key VARCHAR, value VARCHAR)")
  DBI::dbExecute(con, "DELETE FROM meta WHERE key = 'schema_version'")
  DBI::dbExecute(con, "INSERT INTO meta VALUES ('schema_version', ?)",
                 params = list(CIMT_SCHEMA_VERSION))
}

# Lookups are only marked complete once build_lookups() has fully finished
lookups_exist <- function(con) {
  nrow(DBI::dbGetQuery(con, "SELECT 1 FROM meta WHERE key = 'lookups_built'")) > 0
}

# duckdb >= 1.5 asks where to keep its extension cache; the app needs none
duckdb_driver <- function(...) {
  if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    duckdb::duckdb(..., shared_home = FALSE)
  } else {
    duckdb::duckdb(...)
  }
}

# ------------------------------------------------------------
# Loading one archive
# ------------------------------------------------------------

load_archive <- function(con, z, work_dir) {
  members <- utils::unzip(z$path, list = TRUE)$Name
  tables  <- CIMT_TABLES[CIMT_TABLES$flow == z$flow, , drop = FALSE]

  unlink(work_dir, recursive = TRUE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  DBI::dbBegin(con)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbRollback(con), add = TRUE)

  DBI::dbExecute(con, "DELETE FROM trade WHERE flow = ? AND year = ?",
                 params = list(z$flow, z$year))
  DBI::dbExecute(con, "DELETE FROM sources WHERE archive = ?", params = list(z$archive))

  n_rows <- 0
  for (j in seq_len(nrow(tables))) {
    tb <- tables[j, ]
    pattern <- sprintf("(^|/)ODPFN%s_\\d{6}[A-Z]?\\.csv$", tb$table)
    member  <- members[grepl(pattern, members, ignore.case = TRUE)]
    if (length(member) == 0) next
    member <- member[1]

    utils::unzip(z$path, files = member, exdir = work_dir, junkpaths = TRUE)
    csv <- file.path(work_dir, basename(member))
    n_rows <- n_rows + insert_csv(con, csv, tb)
    unlink(csv)
  }

  DBI::dbExecute(con, "INSERT INTO sources VALUES (?, ?, ?, ?, current_timestamp)",
                 params = list(z$archive, z$size, z$mtime, n_rows))
  DBI::dbCommit(con)
  ok <- TRUE
  n_rows
}

insert_csv <- function(con, csv, tb) {
  cols <- c(ym = "INTEGER", hs = "VARCHAR", country = "VARCHAR")
  if (tb$has_province) cols <- c(cols, province = "VARCHAR")
  cols <- c(cols, state = "VARCHAR", value = "DOUBLE")
  if (tb$has_quantity) cols <- c(cols, quantity = "DOUBLE", unit = "VARCHAR")

  col_spec <- paste0("{", paste0("'", names(cols), "': '", cols, "'", collapse = ", "), "}")
  path <- DBI::dbQuoteString(con, normalizePath(csv, winslash = "/"))

  sql <- sprintf("
    INSERT INTO trade
    SELECT %s, %s,
           CAST(ym // 100 AS SMALLINT), CAST(ym %% 100 AS TINYINT),
           trim(hs), trim(country), %s, NULLIF(trim(state), ''),
           value, %s, %s
    FROM read_csv(%s, header = false, skip = 1, delim = ',', quote = '\"',
                  columns = %s)",
    DBI::dbQuoteString(con, tb$flow), DBI::dbQuoteString(con, tb$level),
    # Newfoundland was coded NF until 2002, NL after: use NL throughout
    if (tb$has_province) "CASE WHEN trim(province) = 'NF' THEN 'NL' ELSE NULLIF(trim(province), '') END" else "NULL",
    if (tb$has_quantity) "quantity" else "NULL",
    if (tb$has_quantity) "NULLIF(trim(unit), '')" else "NULL",
    path, col_spec)

  DBI::dbExecute(con, sql)
}

# ------------------------------------------------------------
# Lookup tables from the fixed-width description files
# ------------------------------------------------------------

# Column positions (1-based) of the ODPF_*Desc.TXT layouts.
# Product files (HS6/HS8/HS10) carry a unit-of-measure column; the others don't.
FWF_PRODUCT <- list(code = c(1, 11), start = c(12, 17), end = c(19, 24),
                    uom = c(26, 28), desc_en = c(30, 112), desc_fr = c(113, 195))
FWF_OTHER   <- list(code = c(1, 11), start = c(12, 17), end = c(19, 24),
                    desc_en = c(26, 108), desc_fr = c(109, 191))

# `key_fun` turns the raw code column into the lookup key (e.g. the province
# file stores "10 NF": numeric code then alpha code).
read_desc_file <- function(path, layout, key_fun = identity) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  out <- as.data.frame(lapply(layout, function(p) trimws(substr(lines, p[1], p[2]))),
                       stringsAsFactors = FALSE)
  out$code <- key_fun(out$code)
  # Codes can have several date-ranged descriptions; keep the most recent one.
  out <- out[order(out$code, out$end, out$start, decreasing = TRUE), ]
  out[!duplicated(out$code), ]
}

# Extract one description file from the newest archive that contains it.
extract_desc <- function(zips, flows, file_pattern, work_dir) {
  cand <- zips[zips$flow %in% flows, , drop = FALSE]
  cand <- cand[order(cand$year, decreasing = TRUE), , drop = FALSE]
  for (i in seq_len(nrow(cand))) {
    members <- utils::unzip(cand$path[i], list = TRUE)$Name
    hit <- members[grepl(file_pattern, basename(members), ignore.case = TRUE)]
    if (length(hit) > 0) {
      dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(cand$path[i], files = hit[1], exdir = work_dir, junkpaths = TRUE)
      return(file.path(work_dir, basename(hit[1])))
    }
  }
  NULL
}

build_lookups <- function(con, zips, work_dir) {
  DBI::dbExecute(con, "DELETE FROM meta WHERE key = 'lookups_built'")
  all_flows <- unname(CIMT_ARCHIVES)
  get <- function(pattern, flows = all_flows) extract_desc(zips, flows, pattern, work_dir)

  # --- Products --------------------------------------------------------
  hs6m <- get("^ODPF_3_HS6MDesc\\.txt$", "imp")
  hs6x <- get("^ODPF_4_HS6XDesc\\.txt$", c("dom_exp", "tot_exp"))
  hs2  <- get("^ODPF_5_HS2Desc\\.txt$")

  hs6 <- do.call(rbind, lapply(Filter(Negate(is.null), list(hs6x, hs6m)),
                               read_desc_file, layout = FWF_PRODUCT))
  # Prefer the export nomenclature's wording (as the original app did)
  hs6 <- hs6[!duplicated(hs6$code), c("code", "desc_en", "desc_fr")]
  hs6$level <- "HS6"
  hs2 <- if (!is.null(hs2)) read_desc_file(hs2, FWF_OTHER)[, c("code", "desc_en", "desc_fr")] else
    data.frame(code = character(), desc_en = character(), desc_fr = character())
  hs2$level <- "HS2"
  desc <- rbind(hs6, hs2)

  # Make sure every code that appears in the data is listed, even without a
  # description.
  in_data <- DBI::dbGetQuery(con, "
    SELECT level, hs AS code, MIN(year) AS first_year, MAX(year) AS last_year
    FROM trade GROUP BY level, hs")
  products <- merge(in_data, desc, by = c("level", "code"), all.x = TRUE)
  products$desc_en[is.na(products$desc_en) | products$desc_en == ""] <- "(no description available)"
  products <- products[order(products$level, products$code), ]
  write_lookup(con, "products", products)

  # --- Countries, provinces, states --------------------------------------
  cty <- get("^ODPF_6_CtyDesc\\.txt$")
  if (!is.null(cty)) {
    cty <- read_desc_file(cty, FWF_OTHER, function(code) substr(code, 1, 2))  # "AD 156" -> "AD"
  } else cty <- data.frame(code = character(), desc_en = character())
  write_lookup(con, "countries", data.frame(code = cty$code, name = cty$desc_en))

  prov <- get("^ODPF_8_ProvDesc\\.txt$")
  if (!is.null(prov)) {
    prov <- read_desc_file(prov, FWF_OTHER, function(code) trimws(substr(code, 4, 11)))  # "10 NF" -> "NF"
  } else prov <- data.frame(code = character(), desc_en = character())
  prov <- prov[prov$code != "NF", ]  # merged into NL when loading
  write_lookup(con, "provinces", data.frame(code = prov$code, name = prov$desc_en))

  st <- get("^ODPF_7_StateDesc\\.txt$")
  st <- if (!is.null(st)) read_desc_file(st, FWF_OTHER) else
    data.frame(code = character(), desc_en = character())
  write_lookup(con, "states", data.frame(code = st$code, name = st$desc_en))

  # --- Units -------------------------------------------------------------
  uom <- get("^ODPF_9_UOMDesc\\.txt$")
  uom <- if (!is.null(uom)) read_desc_file(uom, FWF_OTHER) else
    data.frame(code = character(), desc_en = character())
  units <- data.frame(code = uom$code, name = uom$desc_en,
                      to_kg = unname(MASS_TO_KG[uom$code]))
  missing_mass <- setdiff(names(MASS_TO_KG), units$code)
  if (length(missing_mass)) {
    units <- rbind(units, data.frame(code = missing_mass, name = missing_mass,
                                     to_kg = unname(MASS_TO_KG[missing_mass])))
  }
  write_lookup(con, "units", units)

  # --- Coverage ----------------------------------------------------------
  coverage <- DBI::dbGetQuery(con, "
    SELECT flow, MIN(CAST(year AS INTEGER) * 100 + month) AS first_ym,
           MAX(CAST(year AS INTEGER) * 100 + month) AS last_ym
    FROM trade GROUP BY flow")
  write_lookup(con, "coverage", coverage)
  DBI::dbExecute(con, "DELETE FROM meta WHERE key = 'lookups_built'")
  DBI::dbExecute(con, "INSERT INTO meta VALUES ('lookups_built', CAST(current_timestamp AS VARCHAR))")
}

write_lookup <- function(con, name, df) {
  DBI::dbWriteTable(con, name, df, overwrite = TRUE)
}

# ------------------------------------------------------------
# Command-line entry point
# ------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else "scripts"
  prepare_data(root = normalizePath(file.path(script_dir, "..")),
               rebuild = "--rebuild" %in% args)
}
