# ============================================================
# CANADIAN TRADE EXPLORER - LAUNCHER
# ------------------------------------------------------------
# Does everything needed to open the app in your web browser:
#   1. installs any missing R packages (first run only)
#   2. prepares the trade data from data/raw/*.zip (first run, or when new
#      zips are added)
#   3. starts the app and opens it in your browser
#
# Normally started by double-clicking "Start Trade Explorer.bat" (Windows) or
# "Start Trade Explorer.command" (Mac). From RStudio: open this file and click
# "Source", or run  source("launch.R")  with the project folder as the
# working directory.
# ============================================================

local({

  # ---- Locate the project folder -------------------------------------------
  find_root <- function() {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
    }
    getwd()
  }
  root <- find_root()
  if (!file.exists(file.path(root, "app", "app.R"))) {
    stop("Can't find the app. Run this script from the Shiny-Trade-Tool folder.", call. = FALSE)
  }

  say <- function(...) cat(..., "\n", sep = "")
  say("")
  say("==============================================")
  say("  Canadian Trade Explorer")
  say("==============================================")

  if (getRversion() < "4.1.0") {
    stop("R 4.1 or newer is needed (you have ", getRversion(), "). ",
         "Please install the latest R from https://cran.r-project.org", call. = FALSE)
  }

  # ---- 1. Packages ---------------------------------------------------------
  required <- c(shiny = "1.8.0", bslib = "0.6.0", plotly = "4.10.0", DT = "0.27",
                dplyr = "1.1.0", DBI = "1.1.0", duckdb = "0.9.0")

  needs_install <- vapply(names(required), function(p) {
    !requireNamespace(p, quietly = TRUE) || utils::packageVersion(p) < required[[p]]
  }, logical(1))

  if (any(needs_install)) {
    pkgs <- names(required)[needs_install]
    say("")
    say("Installing R packages (first run only, a few minutes): ", paste(pkgs, collapse = ", "))

    # Install into the personal library so no administrator rights are needed
    lib <- Sys.getenv("R_LIBS_USER")
    if (!nzchar(lib)) lib <- file.path(path.expand("~"), "R", "library")
    lib <- strsplit(lib, .Platform$path.sep)[[1]][1]
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))

    repos <- getOption("repos")
    if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    options(install.packages.compile.from.source = "never")
    utils::install.packages(pkgs, lib = lib, repos = repos, quiet = TRUE)

    still_missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing)) {
      stop("These packages could not be installed: ", paste(still_missing, collapse = ", "),
           ".\nCheck your internet connection (or proxy settings) and try again.", call. = FALSE)
    }
    say("Packages installed.")
  }

  # ---- 2. Data ---------------------------------------------------------------
  say("")
  say("Checking the trade data...")
  source(file.path(root, "scripts", "prepare_data.R"), local = TRUE)
  prepare_data(root)

  # ---- 3. App ----------------------------------------------------------------
  say("")
  say("Starting the app. It will open in your web browser.")
  say("Keep this window open while you use the app; close it to stop the app.")
  say("")
  shiny::runApp(file.path(root, "app"), launch.browser = TRUE, quiet = TRUE)
})
