# Canadian Trade Explorer

An interactive app for exploring Canada's imports and exports, built from
Statistics Canada's **Canadian International Merchandise Trade (CIMT)** data,
January 1996 onward. Pick a product, choose the years and region, and see who
Canada trades with, how that has changed over time, and how dependent that
trade is on the United States.

The app runs on your own computer and opens in your web browser. No account,
server or internet connection is needed once it is set up.

---

## Quick start (Windows)

You only need to do steps 1 and 2 once.

1. **Install R** (the free program the app runs on)
   - Go to <https://cran.r-project.org/bin/windows/base/> and click
     **Download R for Windows**.
   - Run the downloaded file and click **Next** through every screen.
     If Windows asks for an administrator password you don't have, cancel and
     run it again, choosing to install **just for me**.

2. **Download this project**
   - On this project's GitHub page, click the green **Code** button, then
     **Download ZIP**. The download is large (about 2.5 GB) because it includes
     all the trade data.
   - Find the ZIP in your *Downloads* folder, right-click it and choose
     **Extract All…**, then **Extract**.

3. **Start the app**
   - Open the extracted folder and double-click **`Start Trade Explorer`**
     (the file ending in `.bat`).
   - If Windows shows *"Windows protected your PC"*, click **More info** and then
     **Run anyway**.
   - A black window opens and shows progress. **The first time only**, it
     installs a few add-ons and prepares the data. This takes about 5–15
     minutes. After that, the app starts in a few seconds.
   - The app opens in your web browser. **Keep the black window open while
     you use the app**; close it when you're done.

## Quick start (Mac)

1. Install R from <https://cran.r-project.org/bin/macosx/> (choose the
   installer that matches your Mac: *Apple silicon* or *Intel*).
2. Download and unzip this project as in step 2 above.
3. Double-click **`Start Trade Explorer.command`**. If macOS says it can't be
   opened, right-click the file, choose **Open**, then **Open** again.

## Using RStudio instead

Open the project folder in RStudio, open `launch.R` and click **Source** (or run
`source("launch.R")` in the console).

---

## What's in the app

Use the panel on the left to choose:

| Setting | What it does |
|---|---|
| **Level of detail** | *HS2*: 99 broad chapters (e.g. 72 Iron and steel). *HS6*: about 6,000 detailed products. |
| **Products** | Type a code (`7208`) or a word (`steel`). Pick up to 10. Your choice is kept when you change any other setting. |
| **Measure** | Trade value in Canadian dollars, or quantity (HS6 only; weights are converted to kg). |
| **Years** | Any range from 1996 to the latest month available. |
| **Exports** | *Domestic*: goods made in Canada. *Total*: also includes re-exports of foreign goods (published for Canada as a whole only). |
| **Province / territory** | All of Canada, or one province. |
| **Show trading partners as** | Countries; countries by province (e.g. *Ontario → United States*); or U.S. states by province (e.g. *Ontario → Michigan (U.S.)*). |

The tabs:

- **Time Series**: headline totals, then exports and imports over time
  (monthly, quarterly or yearly), split by trading partner or by product.
- **Market Share**: each partner's share of trade.
- **Data Table**: the monthly numbers behind the charts. Filter, sort and
  download them.
- **Steel**: the same views for groups of steel products. It appears only when
  a steel category file is provided (see
  [`data/reference/README.md`](data/reference/README.md)).
- **Other**: dependency analysis.
  - *Dependency Matrix*: every product plotted by how concentrated its trading
    partners are (HHI) and how much of its trade is with the U.S. Click a
    bubble to add that product to your selection.
  - *Partner Concentration (HHI)*: the distribution of concentration scores,
    with your products marked.
  - *Canada vs U.S. Competitiveness*: who leads Canada–U.S. trade in each
    product and how Canada's prices compare.

Every chart can be saved as a picture with the camera icon that appears when
you hover over it, and every tab has a **Download** button for its data.

---

## Updating the data

The trade data lives in `data/raw/` as the original Statistics Canada zip files,
one per year and trade type:

```
data/raw/CIMT-CICM_Imp_2025.zip       imports
data/raw/CIMT-CICM_Dom_Exp_2025.zip   domestic exports
data/raw/CIMT-CICM_Tot_Exp_2025.zip   total exports
```

To add a new year, or replace the current year with a newer release, download
the zip from Statistics Canada's CIMT data downloads, keep the same file name
pattern, put it in `data/raw/`, and start the app again. Only new or changed
files are processed, so updates take seconds.

To rebuild everything from scratch, delete `data/processed/cimt.duckdb` (or run
`Rscript scripts/prepare_data.R --rebuild`).

---

## Troubleshooting

| Problem | Fix |
|---|---|
| *"R is not installed"* | Install R (step 1), then start the app again. |
| Packages fail to install | Check your internet connection. On a work network, you may need IT to allow access to `cloud.r-project.org`. |
| *"The data file is in use"* | Another copy of the app is still running. Close all black app windows and try again. |
| The browser didn't open | Copy the `http://127.0.0.1:...` address shown in the black window into your browser. |
| Out of disk space while preparing data | Preparing the data needs about 3 GB of free space. |

---

## For developers

### Project layout

```
├── Start Trade Explorer.bat       Windows launcher (finds R, runs launch.R)
├── Start Trade Explorer.command   macOS / Linux launcher
├── launch.R                       installs packages, prepares data, starts the app
├── app/
│   ├── app.R                      entry point (Shiny auto-loads app/R/)
│   ├── R/
│   │   ├── config.R               settings, colours, paths
│   │   ├── data_access.R          SQL queries against the DuckDB database
│   │   ├── analysis.R             HHI, U.S. dependency, competitiveness, labels
│   │   ├── plots.R                plotly chart builders
│   │   ├── mod_explorer.R         Time Series / Market Share / Data Table tabs
│   │   ├── mod_steel.R            Steel tab
│   │   ├── mod_other.R            Dependency analysis ("Other") tab
│   │   ├── app_ui.R               page layout
│   │   └── app_server.R           wiring between inputs and modules
│   └── www/styles.css
├── scripts/
│   └── prepare_data.R             zip archives -> data/processed/cimt.duckdb
└── data/
    ├── raw/                       Statistics Canada zip archives (tracked in git)
    ├── reference/                 optional steel category file + template
    └── processed/                 generated database (ignored by git)
```

### Data pipeline

`scripts/prepare_data.R` reads each zip in `data/raw/`, extracts only the HS2 and
HS6 tables (CIMT tables 015/022 imports, 018/020 domestic exports, 019/021
total exports), and loads them into a single DuckDB table, `trade`. It parses the
fixed-width `ODPF_*Desc.TXT` description files (Latin-1) into lookup tables
(`products`, `countries`, `provinces`, `states`, `units`) using the newest
description for each code. Processed archives are recorded in `sources` by
size and modification time, so reruns are incremental. Bump
`CIMT_SCHEMA_VERSION` when the database layout changes to force a rebuild.

The app opens the database read-only and pushes aggregation into SQL, so all
30 years stay available without loading them into memory.

### Requirements

R ≥ 4.1 and the packages `shiny`, `bslib`, `plotly`, `DT`, `dplyr`, `DBI` and
`duckdb`. `launch.R` installs any that are missing into the user's personal
library.

### Git conventions

- Generated data (`data/processed/`) and extracted CSV/TXT files are ignored.
  Only the original zip archives are committed.
- Work on a feature branch and open a pull request into `main`.
- `.gitattributes` keeps `.bat` files in CRLF and shell scripts in LF so the
  launchers work on every platform.
