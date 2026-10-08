# ============================================================
# USER INTERFACE
# ============================================================

hs_choices <- function(lk, level) {
  p <- lk$products[lk$products$level == level, ]
  setNames(p$code, p$label)
}

default_years <- function(lk) c(max(lk$first_year, lk$last_year - 3L), lk$last_year)

app_ui <- function(lk, steel_categories) {
  sidebar_ui <- sidebar(
    width = 340,
    accordion(
      open = c("Products", "Time & place"), multiple = TRUE,
      accordion_panel(
        "Products",
        radioButtons("level", "Level of detail",
                     choices = c("HS2 - broad chapters" = "HS2",
                                 "HS6 - detailed products" = "HS6"),
                     selected = "HS2"),
        selectizeInput("products", "Products (up to 10)", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 10, plugins = list("remove_button"),
                                      placeholder = "Type a code or a word, e.g. steel")),
        radioButtons("metric", "Measure",
                     choices = c("Value (CAD $)" = "value", "Quantity" = "quantity"),
                     inline = TRUE),
        conditionalPanel(
          "input.level == 'HS2' && input.metric == 'quantity'",
          div(class = "alert alert-warning py-2 small",
              "Quantities are only published at HS6. Showing values instead.")
        )
      ),
      accordion_panel(
        "Time & place",
        sliderInput("years", "Years", min = lk$first_year, max = lk$last_year,
                    value = default_years(lk), step = 1, sep = "", ticks = FALSE),
        radioButtons("export_flow", "Exports",
                     choices = c("Domestic (made in Canada)" = "dom_exp",
                                 "Total (incl. re-exports)" = "tot_exp")),
        selectInput("province", "Province / territory",
                    choices = c("All of Canada" = "", setNames(names(lk$provinces), lk$provinces)))
      ),
      accordion_panel(
        "Display",
        selectInput("partner_mode", "Show trading partners as", choices = PARTNER_MODES),
        sliderInput("top_n", "Number of partners to show", min = 1, max = MAX_TOP_N,
                    value = 5, step = 1, ticks = FALSE),
        checkboxInput("exclude_internal", "Leave out Canada as a partner (re-imports)", FALSE)
      )
    ),
    actionButton("reset", "Reset settings", class = "btn-sm btn-outline-secondary"),
    div(class = "text-muted small",
        "Source: Statistics Canada, Canadian International Merchandise Trade. Data from ",
        paste0(ym_label(min(lk$coverage$first_ym)), " to ", ym_label(lk$last_ym), "."))
  )

  main_tabs <- c(
    list(id = "main_tabs"),
    explorer_tabs("main", noun = "Product"),
    if (!is.null(steel_categories)) list(steel_tab("steel", steel_categories)),
    list(other_tab("other"))
  )

  page_sidebar(
    title = div(class = "d-flex align-items-center w-100",
                span(APP_TITLE),
                actionLink("help", "How to use", class = "ms-auto small")),
    window_title = APP_TITLE,
    theme = bs_theme(version = 5, primary = HIGHLIGHT_COLOUR,
                     "font-size-base" = "0.95rem"),
    fillable = FALSE,
    sidebar = sidebar_ui,
    tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
    # Spinners while charts update (shiny >= 1.8.1)
    if ("useBusyIndicators" %in% getNamespaceExports("shiny")) shiny::useBusyIndicators(),
    do.call(navset_card_underline, main_tabs)
  )
}

no_data_ui <- function() {
  page_fillable(
    title = APP_TITLE,
    card(
      class = "m-5", max_height = "420px",
      card_header(h3(APP_TITLE)),
      card_body(
        h4("The trade data hasn't been prepared yet."),
        p("Close this window and start the app with the ", strong("Start Trade Explorer"),
          " launcher in the project folder. It prepares the data automatically the first time."),
        p("Or, in R, run: ", code("source(\"launch.R\")"), " from the project folder."),
        p(class = "text-muted", "Expected data file: ", code(DB_PATH))
      )
    )
  )
}
