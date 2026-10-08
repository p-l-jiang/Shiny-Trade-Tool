# ============================================================
# SERVER
# ============================================================

app_server <- function(con, lk, steel_categories) function(input, output, session) {

  # ---- Product picker that remembers its selection ----------------------
  # Each level keeps its own selection, so switching HS2 <-> HS6 or changing
  # any other setting never throws away what the user picked.
  remembered <- reactiveValues(HS2 = DEFAULT_PRODUCT[["HS2"]], HS6 = character(0))
  code_width <- c(HS2 = 2L, HS6 = 6L)

  set_products <- function(level, selected) {
    updateSelectizeInput(session, "products", choices = hs_choices(lk, level),
                         selected = selected, server = TRUE)
  }

  observeEvent(input$level, {
    lvl <- input$level
    selected <- remembered[[lvl]]
    # First visit to HS2 after picking HS6 products: use their chapters
    if (length(selected) == 0 && lvl == "HS2" && length(remembered$HS6) > 0) {
      selected <- unique(substr(remembered$HS6, 1, 2))
    }
    set_products(lvl, selected)
  })

  observeEvent(input$products, {
    lvl <- isolate(input$level)
    codes <- input$products
    valid <- codes[nchar(codes) == code_width[[lvl]]]
    # Ignore stale values from the other level while the picker is updating
    if (length(codes) > 0 && length(valid) == 0) return()
    remembered[[lvl]] <- valid
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  level <- reactive(input$level)
  selected_codes <- reactive({
    codes <- input$products
    codes[nchar(codes) == code_width[[level()]]]
  })

  # ---- Shared filters (debounced so dragging the slider stays smooth) ----
  filt <- reactive({
    list(
      level            = level(),
      export_flow      = input$export_flow,
      years            = input$years,
      province         = if (nzchar(input$province)) input$province else NULL,
      exclude_internal = input$exclude_internal,
      metric           = if (level() == "HS6") input$metric else "value"
    )
  }) |> debounce(400)

  opts <- reactive(list(top_n = input$top_n, partner_mode = input$partner_mode))

  sel <- reactive({
    codes <- selected_codes()
    list(
      level  = level(),
      hs     = codes,
      groups = data.frame(group = short_label(product_label(codes, lk, level())), hs = codes),
      noun   = "Product",
      title  = if (length(codes) == 1) short_label(product_label(codes, lk, level()), 70) else
        short_product_text(codes)
    )
  })

  # ---- Tabs ---------------------------------------------------------------
  explorer_server("main", con, lk, filt, sel, opts)

  clicked <- other_server("other", con, lk, filt, sel)
  observeEvent(clicked(), {
    code <- clicked()$code
    current <- selected_codes()
    if (nchar(code) != code_width[[level()]] || code %in% current) return()
    if (length(current) >= 10) {
      showNotification("You can compare up to 10 products. Remove one first.", type = "warning")
      return()
    }
    set_products(level(), c(current, code))
    showNotification(paste("Added", product_label(code, lk, level()), "to your selection."),
                     duration = 3)
  })

  if (!is.null(steel_categories)) {
    steel_server("steel", con, lk, filt, opts, steel_categories)
  }

  # ---- Reset & help ---------------------------------------------------------
  observeEvent(input$reset, {
    updateRadioButtons(session, "metric", selected = "value")
    updateSliderInput(session, "years", value = default_years(lk))
    updateRadioButtons(session, "export_flow", selected = "dom_exp")
    updateSelectInput(session, "province", selected = "")
    updateSelectInput(session, "partner_mode", selected = "country")
    updateSliderInput(session, "top_n", value = 5)
    updateCheckboxInput(session, "exclude_internal", value = FALSE)
  })

  observeEvent(input$help, {
    showModal(modalDialog(
      title = "How to use the Trade Explorer",
      easyClose = TRUE, size = "l", footer = modalButton("Got it"),
      tags$ol(
        tags$li(strong("Pick products"), " on the left. Type a code (e.g. 7208) or a word (e.g. ",
                em("steel"), "). HS2 gives broad chapters; HS6 gives detailed products."),
        tags$li(strong("Choose years and a region."), " Your products stay selected when you change ",
                "any other setting."),
        tags$li(strong("Browse the tabs:"), tags$ul(
          tags$li(strong("Time Series"), " \u2013 trade over time, by partner or by product."),
          tags$li(strong("Market Share"), " \u2013 who Canada trades these products with."),
          tags$li(strong("Data Table"), " \u2013 the underlying numbers, ready to filter and download."),
          if (!is.null(steel_categories)) tags$li(strong("Steel"), " \u2013 the same views for steel categories."),
          tags$li(strong("Other"), " \u2013 dependency analysis: concentration of partners (HHI), ",
                  "reliance on the U.S., and Canada-vs-U.S. competitiveness.")
        )),
        tags$li(strong("Save charts"), " with the camera icon that appears when you hover over a chart, ",
                "or use the ", em("Download"), " buttons for the data.")
      ),
      p(class = "text-muted small",
        "Domestic exports are goods produced in Canada; total exports also include re-exports of ",
        "foreign goods. Total exports are only published for Canada as a whole.")
    ))
  })
}
