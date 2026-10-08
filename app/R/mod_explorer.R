# ============================================================
# EXPLORER MODULE
# ------------------------------------------------------------
# The "Time Series", "Market Share" and "Data Table" tabs for a selection of
# products. Used for the main product picker and again inside the Steel tab
# (where the selection is a set of steel categories).
#
# Server arguments:
#   filt()  filter list (see data_access.R)
#   sel()   list(level, hs, groups = data.frame(group, hs), noun, title)
#   opts()  list(top_n, partner_mode)
# ============================================================

explorer_tabs <- function(id, noun = "Product") {
  ns <- NS(id)
  breakdown_choices <- setNames(c("partner", "group"), c("Trading partner", noun))

  list(
    nav_panel(
      "Time Series",
      layout_column_wrap(
        width = "160px", fill = FALSE, class = "mb-3",
        value_box(title = "Export value", value = textOutput(ns("vb_exports"), inline = TRUE),
                  p(textOutput(ns("vb_exports_note"), inline = TRUE)), theme = "light"),
        value_box(title = "Import value", value = textOutput(ns("vb_imports"), inline = TRUE),
                  p(textOutput(ns("vb_imports_note"), inline = TRUE)), theme = "light"),
        value_box(title = "Trade balance", value = textOutput(ns("vb_balance"), inline = TRUE),
                  p(textOutput(ns("vb_balance_note"), inline = TRUE)), theme = "light"),
        value_box(title = "Top export destination", value = textOutput(ns("vb_top_exp"), inline = TRUE),
                  p(textOutput(ns("vb_top_exp_note"), inline = TRUE)), theme = "light"),
        value_box(title = "Top import source", value = textOutput(ns("vb_top_imp"), inline = TRUE),
                  p(textOutput(ns("vb_top_imp_note"), inline = TRUE)), theme = "light")
      ),
      card(
        card_header(
          class = "d-flex flex-wrap gap-3 align-items-end",
          checkboxGroupInput(ns("directions"), "Show", choices = c("Exports", "Imports"),
                             selected = c("Exports", "Imports"), inline = TRUE),
          selectInput(ns("freq"), "Time step", choices = FREQUENCIES, selected = "month",
                      width = "140px"),
          radioButtons(ns("breakdown"), "Lines show", choices = breakdown_choices, inline = TRUE),
          div(class = "ms-auto",
              downloadButton(ns("dl_ts"), "Download chart data", class = "btn-sm btn-outline-secondary"))
        ),
        card_body(
          conditionalPanel(
            sprintf("input['%s'] && input['%s'].indexOf('Exports') > -1", ns("directions"), ns("directions")),
            plotlyOutput(ns("ts_exports"), height = "420px")
          ),
          conditionalPanel(
            sprintf("input['%s'] && input['%s'].indexOf('Imports') > -1", ns("directions"), ns("directions")),
            plotlyOutput(ns("ts_imports"), height = "420px")
          ),
          uiOutput(ns("ts_note"))
        )
      )
    ),

    nav_panel(
      "Market Share",
      card(
        card_header(
          class = "d-flex flex-wrap gap-3 align-items-end",
          radioButtons(ns("share_by"), "Share by", choices = breakdown_choices, inline = TRUE),
          div(class = "ms-auto",
              downloadButton(ns("dl_share"), "Download chart data", class = "btn-sm btn-outline-secondary"))
        ),
        card_body(
          layout_columns(
            col_widths = breakpoints(sm = 12, lg = c(6, 6)),
            plotlyOutput(ns("share_exports"), height = "460px"),
            plotlyOutput(ns("share_imports"), height = "460px")
          )
        )
      )
    ),

    nav_panel(
      "Data Table",
      card(
        card_header(
          class = "d-flex flex-wrap gap-3 align-items-center",
          span("Monthly trade for your selection. Type in the boxes under each heading to filter."),
          div(class = "ms-auto",
              downloadButton(ns("dl_table"), "Download all rows (CSV)", class = "btn-sm btn-outline-secondary"))
        ),
        card_body(fillable = FALSE, DT::DTOutput(ns("table")))
      )
    )
  )
}

explorer_server <- function(id, con, lk, filt, sel, opts) {
  moduleServer(id, function(input, output, session) {

    # ---- Data -----------------------------------------------------------
    detail <- reactive({
      s <- sel()
      f <- filt()
      validate(need(length(s$hs) > 0,
                    paste0("Choose at least one ", tolower(s$noun), " on the left to get started.")))
      exp <- query_detail(con, f, f$export_flow, s$hs, s$level)
      imp <- query_detail(con, f, "imp", s$hs, s$level)
      d <- rbind(with_direction(exp, "Exports"), with_direction(imp, "Imports"))
      add_partner_label(d, opts()$partner_mode, lk)
    })

    metric <- reactive(filt()$metric)
    unit   <- reactive(unit_label(detail()$metric_unit, lk))

    # Rows labelled by the chosen breakdown ("series"), top-N lumped
    with_series <- function(d, breakdown) {
      if (breakdown == "group") {
        g <- sel()$groups
        d <- dplyr::inner_join(d, g, by = "hs", relationship = "many-to-many")
        d$series <- d$group
      } else {
        d$series <- d$partner
      }
      lump_top_n(d, "series", opts()$top_n)
    }

    series_data <- function(direction) {
      d <- detail()
      d <- d[d$direction == direction, , drop = FALSE]
      if (nrow(d) == 0) return(data.frame(period = as.Date(character()), series = factor(), metric = numeric()))
      d <- with_series(d, input$breakdown)
      d <- add_period(d, input$freq)
      d |>
        dplyr::group_by(period, series) |>
        dplyr::summarise(metric = sum(metric, na.rm = TRUE), .groups = "drop")
    }

    ts_title <- function(direction) {
      f <- filt()
      flow <- if (direction == "Exports") FLOW_LABELS[[f$export_flow]] else "Imports"
      prov <- if (!is.null(f$province) && !(direction == "Exports" && f$export_flow == "tot_exp"))
        paste0(" \u2013 ", lookup_name(f$province, lk$provinces)) else ""
      paste0(flow, prov, ": ", sel()$title)
    }

    # ---- Value boxes ----------------------------------------------------
    totals <- reactive({
      d <- detail()
      list(
        exp = sum(d$value[d$direction == "Exports"], na.rm = TRUE),
        imp = sum(d$value[d$direction == "Imports"], na.rm = TRUE),
        top = lapply(c(Exports = "Exports", Imports = "Imports"), function(dir) {
          dd <- d[d$direction == dir, ]
          if (nrow(dd) == 0) return(NULL)
          by_c <- sort(tapply(dd$value, dd$country, sum, na.rm = TRUE), decreasing = TRUE)
          list(name = country_name(names(by_c)[1], lk), share = by_c[[1]] / sum(by_c))
        })
      )
    })
    years_text <- reactive({
      y <- filt()$years
      if (y[1] == y[2]) as.character(y[1]) else paste0(y[1], "\u2013", y[2])
    })

    output$vb_exports      <- renderText(fmt_dollars(totals()$exp))
    output$vb_exports_note <- renderText(paste(FLOW_LABELS[[filt()$export_flow]], years_text()))
    output$vb_imports      <- renderText(fmt_dollars(totals()$imp))
    output$vb_imports_note <- renderText(paste("Imports", years_text()))
    output$vb_balance      <- renderText({
      b <- totals()$exp - totals()$imp
      paste0(if (b < 0) "\u2212" else "+", fmt_dollars(abs(b)))
    })
    output$vb_balance_note <- renderText(if (totals()$exp >= totals()$imp) "Surplus (exports > imports)" else "Deficit (imports > exports)")
    output$vb_top_exp      <- renderText(totals()$top$Exports$name %||% "\u2013")
    output$vb_top_exp_note <- renderText({
      t <- totals()$top$Exports
      if (is.null(t)) "" else paste(fmt_pct(t$share), "of exports")
    })
    output$vb_top_imp      <- renderText(totals()$top$Imports$name %||% "\u2013")
    output$vb_top_imp_note <- renderText({
      t <- totals()$top$Imports
      if (is.null(t)) "" else paste(fmt_pct(t$share), "of imports")
    })

    # ---- Time series ----------------------------------------------------
    output$ts_exports <- renderPlotly({
      plot_time_series(series_data("Exports"), ts_title("Exports"), metric(), unit(),
                       input$freq, "exports-over-time")
    })
    output$ts_imports <- renderPlotly({
      plot_time_series(series_data("Imports"), ts_title("Imports"), metric(), unit(),
                       input$freq, "imports-over-time")
    })

    output$ts_note <- renderUI({
      notes <- character()
      f <- filt()
      if (f$years[2] >= lk$last_year && lk$last_ym %% 100 < 12) {
        notes <- c(notes, paste0("Data for ", lk$last_year, " runs to ", ym_label(lk$last_ym),
                                 if (input$freq == "year") ", so that year's total is partial." else "."))
      }
      if (metric() == "quantity" && unit() == "mixed units") {
        notes <- c(notes, "Your selection is reported in different units; weights are converted to kg, other units are added as reported.")
      }
      if (!is.null(f$province) && f$export_flow == "tot_exp") {
        notes <- c(notes, "Total exports are only published for Canada as a whole, so the province filter applies to imports only.")
      }
      if (length(notes)) div(class = "text-muted small", lapply(notes, p))
    })

    output$dl_ts <- downloadHandler(
      filename = function() paste0("time-series-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- rbind(with_direction(series_data("Exports"), "Exports"),
                     with_direction(series_data("Imports"), "Imports"))
        names(out)[names(out) == "metric"] <- if (metric() == "value") "value_cad" else paste0("quantity_", unit())
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # ---- Market share ---------------------------------------------------
    share_data <- function(direction) {
      d <- detail()
      d <- d[d$direction == direction, , drop = FALSE]
      if (nrow(d) == 0) return(data.frame(series = factor(), total = numeric()))
      d <- with_series(d, input$share_by)
      d |>
        dplyr::group_by(series) |>
        dplyr::summarise(total = sum(metric, na.rm = TRUE), .groups = "drop")
    }
    output$share_exports <- renderPlotly({
      plot_share_bars(share_data("Exports"), ts_title("Exports"), metric(), unit(), "export-share")
    })
    output$share_imports <- renderPlotly({
      plot_share_bars(share_data("Imports"), ts_title("Imports"), metric(), unit(), "import-share")
    })
    output$dl_share <- downloadHandler(
      filename = function() paste0("market-share-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- rbind(with_direction(share_data("Exports"), "Exports"),
                     with_direction(share_data("Imports"), "Imports"))
        out$share <- ave(out$total, out$direction, FUN = function(x) x / sum(x))
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # ---- Data table -----------------------------------------------------
    table_data <- reactive({
      d <- detail()
      s <- sel()
      d |>
        dplyr::group_by(year, month, direction, hs, partner, metric_unit) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE),
                         quantity = sum(quantity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(year), dplyr::desc(month), direction, dplyr::desc(value)) |>
        dplyr::transmute(
          Period    = sprintf("%d-%02d", year, month),
          Direction = direction,
          Product   = product_label(hs, lk, s$level),
          Partner   = partner,
          `Value (CAD)` = value,
          Quantity  = ifelse(is.na(metric_unit), NA, quantity),
          Unit      = ifelse(metric_unit == "KGM", "kg", metric_unit)
        )
    })

    output$table <- DT::renderDT({
      DT::datatable(
        table_data(), rownames = FALSE, filter = "top", fillContainer = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE,
                       language = list(search = "Search all columns:"))
      ) |>
        DT::formatCurrency("Value (CAD)", digits = 0) |>
        DT::formatRound("Quantity", digits = 0)
    })

    output$dl_table <- downloadHandler(
      filename = function() paste0("trade-data-", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(table_data(), file, row.names = FALSE, na = "")
    )
  })
}
