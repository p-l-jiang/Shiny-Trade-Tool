# ============================================================
# "OTHER" TAB: DEPENDENCY ANALYSIS
# ------------------------------------------------------------
# Dependency matrix (HHI vs U.S. share), partner-concentration histograms and
# the Canada-vs-U.S. competitiveness view, all for the main product selection.
#
# Returns a reactive holding list(code, at) for the product most recently
# clicked in a dependency-matrix chart, so the app can add it to the selection.
# ============================================================

other_tab <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Other",
    navset_pill(
      id = ns("tabs"),

      nav_panel(
        "Dependency Matrix",
        div(class = "text-muted small my-2",
            "Each bubble is a product, sized by trade value. Further right = trade is concentrated in ",
            "fewer partner countries; higher = a bigger share of trade is with the U.S. Dashed lines ",
            "are averages. ", strong("Tip: click a bubble to add that product to your selection.")),
        card(
          card_header(
            class = "d-flex flex-wrap gap-3 align-items-end",
            selectInput(ns("highlight"), "Highlight an HS section",
                        choices = c("None" = "", unique(hs_section(sprintf("%02d", 1:99)))),
                        width = "260px"),
            div(class = "ms-auto",
                downloadButton(ns("dl_dep"), "Download chart data", class = "btn-sm btn-outline-secondary"))
          ),
          card_body(
            layout_columns(
              col_widths = breakpoints(sm = 12, lg = c(6, 6)),
              plotlyOutput(ns("dep_exports"), height = "500px"),
              plotlyOutput(ns("dep_imports"), height = "500px")
            )
          )
        ),
        card(card_header("Your selection"), card_body(fillable = FALSE, DT::DTOutput(ns("dep_table"))))
      ),

      nav_panel(
        "Partner Concentration (HHI)",
        div(class = "text-muted small my-2",
            "The Herfindahl-Hirschman Index (HHI) measures how concentrated trade is among partner ",
            "countries: below 1,500 is diversified, above 2,500 is highly concentrated, 10,000 means a ",
            "single partner. At HS6 the charts show the products in the same HS2 chapter(s) as your ",
            "selection; orange lines mark your selected products."),
        card(
          card_header(class = "d-flex",
                      div(class = "ms-auto",
                          downloadButton(ns("dl_hhi"), "Download chart data",
                                         class = "btn-sm btn-outline-secondary"))),
          card_body(
            layout_columns(
              col_widths = breakpoints(sm = 12, lg = c(6, 6)),
              plotlyOutput(ns("hhi_exports"), height = "420px"),
              plotlyOutput(ns("hhi_imports"), height = "420px")
            )
          )
        ),
        card(card_header("Your selection"), card_body(fillable = FALSE, DT::DTOutput(ns("hhi_table"))))
      ),

      nav_panel(
        "Canada vs U.S. Competitiveness",
        div(class = "text-muted small my-2",
            "Compares Canada's trade with the U.S. for every HS6 product. The vertical axis shows who ",
            "dominates the two-way trade; the horizontal axis compares Canada's average export price ",
            "with its average import price. Always uses HS6 products and trade values; if you picked ",
            "HS2 chapters, their HS6 products are highlighted."),
        card(
          card_header(class = "d-flex",
                      div(class = "ms-auto",
                          downloadButton(ns("dl_comp"), "Download chart data",
                                         class = "btn-sm btn-outline-secondary"))),
          card_body(plotlyOutput(ns("comp_plot"), height = "560px"))
        ),
        card(card_header("Your selection"), card_body(fillable = FALSE, DT::DTOutput(ns("comp_table"))))
      )
    )
  )
}

other_server <- function(id, con, lk, filt, sel) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metric <- reactive(filt()$metric)

    # ---- Concentration for every product ---------------------------------
    conc <- function(flow) {
      f <- filt()
      out <- summarise_concentration(query_partner_totals(con, f, flow))
      out$label <- product_label(out$product, lk, f$level)
      out
    }
    conc_exp <- reactive(conc(filt()$export_flow))
    conc_imp <- reactive(conc("imp"))

    unit <- reactive(if (metric() == "value") "" else "(kg or reported unit)")

    # ---- Dependency matrix ---------------------------------------------
    dep_source <- function(direction) paste0(session$ns("dep_"), direction)

    output$dep_exports <- renderPlotly({
      plot_dependency_matrix(conc_exp(), sel()$hs, paste0(FLOW_LABELS[[filt()$export_flow]], " (", filt()$level, ")"),
                             "exports", metric(), unit(), source = dep_source("exports"),
                             highlight_section = if (nzchar(input$highlight)) input$highlight,
                             filename = "export-dependency-matrix")
    })
    output$dep_imports <- renderPlotly({
      plot_dependency_matrix(conc_imp(), sel()$hs, paste0("Imports (", filt()$level, ")"),
                             "imports", metric(), unit(), source = dep_source("imports"),
                             highlight_section = if (nzchar(input$highlight)) input$highlight,
                             filename = "import-dependency-matrix")
    })

    position_table <- reactive({
      hs <- sel()$hs
      validate(need(length(hs) > 0, "Choose a product on the left to see its position."))
      ce <- conc_exp(); ci <- conc_imp()
      one <- function(cc, dir) {
        x <- cc[match(hs, cc$product), ]
        data.frame(
          hhi = round(x$hhi),
          us  = x$us_dependency,
          assessment = ifelse(is.na(x$hhi), "No trade recorded",
                              dependency_quadrant(x$us_dependency, x$hhi,
                                                  mean(cc$us_dependency), mean(cc$hhi)))
        ) |> stats::setNames(paste(dir, c("HHI", "U.S. share", "assessment")))
      }
      cbind(Product = product_label(hs, lk, filt()$level), one(ce, "Export"), one(ci, "Import"))
    })
    output$dep_table <- DT::renderDT({
      DT::datatable(position_table(), rownames = FALSE, fillContainer = FALSE,
                    options = list(dom = "t", pageLength = 50, scrollX = TRUE)) |>
        DT::formatPercentage(c("Export U.S. share", "Import U.S. share"), 1)
    })
    output$dl_dep <- downloadHandler(
      filename = function() paste0("dependency-matrix-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- rbind(with_direction(conc_exp(), "Exports"), with_direction(conc_imp(), "Imports"))
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # Clicking a bubble -> report the product code to the app
    clicked <- reactiveVal(NULL)
    for (dir in c("exports", "imports")) local({
      src <- dep_source(dir)
      # Read the click straight from the input plotly sends (what
      # plotly::event_data() does), which avoids a spurious "not registered"
      # warning while the chart's tab hasn't been opened yet.
      click <- reactive({
        raw <- session$rootScope()$input[[paste0("plotly_click-", src)]]
        if (!is.null(raw)) jsonlite::parse_json(raw, simplifyVector = TRUE)
      })
      observeEvent(click(), {
        ev <- click()
        if (!is.null(ev$customdata)) clicked(list(code = as.character(ev$customdata[[1]]), at = Sys.time()))
      })
    })

    # ---- HHI ------------------------------------------------------------
    hhi_subset <- function(cc) {
      f <- filt()
      hs <- sel()$hs
      if (f$level == "HS6" && length(hs) > 0) {
        cc <- cc[substr(cc$product, 1, 2) %in% unique(substr(hs, 1, 2)), ]
      }
      cc
    }
    hhi_marks <- function(cc) {
      hs <- sel()$hs
      if (length(hs) == 0 || length(hs) > 5) return(NULL)
      x <- cc[cc$product %in% hs, ]
      setNames(x$hhi, x$product)
    }
    hhi_title <- function(direction) {
      f <- filt(); hs <- sel()$hs
      scope <- if (f$level == "HS6" && length(hs) > 0)
        paste0("HS6 products in chapter ", paste(unique(substr(hs, 1, 2)), collapse = ", ")) else
        "All HS2 chapters"
      paste0(direction, " \u2013 ", scope)
    }
    output$hhi_exports <- renderPlotly({
      cc <- hhi_subset(conc_exp())
      plot_hhi_histogram(cc, hhi_title(FLOW_LABELS[[filt()$export_flow]]), hhi_marks(cc), "export-hhi")
    })
    output$hhi_imports <- renderPlotly({
      cc <- hhi_subset(conc_imp())
      plot_hhi_histogram(cc, hhi_title("Imports"), hhi_marks(cc), "import-hhi")
    })
    output$hhi_table <- DT::renderDT({
      hs <- sel()$hs
      validate(need(length(hs) > 0, "Choose a product on the left to see its details."))
      rows <- rbind(
        with_direction(conc_exp()[conc_exp()$product %in% hs, ], "Exports"),
        with_direction(conc_imp()[conc_imp()$product %in% hs, ], "Imports")
      )
      out <- data.frame(
        Product = rows$label,
        Direction = rows$direction,
        HHI = round(rows$hhi),
        Concentration = as.character(rows$concentration),
        `Partner countries` = rows$n_partners,
        `Largest partner` = country_name(rows$top_partner, lk),
        `Largest partner share` = rows$top_partner_share,
        check.names = FALSE
      )
      DT::datatable(out, rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE)) |>
        DT::formatPercentage("Largest partner share", 1)
    })
    output$dl_hhi <- downloadHandler(
      filename = function() paste0("partner-concentration-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- rbind(with_direction(hhi_subset(conc_exp()), "Exports"),
                     with_direction(hhi_subset(conc_imp()), "Imports"))
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # ---- Competitiveness ------------------------------------------------
    comp <- reactive({
      f <- filt()
      out <- calculate_competitiveness(
        query_competitiveness_inputs(con, f, f$export_flow),
        query_competitiveness_inputs(con, f, "imp")
      )
      out$label <- product_label(out$hs, lk, "HS6")
      out
    })
    # HS6 codes to highlight: the selection itself, or the HS6 codes within
    # selected HS2 chapters
    comp_selected <- reactive({
      hs <- sel()$hs
      if (filt()$level == "HS2") comp()$hs[substr(comp()$hs, 1, 2) %in% hs] else hs
    })
    output$comp_plot <- renderPlotly({
      plot_competitiveness(comp(), comp_selected(), "canada-vs-us-competitiveness")
    })
    output$comp_table <- DT::renderDT({
      d <- comp()
      validate(need(length(comp_selected()) > 0, "Choose a product on the left to see its position."))
      x <- d[d$hs %in% comp_selected(), ]
      x <- x[order(-x$bilateral_trade), ]
      out <- data.frame(
        Product = x$label,
        `Canada-U.S. trade` = x$bilateral_trade,
        `Bilateral balance` = round(x$bilateral_balance, 3),
        `Who leads` = balance_description(x$bilateral_balance),
        `Price ratio` = round(x$pci, 2),
        Interpretation = competitiveness_quadrant(x$competitive_disadvantage, x$pci,
                                                  mean(d$competitive_disadvantage), mean(d$pci)),
        check.names = FALSE
      )
      DT::datatable(out, rownames = FALSE, fillContainer = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatCurrency("Canada-U.S. trade", digits = 0)
    })
    output$dl_comp <- downloadHandler(
      filename = function() paste0("canada-vs-us-competitiveness-", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(comp(), file, row.names = FALSE)
    )

    clicked
  })
}
