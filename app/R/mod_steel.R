# ============================================================
# STEEL TAB
# ------------------------------------------------------------
# Only shown when data/reference/steel_categories.csv exists. That file has
# one column per steel category with the category's HS6 codes listed below
# the header (see data/reference/steel_categories_TEMPLATE.csv).
# ============================================================

load_steel_categories <- function(path = STEEL_CSV) {
  if (!file.exists(path)) return(NULL)
  wide <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                          fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
  long <- data.frame(
    group = rep(names(wide), each = nrow(wide)),
    hs    = unlist(wide, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  long$hs <- gsub("[^0-9]", "", long$hs)
  long <- long[!is.na(long$hs) & nzchar(long$hs), ]
  # Spreadsheets drop leading zeros: pad back to six digits
  long$hs <- formatC(as.integer(long$hs), width = 6, flag = "0")
  long <- unique(long)
  if (nrow(long) == 0) return(NULL)
  long
}

steel_tab <- function(id, categories) {
  ns <- NS(id)
  cats <- sort(unique(categories$group))
  nav_panel(
    "Steel",
    card(
      class = "mb-3",
      card_body(
        class = "d-flex flex-wrap gap-3 align-items-end",
        selectizeInput(ns("categories"), "Steel categories", choices = cats, selected = cats[1],
                       multiple = TRUE, width = "100%",
                       options = list(plugins = list("remove_button"),
                                      placeholder = "Pick one or more categories")),
        div(class = "text-muted small",
            "Uses HS6 data for the categories' products, together with the years, region and ",
            "other settings on the left.")
      )
    ),
    do.call(navset_pill, c(
      list(id = ns("tabs")),
      explorer_tabs(ns("explorer"), noun = "Category"),
      list(nav_panel(
        "Category Comparison",
        div(class = "text-muted small my-2",
            "How every steel category compares on partner concentration and reliance on the U.S. ",
            "Your selected categories are highlighted."),
        card(
          card_header(class = "d-flex",
                      div(class = "ms-auto",
                          downloadButton(ns("dl_compare"), "Download chart data",
                                         class = "btn-sm btn-outline-secondary"))),
          card_body(
            layout_columns(
              col_widths = breakpoints(sm = 12, lg = c(6, 6)),
              plotlyOutput(ns("dep_exports"), height = "480px"),
              plotlyOutput(ns("dep_imports"), height = "480px"),
              plotlyOutput(ns("hhi_exports"), height = "420px"),
              plotlyOutput(ns("hhi_imports"), height = "420px")
            )
          )
        )
      ))
    ))
  )
}

steel_server <- function(id, con, lk, filt, opts, categories) {
  moduleServer(id, function(input, output, session) {

    steel_filt <- reactive({
      f <- filt()
      f$level <- "HS6"
      f
    })

    sel <- reactive({
      cats <- input$categories
      groups <- categories[categories$group %in% cats, ]
      list(level = "HS6", hs = unique(groups$hs), groups = groups, noun = "Category",
           title = short_product_text(cats, 2))
    })

    explorer_server("explorer", con, lk, steel_filt, sel, opts)

    # ---- Category comparison -------------------------------------------
    conc <- function(flow) {
      pt <- query_partner_totals(con, steel_filt(), flow)
      pt <- dplyr::inner_join(pt, categories, by = "hs", relationship = "many-to-many")
      out <- summarise_concentration(pt, group_col = "group")
      out$label <- out$product
      out$section <- "Steel category"
      out
    }
    conc_exp <- reactive(conc(filt()$export_flow))
    conc_imp <- reactive(conc("imp"))
    unit <- reactive(if (filt()$metric == "value") "" else "kg")

    output$dep_exports <- renderPlotly({
      plot_dependency_matrix(conc_exp(), input$categories, FLOW_LABELS[[filt()$export_flow]],
                             "exports", filt()$metric, unit(), source = session$ns("dep_exports"),
                             filename = "steel-export-dependency", noun = "categories")
    })
    output$dep_imports <- renderPlotly({
      plot_dependency_matrix(conc_imp(), input$categories, "Imports",
                             "imports", filt()$metric, unit(), source = session$ns("dep_imports"),
                             filename = "steel-import-dependency", noun = "categories")
    })
    output$hhi_exports <- renderPlotly({
      plot_hhi_bars(conc_exp(), paste(FLOW_LABELS[[filt()$export_flow]], "\u2013 partner concentration"),
                    "steel-export-hhi")
    })
    output$hhi_imports <- renderPlotly({
      plot_hhi_bars(conc_imp(), "Imports \u2013 partner concentration", "steel-import-hhi")
    })
    output$dl_compare <- downloadHandler(
      filename = function() paste0("steel-category-comparison-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- rbind(with_direction(conc_exp(), "Exports"), with_direction(conc_imp(), "Imports"))
        names(out)[names(out) == "product"] <- "category"
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
  })
}
