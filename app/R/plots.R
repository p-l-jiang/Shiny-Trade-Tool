# ============================================================
# PLOTS
# ------------------------------------------------------------
# Plotly chart builders. Each takes a prepared data frame and returns a
# plotly object; no Shiny code lives here.
# ============================================================

FONT <- list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
             size = 13, color = "#0b0b0b")
GRID <- "#e9e8e4"

base_layout <- function(p, ...) {
  plotly::layout(
    p,
    font = FONT,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    margin = list(l = 10, r = 10, t = 40, b = 10),
    hoverlabel = list(font = list(family = FONT$family)),
    ...
  )
}

plot_config <- function(p, filename = "trade-explorer-chart") {
  plotly::config(
    p,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                               "hoverClosestCartesian", "hoverCompareCartesian",
                               "toggleSpikelines"),
    toImageButtonOptions = list(format = "png", filename = filename, scale = 2)
  )
}

empty_plot <- function(message) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    base_layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = message, showarrow = FALSE,
                              font = list(size = 15, color = "#52514e"),
                              xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# Bubble diameters in pixels (6-30px) from log trade size. Computed here rather
# than with plotly's sizeref, which is ignored for single-point traces.
bubble_px <- function(total) {
  z <- log10(pmax(total, 0) + 1)
  rng <- range(z, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(14, length(total)))
  6 + 24 * (z - rng[1]) / diff(rng)
}

# ------------------------------------------------------------
# Time series (one direction)
# ------------------------------------------------------------
# d: period, series (factor), metric
plot_time_series <- function(d, title, metric, unit, freq, filename) {
  if (nrow(d) == 0) return(empty_plot("No trade recorded for this selection"))

  sc <- axis_scale(tapply(d$metric, list(d$period, d$series), sum))
  d$y <- d$metric / sc$div
  d$hover <- paste0(d$series, ": ", fmt_metric(d$metric, metric, unit))
  cols <- series_colours(levels(d$series))

  p <- plotly::plot_ly()
  for (s in levels(d$series)) {
    ds <- d[d$series == s, ]
    ds <- ds[order(ds$period), ]
    p <- plotly::add_trace(
      p, data = ds, x = ~period, y = ~y, name = s,
      type = "scatter", mode = if (freq == "year") "lines+markers" else "lines",
      line = list(color = cols[[s]], width = 2),
      marker = if (freq == "year") list(color = cols[[s]], size = 8),
      text = ~hover, hoverinfo = "text"
    )
  }

  # Date axis that never shows clock times, even when only one period is shown
  xaxis <- list(title = "", type = "date", gridcolor = GRID, zeroline = FALSE,
                tickformat = if (freq == "year") "%Y" else "%b %Y",
                hoverformat = if (freq == "year") "%Y" else "%b %Y")
  if (freq == "year") xaxis$dtick <- "M12"
  periods <- range(d$period)
  if (periods[1] == periods[2]) {
    pad <- switch(freq, month = 20, quarter = 60, year = 200)
    xaxis$range <- as.character(c(periods[1] - pad, periods[2] + pad))
    xaxis$dtick <- switch(freq, month = "M1", quarter = "M3", year = "M12")
  }

  p |>
    base_layout(
      title = list(text = title, x = 0, xanchor = "left", font = list(size = 15)),
      xaxis = xaxis,
      yaxis = list(title = metric_axis_title(metric, unit, sc), gridcolor = GRID,
                   rangemode = "tozero", zeroline = FALSE),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.12, x = 0)
    ) |>
    plot_config(filename)
}

# ------------------------------------------------------------
# Market share (horizontal bars, one direction)
# ------------------------------------------------------------
# d: series (factor, ordered largest first, "Other" last), total
plot_share_bars <- function(d, title, metric, unit, filename) {
  if (nrow(d) == 0 || sum(d$total) <= 0) return(empty_plot("No trade recorded for this selection"))

  d$share <- d$total / sum(d$total)
  d$label <- fmt_pct(d$share, 0.1)
  d$hover <- paste0("<b>", d$series, "</b><br>", d$label, " of total<br>",
                    fmt_metric(d$total, metric, unit))
  cols <- series_colours(levels(d$series))
  # plotly draws the first category at the bottom: reverse so the largest is on top
  d$series <- factor(d$series, levels = rev(levels(d$series)))

  plotly::plot_ly(
    d, x = ~share, y = ~series, type = "bar", orientation = "h",
    marker = list(color = unname(cols[as.character(d$series)])),
    text = ~label, textposition = "outside", cliponaxis = FALSE,
    hovertext = ~hover, hoverinfo = "text"
  ) |>
    base_layout(
      title = list(text = title, x = 0, xanchor = "left", font = list(size = 15)),
      xaxis = list(title = "Share of total", tickformat = ".0%", gridcolor = GRID,
                   range = c(0, min(1, max(d$share) * 1.2)), zeroline = FALSE),
      yaxis = list(title = "", automargin = TRUE),
      bargap = 0.35,
      showlegend = FALSE
    ) |>
    plot_config(filename)
}

# ------------------------------------------------------------
# Dependency matrix: HHI vs U.S. dependency, one bubble per product
# ------------------------------------------------------------
# d: product, hhi, us_dependency, total, label (hover name)
plot_dependency_matrix <- function(d, selected, title, direction, metric, unit,
                                   source, highlight_section = NULL, filename,
                                   noun = "products") {
  if (nrow(d) == 0) return(empty_plot("No trade recorded for this selection"))

  avg_hhi <- mean(d$hhi, na.rm = TRUE)
  avg_us  <- mean(d$us_dependency, na.rm = TRUE)
  d$size  <- bubble_px(d$total)
  if (!"section" %in% names(d)) d$section <- hs_section(d$product)
  d$hover <- paste0(
    "<b>", d$label, "</b>",
    "<br>", d$section,
    "<br>HHI: ", round(d$hhi), " (", d$concentration, ")",
    "<br>U.S. share: ", fmt_pct(d$us_dependency),
    "<br>Total: ", fmt_metric(d$total, metric, unit)
  )

  is_sel <- d$product %in% selected
  is_hl  <- !is_sel & !is.null(highlight_section) & d$section %in% highlight_section

  p <- plotly::plot_ly(source = source)
  add_bubbles <- function(p, dd, name, colour, opacity, show_text = FALSE) {
    if (nrow(dd) == 0) return(p)
    plotly::add_trace(
      p, data = dd, x = ~hhi, y = ~us_dependency, customdata = ~product,
      type = "scatter", mode = if (show_text) "markers+text" else "markers",
      name = name,
      text = if (show_text) ~product, textposition = if (show_text) "top center",
      textfont = if (show_text) list(color = "#0b0b0b", size = 12),
      hovertext = ~hover, hoverinfo = "text",
      marker = list(color = colour, opacity = opacity,
                    size = ~size, line = list(color = "#ffffff", width = 1))
    )
  }
  p <- add_bubbles(p, d[!is_sel & !is_hl, ], paste("Other", noun), MUTED_COLOUR, 0.6)
  p <- add_bubbles(p, d[is_hl, ], paste("Highlighted:", paste(highlight_section, collapse = ", ")),
                   ACCENT_COLOUR, 0.75)
  p <- add_bubbles(p, d[is_sel, ], "Your selection", HIGHLIGHT_COLOUR, 0.95, show_text = TRUE)

  ref_line <- function(x0, x1, y0, y1, xref = "x", yref = "y") {
    list(type = "line", x0 = x0, x1 = x1, y0 = y0, y1 = y1, xref = xref, yref = yref,
         line = list(color = "#52514e", width = 1, dash = "dash"))
  }

  p |>
    base_layout(
      title = list(text = title, x = 0, xanchor = "left", font = list(size = 15)),
      xaxis = list(title = "Concentration of partners (HHI)", gridcolor = GRID,
                   zeroline = FALSE, rangemode = "tozero"),
      yaxis = list(title = paste("U.S. share of", tolower(direction)), tickformat = ".0%",
                   range = c(-0.05, 1.08), gridcolor = GRID, zeroline = FALSE),
      shapes = list(ref_line(avg_hhi, avg_hhi, 0, 1, yref = "paper"),
                    ref_line(0, 1, avg_us, avg_us, xref = "paper")),
      legend = list(orientation = "h", y = -0.25, x = 0)
    ) |>
    plotly::event_register("plotly_click") |>
    plot_config(filename)
}

# ------------------------------------------------------------
# HHI distribution
# ------------------------------------------------------------
# d: hhi, concentration; marks: named vector of HHI values to mark
plot_hhi_histogram <- function(d, title, marks = NULL, filename) {
  if (nrow(d) == 0) return(empty_plot("No trade recorded for this selection"))

  p <- plotly::plot_ly()
  for (lev in levels(d$concentration)) {
    dd <- d[d$concentration == lev, ]
    if (nrow(dd) == 0) next
    p <- plotly::add_histogram(
      p, x = dd$hhi, name = lev, marker = list(color = CONCENTRATION_COLOURS[[lev]],
                                               line = list(color = "#ffffff", width = 1)),
      xbins = list(start = 0, end = 10000, size = 250),
      hovertemplate = paste0(lev, "<br>HHI %{x}<br>%{y} products<extra></extra>")
    )
  }

  shapes <- list(
    list(type = "line", x0 = HHI_MODERATE, x1 = HHI_MODERATE, y0 = 0, y1 = 1, yref = "paper",
         line = list(color = "#52514e", width = 1, dash = "dot")),
    list(type = "line", x0 = HHI_HIGH, x1 = HHI_HIGH, y0 = 0, y1 = 1, yref = "paper",
         line = list(color = "#52514e", width = 1, dash = "dot"))
  )
  annotations <- list()
  for (i in seq_along(marks)) {
    nm <- names(marks)[i]
    shapes <- c(shapes, list(list(type = "line", x0 = marks[[nm]], x1 = marks[[nm]],
                                  y0 = 0, y1 = 1, yref = "paper",
                                  line = list(color = ACCENT_COLOUR, width = 2))))
    # stagger labels so neighbouring products don't overlap
    annotations <- c(annotations, list(list(x = marks[[nm]], y = 1 + 0.06 * ((i - 1) %% 2),
                                            yref = "paper",
                                            text = nm, showarrow = FALSE, yanchor = "bottom",
                                            font = list(size = 11, color = "#0b0b0b"))))
  }

  p |>
    base_layout(
      title = list(text = title, x = 0, xanchor = "left", font = list(size = 15)),
      barmode = "stack",
      xaxis = list(title = "Concentration of partners (HHI)", range = c(0, 10000),
                   gridcolor = GRID, zeroline = FALSE),
      yaxis = list(title = "Number of products", gridcolor = GRID, zeroline = FALSE),
      shapes = shapes, annotations = annotations,
      legend = list(orientation = "h", y = -0.25, x = 0)
    ) |>
    plot_config(filename)
}

# HHI per group as bars (used when there are only a handful of groups, e.g.
# steel categories)
plot_hhi_bars <- function(d, title, filename) {
  if (nrow(d) == 0) return(empty_plot("No trade recorded for this selection"))
  d <- d[order(d$hhi), ]
  d$product <- factor(d$product, levels = d$product)
  plotly::plot_ly(
    d, x = ~hhi, y = ~product, type = "bar", orientation = "h",
    marker = list(color = unname(CONCENTRATION_COLOURS[as.character(d$concentration)])),
    text = ~round(hhi), textposition = "outside", cliponaxis = FALSE,
    hovertext = ~paste0("<b>", product, "</b><br>HHI: ", round(hhi), " (", concentration, ")",
                        "<br>Partners: ", n_partners,
                        "<br>U.S. share: ", fmt_pct(us_dependency)),
    hoverinfo = "text"
  ) |>
    base_layout(
      title = list(text = title, x = 0, xanchor = "left", font = list(size = 15)),
      xaxis = list(title = "Concentration of partners (HHI)", gridcolor = GRID, zeroline = FALSE,
                   range = c(0, max(10000, max(d$hhi) * 1.1))),
      yaxis = list(title = "", automargin = TRUE),
      shapes = list(
        list(type = "line", x0 = HHI_MODERATE, x1 = HHI_MODERATE, y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#52514e", width = 1, dash = "dot")),
        list(type = "line", x0 = HHI_HIGH, x1 = HHI_HIGH, y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#52514e", width = 1, dash = "dot"))
      ),
      bargap = 0.35, showlegend = FALSE
    ) |>
    plot_config(filename)
}

# ------------------------------------------------------------
# Canada vs U.S. competitiveness
# ------------------------------------------------------------
# d: hs, label, pci, competitive_disadvantage, bilateral_trade
plot_competitiveness <- function(d, selected, filename) {
  if (nrow(d) == 0) return(empty_plot("Not enough price data for this selection"))

  avg_pci <- mean(d$pci, na.rm = TRUE)
  avg_cd  <- mean(d$competitive_disadvantage, na.rm = TRUE)
  d$size  <- bubble_px(d$bilateral_trade)
  d$hover <- paste0(
    "<b>", d$label, "</b>",
    "<br>Comparative disadvantage: ", round(d$competitive_disadvantage, 3),
    "<br>Price ratio (export / import): ", round(d$pci, 2),
    "<br>Canada\u2013U.S. trade: ", fmt_dollars(d$bilateral_trade)
  )
  is_sel <- d$hs %in% selected

  p <- plotly::plot_ly()
  add_bubbles <- function(p, dd, name, colour, opacity, show_text = FALSE) {
    if (nrow(dd) == 0) return(p)
    plotly::add_trace(
      p, data = dd, x = ~pci, y = ~competitive_disadvantage,
      type = "scatter", mode = if (show_text) "markers+text" else "markers", name = name,
      text = if (show_text) ~hs, textposition = if (show_text) "top center",
      hovertext = ~hover, hoverinfo = "text",
      marker = list(color = colour, opacity = opacity, size = ~size,
                    line = list(color = "#ffffff", width = 1))
    )
  }
  p <- add_bubbles(p, d[!is_sel, ], "Other products", MUTED_COLOUR, 0.6)
  p <- add_bubbles(p, d[is_sel, ], "Your selection", HIGHLIGHT_COLOUR, 0.95, show_text = sum(is_sel) <= 15)

  quad <- function(x, y, text) list(x = x, y = y, xref = "paper", yref = "paper", text = text,
                                    xanchor = if (x > 0.5) "right" else "left",
                                    yanchor = if (y > 0.5) "top" else "bottom",
                                    showarrow = FALSE, font = list(size = 11, color = "#52514e"))
  p |>
    base_layout(
      title = list(text = "Canada vs U.S. competitive position (HS6, value)", x = 0,
                   xanchor = "left", font = list(size = 15)),
      xaxis = list(title = "Price ratio (Canada's export price \u00f7 import price)",
                   gridcolor = GRID, zeroline = FALSE),
      yaxis = list(title = "Comparative disadvantage<br>(0 = Canada dominates, 1 = U.S. dominates)",
                   range = c(-0.05, 1.05), gridcolor = GRID, zeroline = FALSE),
      shapes = list(
        list(type = "line", x0 = avg_pci, x1 = avg_pci, y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#52514e", width = 1, dash = "dash")),
        list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = avg_cd, y1 = avg_cd,
             line = list(color = "#52514e", width = 1, dash = "dash"))
      ),
      annotations = list(
        quad(0.02, 0.98, "Competitively priced<br>but less specialized"),
        quad(0.98, 0.98, "More expensive and<br>limited advantage"),
        quad(0.02, 0.02, "Strong specialization<br>with competitive pricing"),
        quad(0.98, 0.02, "More expensive but<br>strong advantage")
      ),
      legend = list(orientation = "h", y = -0.25, x = 0)
    ) |>
    plot_config(filename)
}
