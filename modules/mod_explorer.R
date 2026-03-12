# =============================================================================
# modules/mod_explorer.R
#
# Shiny module: Yakima Basin Climate & Hydrology Explorer
# Five stacked panels on a shared Date x-axis with rangeslider:
#   1. Monthly Temperature anomaly  (bars, red/blue)
#   2. Monthly Precipitation anomaly (bars, green/tan)
#   3. System Snowpack               (daily line, AF)
#   4. System Reservoir Storage      (daily line + capacity)
#   5. System Combined Storage       (daily line + Oct 1 dots + median)
#
# All data objects come from global.R.
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────

mod_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        plotlyOutput(ns("explorer_plot"), height = "800px", width = "100%")
      )
    ),
    tags$div(
      style = "margin-top:6px; font-size:10px; color:#888; padding:0 4px;
               display:flex; justify-content:space-between;",
      tags$span(
        "Climate: NOAA NCEI, WA Div 6 (East Cascades)  |  ",
        "Snow: NOHRSC/SNODAS  |  ",
        "Storage: USBR Hydromet"
      ),
      tags$span(textOutput(ns("footer_dates"), inline = TRUE))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$explorer_plot <- renderPlotly({
      withProgress(message = "Building explorer...", {
        build_explorer_plot(daily, temp_anom, precip_anom,
                            carryover, carryover_median)
      })
    })

    output$footer_dates <- renderText({
      sprintf(
        "Snow through %s  |  Dam through %s",
        data_freshness$swe_through,
        data_freshness$dam_through
      )
    })

  })
}

# ── Plot builder ──────────────────────────────────────────────────────────────

build_explorer_plot <- function(df_daily, df_temp, df_precip,
                                 df_carryover, co_median) {

  fmt <- function(x) formatC(round(x), format = "d", big.mark = ",")

  # Panel layout — 5 panels, equal height, shared x
  gap <- 0.025
  ph  <- (1 - 4 * gap) / 5   # ~0.181 per panel

  yd <- list(
    p1 = c(4*ph + 4*gap,  1),
    p2 = c(3*ph + 3*gap,  4*ph + 3*gap),
    p3 = c(2*ph + 2*gap,  3*ph + 2*gap),
    p4 = c(ph + gap,      2*ph + gap),
    p5 = c(0,             ph)
  )

  panel_titles <- c(
    "Monthly Temperature Anomaly (\u00b0F vs 1991\u20132020)",
    "Monthly Precipitation Anomaly (in vs 1991\u20132020)",
    "System Snowpack (acre-feet, daily)",
    "System Reservoir Storage (acre-feet, daily)",
    paste0("System Combined Storage (acre-feet, daily)",
           "  \u2022  \u25cf Oct 1 carryover: blue = above median, red = below")
  )

  bar_width_ms <- 30.44 * 24 * 3600 * 1000 * 0.8

  fig  <- plot_ly()
  axes <- list()

  # Shared x-axis base (no tick labels except bottom panel)
  x_base <- list(
    type           = "date",
    showticklabels = FALSE,
    showgrid       = TRUE,
    gridcolor      = col_grid,
    tickfont       = list(size = 9),
    title          = ""
  )

  # ── Panel 1: Temperature anomaly ────────────────────────────────────────────
  fig <- fig %>%
    add_bars(
      data          = df_temp,
      x             = ~bar_date, y = ~anomaly,
      xaxis = "x",  yaxis = "y",
      name          = "Temp Anomaly",
      showlegend    = FALSE,
      width         = bar_width_ms,
      marker        = list(
        color = ifelse(df_temp$anomaly >= 0, col_warm, col_cool),
        line  = list(width = 0)
      ),
      text          = ~paste0(
        format(bar_date, "%b %Y"), "<br>",
        "Anomaly: ", sprintf("%+.1f", anomaly), "\u00b0F"
      ),
      hovertemplate = "%{text}<extra></extra>",
      textposition  = "none"
    )

  axes[["xaxis"]] <- modifyList(x_base, list(anchor = "y"))
  axes[["yaxis"]] <- list(
    domain  = yd$p1, anchor = "x",
    title   = "", zeroline = TRUE, zerolinecolor = "#999999",
    showgrid = TRUE, gridcolor = col_grid,
    tickfont = list(size = 8)
  )

  # ── Panel 2: Precipitation anomaly ──────────────────────────────────────────
  fig <- fig %>%
    add_bars(
      data          = df_precip,
      x             = ~bar_date, y = ~anomaly,
      xaxis = "x2", yaxis = "y2",
      name          = "Precip Anomaly",
      showlegend    = FALSE,
      width         = bar_width_ms,
      marker        = list(
        color = ifelse(df_precip$anomaly >= 0, col_wet, col_dry),
        line  = list(width = 0)
      ),
      text          = ~paste0(
        format(bar_date, "%b %Y"), "<br>",
        "Anomaly: ", sprintf("%+.2f", anomaly), " in"
      ),
      hovertemplate = "%{text}<extra></extra>",
      textposition  = "none"
    )

  axes[["xaxis2"]] <- modifyList(x_base, list(anchor = "y2", matches = "x"))
  axes[["yaxis2"]] <- list(
    domain   = yd$p2, anchor = "x2",
    title    = "", zeroline = TRUE, zerolinecolor = "#999999",
    showgrid = TRUE, gridcolor = col_grid,
    tickfont = list(size = 8)
  )

  # ── Panel 3: Daily snowpack ──────────────────────────────────────────────────
  fig <- fig %>%
    add_lines(
      data          = df_daily,
      x             = ~Date, y = ~snow_af,
      xaxis = "x3", yaxis = "y3",
      line          = list(color = col_snow, width = 1.2),
      showlegend    = FALSE,
      text          = ~paste0(format(Date, "%b %d, %Y"), "<br>",
                              "Snowpack: ", fmt(snow_af), " AF"),
      hovertemplate = "%{text}<extra></extra>"
    )

  axes[["xaxis3"]] <- modifyList(x_base, list(anchor = "y3", matches = "x"))
  axes[["yaxis3"]] <- list(
    domain     = yd$p3, anchor = "x3",
    tickformat = ",.0f", tickfont = list(size = 8),
    title      = "", rangemode = "tozero",
    showgrid   = TRUE, gridcolor = col_grid
  )

  # ── Panel 4: Daily reservoir storage ────────────────────────────────────────
  fig <- fig %>%
    add_lines(
      data          = df_daily,
      x             = ~Date, y = ~dam_af,
      xaxis = "x4", yaxis = "y4",
      line          = list(color = col_dam, width = 1.2),
      showlegend    = FALSE,
      text          = ~paste0(format(Date, "%b %d, %Y"), "<br>",
                              "Reservoir: ", fmt(dam_af), " AF"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    add_segments(
      x    = min(df_daily$Date), xend = max(df_daily$Date),
      y    = system_capacity,    yend = system_capacity,
      xaxis = "x4", yaxis = "y4",
      line       = list(color = col_capacity, width = 1, dash = "dot"),
      showlegend = FALSE,
      text       = paste0("System capacity: ", fmt(system_capacity), " AF"),
      hovertemplate = "%{text}<extra></extra>"
    )

  axes[["xaxis4"]] <- modifyList(x_base, list(anchor = "y4", matches = "x"))
  axes[["yaxis4"]] <- list(
    domain     = yd$p4, anchor = "x4",
    tickformat = ",.0f", tickfont = list(size = 8),
    title      = "", rangemode = "tozero",
    showgrid   = TRUE, gridcolor = col_grid
  )

  # ── Panel 5: Combined storage + Oct 1 dots + median line ────────────────────
  fig <- fig %>%
    add_lines(
      data          = df_daily,
      x             = ~Date, y = ~combined_af,
      xaxis = "x5", yaxis = "y5",
      line          = list(color = col_combined, width = 1.2),
      showlegend    = FALSE,
      text          = ~paste0(format(Date, "%b %d, %Y"), "<br>",
                              "Combined: ", fmt(combined_af), " AF"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    add_markers(
      data       = df_carryover,
      x          = ~Date, y = ~combined_af,
      xaxis = "x5", yaxis = "y5",
      marker     = list(
        color  = I(df_carryover$dot_color),
        size   = 8,
        symbol = "circle",
        line   = list(color = "white", width = 1.5)
      ),
      showlegend = FALSE,
      text = ~paste0(
        "<b>Oct 1 Carryover \u2014 WY", water_year, "</b><br>",
        "Combined storage: ", fmt(combined_af), " AF<br>",
        "Reservoir only: ", fmt(dam_af), " AF<br>",
        pct_median, "% of 1991\u20132020 median (", fmt(co_median), " AF)<br>",
        if_else(above_median, "\u25b2 Above normal", "\u25bc Below normal")
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    add_segments(
      x    = min(df_daily$Date), xend = max(df_daily$Date),
      y    = co_median,           yend = co_median,
      xaxis = "x5", yaxis = "y5",
      line       = list(color = "#aaaaaa", width = 0.8, dash = "dot"),
      showlegend = FALSE,
      text       = paste0("Oct 1 normal (1991\u20132020): ", fmt(co_median), " AF"),
      hovertemplate = "%{text}<extra></extra>"
    )

  axes[["xaxis5"]] <- modifyList(x_base, list(
    anchor         = "y5",
    matches        = "x",
    showticklabels = TRUE,
    tickformat     = "%Y",
    dtick          = "M12",
    rangeslider    = list(
      visible     = TRUE,
      thickness   = 0.05,
      bgcolor     = "#f0f4f8",
      bordercolor = "#cccccc",
      borderwidth = 1
    )
  ))
  axes[["yaxis5"]] <- list(
    domain     = yd$p5, anchor = "x5",
    tickformat = ",.0f", tickfont = list(size = 8),
    title      = "", rangemode = "tozero",
    showgrid   = TRUE, gridcolor = col_grid
  )

  # ── Panel title annotations ──────────────────────────────────────────────────
  ann <- lapply(seq_along(panel_titles), function(i) {
    pd <- yd[[paste0("p", i)]]
    list(
      text      = paste0("<b>", panel_titles[[i]], "</b>"),
      x = 0, xref = "paper",
      y = pd[2], yref = "paper",
      xanchor = "left", yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 10, color = "#444")
    )
  })

  final_layout <- c(
    axes,
    list(
      annotations = ann,
      title = list(
        text = paste0(
          "<b>Yakima Basin \u2014 Climate & Hydrology Explorer</b><br>",
          "<sup>Drag slider to navigate \u2022 ",
          "Scroll/pinch to zoom \u2022 Hover for values</sup>"
        ),
        x = 0.5, font = list(size = 13)
      ),
      margin        = list(t = 70, b = 10, l = 75, r = 20),
      hovermode     = "x unified",
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      bargap        = 0.1
    )
  )

  do.call(plotly::layout, c(list(fig), final_layout)) %>%
    plotly::config(
      responsive             = TRUE,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}
