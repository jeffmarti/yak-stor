# =============================================================================
# modules/mod_storage.R
#
# Shiny module: Yakima Basin Water Storage Dashboard
# Six-panel plotly chart (BUM, CLE, KAC, KEE, RIM, ALL) for a selected
# water year. All data objects come from global.R.
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────

mod_storage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "control-row",
      div(style = "display:flex; align-items:center; gap:12px;",
        tags$label("Water Year:", style = "font-weight:600; margin:0; font-size:13px;"),
        selectInput(
          ns("water_year"),
          label   = NULL,
          choices = setNames(all_wy, paste("WY", all_wy)),
          selected = current_wy,
          width   = "120px"
        )
      ),
      div(style = "font-size:11px; color:#555;",
        textOutput(ns("data_note"), inline = TRUE)
      )
    ),
    fluidRow(
      column(12,
        plotlyOutput(ns("storage_plot"), height = "780px", width = "100%")
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_storage_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Data note ────────────────────────────────────────────────────────────
    output$data_note <- renderText({
      wy  <- as.integer(input$water_year)
      d   <- plot_data %>% filter(water_year == wy, reservoir == "ALL")
      src <- swe_raw %>%
        filter(
          Date %in% (plot_data %>% filter(water_year == wy) %>% pull(Date)),
          reservoir != "ALL"
        ) %>%
        pull(source) %>% unique() %>% na.omit()
      src_str <- if (length(src) > 0) paste(unique(src), collapse = " + ") else "NOHRSC"
      sprintf("WY%d  |  %d days  |  Snow: %s", wy, nrow(d), src_str)
    })

    # ── Plot ──────────────────────────────────────────────────────────────────
    output$storage_plot <- renderPlotly({
      wy <- as.integer(input$water_year)
      withProgress(message = sprintf("Building WY%d...", wy), {
        make_storage_plot(wy)
      })
    })

  })
}

# ── Plot builder ──────────────────────────────────────────────────────────────
# Builds a 2×3 grid of per-reservoir stacked-area plots for a given water year.

make_storage_plot <- function(wy) {

  x_domains <- list(c(0.00, 0.46), c(0.54, 1.00))
  y_domains <- list(c(0.72, 1.00), c(0.36, 0.64), c(0.00, 0.28))

  grid <- list(
    list(xi = 1, yi = 1), list(xi = 2, yi = 1),   # BUM, CLE
    list(xi = 1, yi = 2), list(xi = 2, yi = 2),   # KAC, KEE
    list(xi = 1, yi = 3), list(xi = 2, yi = 3)    # RIM, ALL
  )
  ax_suffix <- c("", "2", "3", "4", "5", "6")

  fig            <- plot_ly()
  all_annotations <- vector("list", length(res_order))
  all_axes       <- list()

  for (i in seq_along(res_order)) {
    res_code  <- res_order[i]
    suf       <- ax_suffix[i]
    xax_name  <- paste0("xaxis", suf)
    yax_name  <- paste0("yaxis", suf)
    xref      <- paste0("x", suf)
    yref      <- paste0("y", suf)
    show_leg  <- (i == 1)
    lbl       <- reservoir_labels[[res_code]]
    cap       <- reservoir_capacity[[res_code]]
    xd        <- x_domains[[grid[[i]]$xi]]
    yd        <- y_domains[[grid[[i]]$yi]]

    d      <- plot_data   %>% filter(water_year == wy, reservoir == res_code) %>% arrange(wy_day)
    d_norm <- dam_normal  %>% filter(reservoir == res_code) %>% arrange(wy_day)
    s_norm <- snow_normal %>% filter(reservoir == res_code) %>% arrange(wy_day)

    # ── Dam storage fill (0 → dam_top) ──────────────────────────────────────
    fig <- fig %>% add_trace(
      data          = d,
      x             = ~wy_day, y = ~dam_top,
      xaxis         = xref, yaxis = yref,
      type          = "scatter", mode = "none",
      fill          = "tozeroy",
      fillcolor     = paste0(col_dam, "D9"),
      name          = "Dam Storage",
      showlegend    = show_leg,
      legendgroup   = "dam",
      hovertemplate = "Dam Storage: %{y:,.0f} AF<extra></extra>"
    )

    # ── Snow storage fill (dam_top → snow_top) ───────────────────────────────
    d <- d %>% mutate(
      snow_hover = paste0(
        "Snow Storage: ",
        formatC(round(snow_storage_acre_feet), format = "d", big.mark = ","),
        " AF | Total: ",
        formatC(round(snow_top), format = "d", big.mark = ","),
        " AF"
      )
    )
    fig <- fig %>% add_trace(
      data          = d,
      x             = ~wy_day, y = ~snow_top,
      xaxis         = xref, yaxis = yref,
      type          = "scatter", mode = "none",
      fill          = "tonexty",
      fillcolor     = paste0(col_snow, "D9"),
      name          = "Snow Storage (SWE)",
      showlegend    = show_leg,
      legendgroup   = "snow",
      text          = ~snow_hover,
      hovertemplate = "%{text}<extra></extra>"
    )

    # ── Dam Avg 1991–2020 (black dashed) ─────────────────────────────────────
    fig <- fig %>% add_trace(
      data          = d_norm,
      x             = ~wy_day, y = ~dam_normal_af,
      xaxis         = xref, yaxis = yref,
      type          = "scatter", mode = "lines",
      line          = list(color = col_dam_normal, width = 1.2, dash = "dash"),
      name          = "Dam Avg (1991\u20132020)",
      showlegend    = show_leg,
      legendgroup   = "dam_normal",
      hovertemplate = "Dam Avg (1991\u20132020): %{y:,.0f} AF<extra></extra>"
    )

    # ── Snow Avg 2004–2025 (green dashed) ────────────────────────────────────
    fig <- fig %>% add_trace(
      data          = s_norm,
      x             = ~wy_day, y = ~snow_normal_af,
      xaxis         = xref, yaxis = yref,
      type          = "scatter", mode = "lines",
      line          = list(color = col_snow_normal, width = 1.2, dash = "dash"),
      name          = "Snow Avg (2004\u20132025)",
      showlegend    = show_leg,
      legendgroup   = "snow_normal",
      hovertemplate = "Snow Avg (2004\u20132025): %{y:,.0f} AF<extra></extra>"
    )

    # ── Dam Capacity (red dashed) ─────────────────────────────────────────────
    fig <- fig %>% add_segments(
      x    = 1,   xend = 366,
      y    = cap, yend = cap,
      xaxis = xref, yaxis = yref,
      line          = list(color = col_capacity, width = 1.2, dash = "dash"),
      name          = "Dam Capacity",
      showlegend    = show_leg,
      legendgroup   = "cap",
      hovertemplate = paste0(
        "Dam Capacity: ",
        formatC(cap, format = "d", big.mark = ","),
        " AF<extra></extra>"
      )
    )

    # ── Panel title annotation ────────────────────────────────────────────────
    all_annotations[[i]] <- list(
      text      = paste0("<b>", lbl, "</b>"),
      x         = mean(xd), xref = "paper",
      y         = yd[2] + 0.01, yref = "paper",
      xanchor   = "center", yanchor = "bottom",
      showarrow = FALSE,
      font      = list(size = 11)
    )

    # ── Axis definitions ──────────────────────────────────────────────────────
    all_axes[[xax_name]] <- list(
      domain   = xd,
      anchor   = yref,
      tickmode = "array",
      tickvals = wy_months$day_start,
      ticktext = wy_months$label,
      range    = c(0, 367),
      showgrid = FALSE,
      tickfont = list(size = 8),
      title    = ""
    )
    all_axes[[yax_name]] <- list(
      domain     = yd,
      anchor     = xref,
      tickformat = ",.0f",
      rangemode  = "tozero",
      tickfont   = list(size = 8),
      title      = ""
    )
  }

  final_layout <- c(
    all_axes,
    list(
      annotations = all_annotations,
      title = list(
        text = paste0(
          "<b>Yakima Basin Water Storage \u2014 Water Year ", wy, "</b><br>",
          "<sup>Blue = dam storage \u00a0|\u00a0 ",
          "Light blue = snow (SWE) stacked \u00a0|\u00a0 ",
          "Red dashed = dam capacity \u00a0|\u00a0 ",
          "Black dashed = dam avg (1991\u20132020) \u00a0|\u00a0 ",
          "Green dashed = snow avg (not stacked) (2004\u20132025)</sup>"
        ),
        x = 0.5, font = list(size = 13)
      ),
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.04,
        font = list(size = 11)
      ),
      autosize      = TRUE,
      margin        = list(t = 80, b = 60, l = 60, r = 20),
      hovermode     = "x",
      paper_bgcolor = "white",
      plot_bgcolor  = "white"
    )
  )

  do.call(plotly::layout, c(list(fig), final_layout)) %>%
    plotly::config(responsive = TRUE)
}

