# ==============================================================================
# modules/mod_forecast.R
#
# Shiny module: Yakima Basin Runoff Outlook
#
# Three-panel layout telling the complete storage-to-delivery story:
#
#   Panel 1 (top):    Scatter — April 1 snowpack vs Apr-Sep NWRFC natural flow
#                     Regression + 95% CI. Current WY Apr 1 snowpack as predictor.
#
#   Panel 2 (middle): Bar — Monthly NWRFC natural flow, current WY Apr-Sep,
#                     compared against historical percentile ribbon (2004-present).
#
#   Panel 3 (bottom): Stacked bar — Apr-Sep delivery composition by water year.
#                     Natural flow (NWRFC) + reservoir augmentation (BOR QD - NWRFC).
#                     Normal reference lines for both components.
#
# NOTE: Statistical estimation tool only. For official forecasts consult NWRFC/NRCS.
#
# All data objects come from global.R:
#   daily           — daily combined storage (snow_af + dam_af)
#   apr1_snow       — April 1 snowpack AF per water year
#   nwrfc_aprsep    — Apr-Sep NWRFC natural flow AF per water year
#   bor_qd_aprsep   — Apr-Sep BOR QD observed discharge AF per water year
#   runoff_raw      — monthly NWRFC by reservoir
#   current_wy, max_date, NORMAL_START, NORMAL_END, SNOW_YR_START
# ==============================================================================

# -- UI ------------------------------------------------------------------------

mod_forecast_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(class = "control-row",
        div(
          style = "font-size:13px; font-weight:600; color:#1a3a5c;",
          textOutput(ns("as_of_label"), inline = TRUE)
        ),
        div(
          style = "font-size:11px; color:#555;",
          "Predictor: April 1 snowpack (SWE) | Response: Apr\u2013Sep natural flow (NWRFC)"
        )
    ),
    
    fluidRow(
      column(12,
             plotlyOutput(ns("scatter_plot"), height = "400px", width = "100%")
      )
    ),
    
    fluidRow(
      column(12,
             plotlyOutput(ns("monthly_plot"), height = "300px", width = "100%")
      )
    ),
    
    fluidRow(
      column(12,
             plotlyOutput(ns("delivery_plot"), height = "340px", width = "100%")
      )
    ),
    
    tags$div(
      style = paste0(
        "margin-top:8px; font-size:10px; color:#888; padding:0 4px;",
        "line-height:1.6; border-top:1px solid #eee; padding-top:6px;"
      ),
      tags$strong("Panel 1:"),
      " April 1 snowpack vs Apr\u2013Sep natural flow. R\u00b2 = 0.74.",
      " Shaded band = 95% confidence interval. Water years 2004\u2013present.",
      tags$br(),
      tags$strong("Panel 2:"),
      " Monthly NWRFC natural flow for the current water year (Apr\u2013Sep).",
      " Ribbon = historical range (10th\u201390th percentile, 2004\u2013present).",
      tags$br(),
      tags$strong("Panel 3:"),
      " Apr\u2013Sep delivery composition by water year.",
      " Blue = NWRFC natural flow. Tan = reservoir augmentation (BOR observed \u2212 natural flow).",
      " Dashed lines = 2004\u20132present normals.",
      tags$br(),
      "Natural flow source: NWRFC. Observed delivery source: USBR Hydromet (QD).",
      " For official seasonal forecasts consult ",
      tags$a("NWRFC", href = "https://www.nwrfc.noaa.gov", target = "_blank"),
      " and ",
      tags$a("NRCS.", href = "https://nwcc-apps.sc.egov.usda.gov/forecast-plots/?state=WA",
             target = "_blank")
    )
  )
}

# -- Server --------------------------------------------------------------------

mod_forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$as_of_label <- renderText({
      paste0("Runoff Outlook \u2014 WY", current_wy,
             " as of ", format(max_date, "%B %d, %Y"))
    })
    
    output$scatter_plot <- renderPlotly({
      withProgress(message = "Building scatter plot...", {
        build_scatter_plot()
      })
    })
    
    output$monthly_plot <- renderPlotly({
      withProgress(message = "Building monthly flow chart...", {
        build_monthly_plot()
      })
    })
    
    output$delivery_plot <- renderPlotly({
      withProgress(message = "Building delivery composition chart...", {
        build_delivery_plot()
      })
    })
    
  })
}

# ==============================================================================
# PANEL 1: April 1 snowpack → Apr-Sep NWRFC natural flow (scatter + regression)
# ==============================================================================

build_scatter_plot <- function() {
  
  fmt_kaf     <- function(x) formatC(round(x / 1000), format = "d", big.mark = ",")
  fmt_kaf_x   <- function(x) formatC(round(x),        format = "d", big.mark = ",")
  today_label <- format(max_date, "%B %d, %Y")
  seas        <- "Apr\u2013Sep"
  
  # -- Historical regression data ----------------------------------------------
  # Join Apr 1 snowpack predictor with Apr-Sep NWRFC response
  # Exclude current WY from regression (incomplete season)
  hist_df <- apr1_snow %>%
    filter(water_year >= SNOW_YR_START,
           water_year <  current_wy) %>%
    inner_join(nwrfc_aprsep, by = "water_year") %>%
    mutate(
      x_kaf = snow_apr1_af / 1000,
      y_kaf = nwrfc_af     / 1000,
      label = paste0(
        "<b>WY", water_year, "</b><br>",
        "Apr 1 snowpack: ", fmt_kaf_x(x_kaf), " KAF<br>",
        seas, " natural flow: ", fmt_kaf_x(y_kaf), " KAF"
      )
    )
  
  if (nrow(hist_df) < 4) {
    return(plot_ly() %>%
             plotly::layout(
               title = "Insufficient historical data",
               paper_bgcolor = "white", plot_bgcolor = "white"
             ))
  }
  
  # -- Current WY Apr 1 snowpack -----------------------------------------------
  curr_snow_af <- apr1_snow %>%
    filter(water_year == current_wy) %>%
    pull(snow_apr1_af)
  
  # Fallback: use nearest date within 5 days if Apr 1 missing
  if (length(curr_snow_af) == 0) {
    curr_snow_af <- daily %>%
      mutate(
        water_year = if_else(month(Date) >= 10, year(Date) + 1L, year(Date)),
        mo_day     = format(Date, "%m-%d")
      ) %>%
      filter(water_year == current_wy) %>%
      mutate(days_diff = abs(as.integer(Date - as.Date(paste0(
        year(Date), "-04-01"))))) %>%
      arrange(days_diff) %>%
      slice(1) %>%
      pull(snow_af)
  }
  
  curr_x_kaf <- if (length(curr_snow_af) > 0) curr_snow_af / 1000 else NA_real_
  
  # -- Apr-Sep natural flow normal (NORMAL_START-NORMAL_END) -------------------
  normal_kaf <- nwrfc_aprsep %>%
    filter(water_year > NORMAL_START, water_year <= NORMAL_END + 1) %>%
    summarise(n = mean(nwrfc_af, na.rm = TRUE)) %>%
    pull(n) / 1000
  
  # -- Regression + 95% CI -----------------------------------------------------
  fit   <- lm(y_kaf ~ x_kaf, data = hist_df)
  r2    <- summary(fit)$r.squared
  slope <- coef(fit)[["x_kaf"]]
  
  x_range <- range(c(hist_df$x_kaf,
                     if (!is.na(curr_x_kaf)) curr_x_kaf else numeric(0)))
  x_seq   <- seq(x_range[1] * 0.85, x_range[2] * 1.15, length.out = 120)
  
  pred_ci <- predict(fit,
                     newdata  = data.frame(x_kaf = x_seq),
                     interval = "confidence",
                     level    = 0.95) %>%
    as.data.frame() %>%
    mutate(x_kaf = x_seq)
  
  # Current WY prediction
  curr_est_kaf <- curr_ci_lo <- curr_ci_hi <- NA_real_
  if (!is.na(curr_x_kaf)) {
    curr_pred    <- predict(fit,
                            newdata  = data.frame(x_kaf = curr_x_kaf),
                            interval = "confidence",
                            level    = 0.95)
    curr_est_kaf <- curr_pred[1]
    curr_ci_lo   <- curr_pred[2]
    curr_ci_hi   <- curr_pred[3]
  }
  
  reg_label <- sprintf(
    "Regression  R\u00b2 = %.2f  |  slope = %.2f KAF runoff per KAF snowpack",
    r2, slope
  )
  
  # -- Colors ------------------------------------------------------------------
  hist_color   <- col_snow     # light blue — snowpack theme
  curr_color   <- col_warm     # red for current year
  normal_color <- "#555555"
  ci_fill      <- "rgba(146, 197, 222, 0.20)"   # col_snow with alpha
  
  # -- Build plot --------------------------------------------------------------
  fig <- plot_ly()
  
  # CI ribbon
  fig <- fig %>%
    add_ribbons(
      data       = pred_ci,
      x          = ~x_kaf, ymin = ~lwr, ymax = ~upr,
      fillcolor  = ci_fill,
      line       = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo  = "skip"
    ) %>%
    add_lines(
      data       = pred_ci,
      x          = ~x_kaf, y = ~fit,
      line       = list(color = col_dam, width = 1.5),
      showlegend = TRUE,
      name       = reg_label,
      hoverinfo  = "skip"
    )
  
  # Normal reference line
  fig <- fig %>%
    add_segments(
      x    = x_range[1] * 0.85, xend = x_range[2] * 1.15,
      y    = normal_kaf,         yend = normal_kaf,
      line = list(color = normal_color, width = 1.2, dash = "dash"),
      showlegend = TRUE,
      name = paste0(seas, " natural flow normal (", NORMAL_START, "\u2013",
                    NORMAL_END, "): ", round(normal_kaf), " KAF"),
      text = paste0(seas, " normal: ", round(normal_kaf), " KAF"),
      hovertemplate = "%{text}<extra></extra>"
    )
  
  # Historical dots
  fig <- fig %>%
    add_markers(
      data          = hist_df,
      x             = ~x_kaf, y = ~y_kaf,
      marker        = list(
        color   = col_dam,
        size    = 9,
        opacity = 0.75,
        line    = list(color = "white", width = 1)
      ),
      text          = ~label,
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = TRUE,
      name          = paste0("Historical WY (Apr 1 snowpack)")
    )
  
  # Current WY triangle + annotation
  if (!is.na(curr_x_kaf)) {
    fig <- fig %>%
      add_markers(
        x      = curr_x_kaf, y = 0,
        marker = list(
          color  = curr_color, size = 14,
          symbol = "triangle-down",
          line   = list(color = "white", width = 1.5)
        ),
        text = paste0(
          "<b>WY", current_wy, " \u2014 Apr 1 Snowpack</b><br>",
          "Snowpack: ", fmt_kaf_x(curr_x_kaf), " KAF<br>",
          "Estimated ", seas, " natural flow: ", round(curr_est_kaf), " KAF<br>",
          "95% CI: ", round(curr_ci_lo), " \u2013 ", round(curr_ci_hi), " KAF<br>",
          seas, " normal: ", round(normal_kaf), " KAF"
        ),
        hovertemplate = "%{text}<extra></extra>",
        showlegend    = TRUE,
        name          = paste0("WY", current_wy, " Apr 1 snowpack")
      ) %>%
      add_segments(
        x = curr_x_kaf, xend = curr_x_kaf,
        y = 0,          yend = curr_est_kaf,
        line      = list(color = curr_color, width = 1.2, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_annotations(
        x = curr_x_kaf, y = curr_est_kaf,
        text = paste0(
          "<b>Estimated ", seas, " natural flow:<br>",
          round(curr_est_kaf), " KAF</b><br>",
          "(95% CI: ", round(curr_ci_lo), " \u2013 ", round(curr_ci_hi), " KAF)"
        ),
        showarrow   = TRUE, arrowhead = 2, arrowsize = 0.8,
        arrowcolor  = curr_color,
        ax = 65, ay = -55,
        font        = list(size = 11, color = curr_color),
        bgcolor     = "white",
        bordercolor = curr_color, borderwidth = 1, borderpad = 4
      )
  }
  
  fig %>%
    plotly::layout(
      title = list(
        text = paste0(
          "<b>April 1 Snowpack vs ", seas,
          " Natural Flow Outlook \u2014 Yakima Basin</b><br>",
          "<sup>WY", current_wy, " Apr 1 snowpack shown",
          "  |  Historical water years ", SNOW_YR_START, "\u2013",
          current_wy - 1,
          "  |  Natural flow: sum of 5 NWRFC stations</sup>"
        ),
        x = 0.5, font = list(size = 13)
      ),
      xaxis = list(
        title    = "April 1 Snowpack (thousand AF)",
        showgrid = TRUE, gridcolor = col_grid,
        tickfont = list(size = 10), zeroline = FALSE
      ),
      yaxis = list(
        title     = paste0(seas, " Natural Flow (thousand AF)"),
        showgrid  = TRUE, gridcolor = col_grid,
        tickfont  = list(size = 10), rangemode = "tozero"
      ),
      legend = list(
        orientation = "h", x = 0.5, xanchor = "center",
        y = -0.22, font = list(size = 10)
      ),
      margin        = list(t = 70, b = 80, l = 70, r = 20),
      hovermode     = "closest",
      paper_bgcolor = "white",
      plot_bgcolor  = "white"
    ) %>%
    plotly::config(
      responsive             = TRUE,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
}

# ==============================================================================
# PANEL 2: Monthly NWRFC natural flow — current WY vs historical range
# ==============================================================================

build_monthly_plot <- function() {
  
  seas_months  <- 4:9
  month_labels <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep")
  # Use integer positions 1-6 for x-axis so ribbon and bars align perfectly
  month_pos    <- seq_along(month_labels)
  
  # Historical monthly values — all complete past water years
  hist_monthly <- runoff_raw %>%
    filter(
      reservoir  == "ALL",
      month      %in% seas_months,
      water_year >= SNOW_YR_START,
      water_year <  current_wy
    ) %>%
    mutate(
      water_year = if_else(month >= 10L, year + 1L, as.integer(year)),
      month_pos  = month - 3L        # Apr=1, May=2, Jun=3, Jul=4, Aug=5, Sep=6
    )
  
  # Percentile ribbon: 10th, 50th, 90th per month
  ribbon <- hist_monthly %>%
    group_by(month, month_pos) %>%
    summarise(
      p10     = quantile(volume_af, 0.10, na.rm = TRUE),
      p50     = quantile(volume_af, 0.50, na.rm = TRUE),
      p90     = quantile(volume_af, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      p10_kaf = p10 / 1000,
      p50_kaf = p50 / 1000,
      p90_kaf = p90 / 1000
    ) %>%
    arrange(month_pos)
  
  # Current WY monthly values (may be partial)
  curr_monthly <- runoff_raw %>%
    filter(
      reservoir  == "ALL",
      month      %in% seas_months,
      water_year == current_wy
    ) %>%
    mutate(
      water_year = if_else(month >= 10L, year + 1L, as.integer(year)),
      month_pos  = month - 3L,
      vol_kaf    = volume_af / 1000,
      hover_text = paste0(
        "<b>WY", current_wy, " \u2014 ", month_labels[month - 3], "</b><br>",
        "Natural flow: ", round(vol_kaf), " KAF"
      )
    ) %>%
    arrange(month_pos)
  
  fig <- plot_ly()
  
  # 10th-90th percentile ribbon — uses numeric x so it aligns with bars
  fig <- fig %>%
    add_ribbons(
      data       = ribbon,
      x          = ~month_pos,
      ymin       = ~p10_kaf,
      ymax       = ~p90_kaf,
      fillcolor  = "rgba(146, 197, 222, 0.25)",
      line       = list(color = "transparent"),
      showlegend = TRUE,
      name       = paste0("Historical range (10th\u201390th pct, ",
                          SNOW_YR_START, "\u2013", current_wy - 1, ")"),
      hoverinfo  = "skip"
    )
  
  # Median line
  fig <- fig %>%
    add_lines(
      data          = ribbon,
      x             = ~month_pos,
      y             = ~p50_kaf,
      line          = list(color = col_dam, width = 1.5, dash = "dash"),
      showlegend    = TRUE,
      name          = "Historical median",
      hovertemplate = "Median: %{y:,.0f} KAF<extra></extra>"
    )
  
  # Current WY bars — also on numeric x
  if (nrow(curr_monthly) > 0) {
    fig <- fig %>%
      add_bars(
        data          = curr_monthly,
        x             = ~month_pos,
        y             = ~vol_kaf,
        marker        = list(
          color = col_snow,
          line  = list(color = col_dam, width = 1)
        ),
        width = 0.6, 
        showlegend    = TRUE,
        name          = paste0("WY", current_wy, " natural flow"),
        text          = ~hover_text,
        hovertemplate = "%{text}<extra></extra>"
      )
  }
  
  fig %>%
    plotly::layout(
      title = list(
        text = paste0(
          "<b>WY", current_wy,
          " Monthly Natural Flow vs Historical Range \u2014 Yakima Basin</b><br>",
          "<sup>Apr\u2013Sep  |  NWRFC system total (5 stations)  |  ",
          "Ribbon = 10th\u201390th percentile (", SNOW_YR_START, "\u2013",
          current_wy - 1, ")</sup>"
        ),
        x = 0.5, font = list(size = 13)
      ),
      xaxis = list(
        title      = "",            # suppress axis title entirely
        tickmode   = "array",
        tickvals   = month_pos,       # positions 1-6
        ticktext   = month_labels,    # "Apr","May","Jun","Jul","Aug","Sep"
        showgrid   = FALSE,
        tickfont   = list(size = 10),
        range      = c(0.4, 6.6)     # small padding each side
      ),
      yaxis = list(
        title    = "Natural Flow (thousand AF)",
        showgrid = TRUE, gridcolor = col_grid,
        tickfont = list(size = 10), rangemode = "tozero"
      ),
      barmode = "overlay",
      legend  = list(
        orientation = "h", x = 0.5, xanchor = "center",
        y = -0.20, font = list(size = 10)
      ),
      margin        = list(t = 65, b = 60, l = 70, r = 20),
      hovermode     = "x unified",
      paper_bgcolor = "white",
      plot_bgcolor  = "white"
    ) %>%
    plotly::config(
      responsive             = TRUE,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
}


# ==============================================================================
# PANEL 3: Apr-Sep delivery composition — natural flow + augmentation stacked bars
# ==============================================================================

build_delivery_plot <- function() {
  
  # Join NWRFC natural flow + BOR QD observed; compute augmentation
  # Floor augmentation at zero (negative = data discrepancy, not real release)
  delivery <- nwrfc_aprsep %>%
    inner_join(bor_qd_aprsep, by = "water_year") %>%
    mutate(
      natural_kaf = nwrfc_af  / 1000,
      augment_kaf = pmax(0, (bor_qd_af - nwrfc_af) / 1000),
      total_kaf   = natural_kaf + augment_kaf
    ) %>%
    arrange(water_year)
  
  # Normal reference values (NORMAL_START to NORMAL_END)
  normal_range <- delivery %>%
    filter(water_year > NORMAL_START,
           water_year <= NORMAL_END + 1)
  
  natural_normal_kaf <- mean(normal_range$natural_kaf, na.rm = TRUE)
  augment_normal_kaf <- mean(normal_range$augment_kaf, na.rm = TRUE)
  total_normal_kaf   <- natural_normal_kaf + augment_normal_kaf
  
  x_range_vals <- range(delivery$water_year)
  
  # Hover text — one tooltip per bar showing full breakdown
  delivery <- delivery %>%
    mutate(
      hover_text = paste0(
        "<b>WY", water_year, "</b><br>",
        "Natural flow: ",    round(natural_kaf), " KAF<br>",
        "Augmentation: ",    round(augment_kaf), " KAF<br>",
        "Total delivery: ",  round(total_kaf),   " KAF"
      )
    )
  
  col_natural <- col_dam     # blue — natural flow
  col_augment <- "#c8a951"   # tan/gold — reservoir augmentation
  
  fig <- plot_ly()
  
  # Natural flow bars (bottom stack)
  fig <- fig %>%
    add_bars(
      data          = delivery,
      x             = ~water_year,
      y             = ~natural_kaf,
      name          = "Natural flow (NWRFC)",
      marker        = list(
        color = col_natural,
        line  = list(color = "white", width = 0.5)
      ),
      text          = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      textposition  = "none"           # ← suppress inline text on bars
    )
  
  # Augmentation bars (top stack)
  fig <- fig %>%
    add_bars(
      data          = delivery,
      x             = ~water_year,
      y             = ~augment_kaf,
      name          = "Reservoir augmentation (BOR QD \u2212 NWRFC)",
      marker        = list(
        color = col_augment,
        line  = list(color = "white", width = 0.5)
      ),
      text          = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      textposition  = "none"           # ← suppress inline text on bars
    )
  
  # Natural flow normal reference line
  fig <- fig %>%
    add_segments(
      x    = x_range_vals[1] - 0.5, xend = x_range_vals[2] + 0.5,
      y    = natural_normal_kaf,     yend = natural_normal_kaf,
      line = list(color = col_natural, width = 1.5, dash = "dash"),
      showlegend    = TRUE,
      name          = paste0("Natural flow normal (", NORMAL_START, "\u2013",
                             NORMAL_END, "): ", round(natural_normal_kaf), " KAF"),
      hovertemplate = paste0("Natural flow normal: ",
                             round(natural_normal_kaf), " KAF<extra></extra>")
    )
  
  # Total delivery normal reference line
  fig <- fig %>%
    add_segments(
      x    = x_range_vals[1] - 0.5, xend = x_range_vals[2] + 0.5,
      y    = total_normal_kaf,       yend = total_normal_kaf,
      line = list(color = col_augment, width = 1.5, dash = "dash"),
      showlegend    = TRUE,
      name          = paste0("Total delivery normal (", NORMAL_START, "\u2013",
                             NORMAL_END, "): ", round(total_normal_kaf), " KAF"),
      hovertemplate = paste0("Total delivery normal: ",
                             round(total_normal_kaf), " KAF<extra></extra>")
    )
  
  fig %>%
    plotly::layout(
      barmode = "stack",
      title   = list(
        text = paste0(
          "<b>Apr\u2013Sep Delivery Composition by Water Year \u2014 Yakima Basin</b><br>",
          "<sup>Blue = natural flow (NWRFC)  \u00a0|\u00a0  ",
          "Tan = reservoir augmentation (BOR observed \u2212 natural flow)  \u00a0|\u00a0  ",
          "Dashed lines = ", NORMAL_START, "\u2013", NORMAL_END, " normals</sup>"
        ),
        x = 0.5, font = list(size = 13)
      ),
      xaxis = list(
        title    = "Water Year",
        showgrid = FALSE,
        tickmode = "linear", dtick = 1,
        tickfont = list(size = 9),
        tickangle = -45
      ),
      yaxis = list(
        title    = "Apr\u2013Sep Volume (thousand AF)",
        showgrid = TRUE, gridcolor = col_grid,
        tickfont = list(size = 10), rangemode = "tozero"
      ),
      legend = list(
        orientation = "h", x = 0.5, xanchor = "center",
        y = -0.28, font = list(size = 10)
      ),
      margin        = list(t = 70, b = 90, l = 70, r = 20),
      hovermode     = "x unified",
      paper_bgcolor = "white",
      plot_bgcolor  = "white"
    ) %>%
    plotly::config(
      responsive             = TRUE,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
}
