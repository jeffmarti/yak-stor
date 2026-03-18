# ==============================================================================
# modules/mod_forecast.R
#
# Shiny module: Yakima Basin Runoff Outlook
#
# Scatter plot of early-season combined storage (snow + reservoir) vs
# subsequent Apr-Sep runoff volume. One dot per historical water year
# (2004-present). Current year shown as inverted triangle at today's
# actual combined storage. Regression line + 95% CI. Normal reference line.
#
# NOTE: This is a statistical estimation tool only. For official seasonal
# volume forecasts consult NWRFC and NRCS.
#
# All data objects come from global.R.
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
        "Combined storage = reservoir + snowpack (SWE)"
      )
    ),
    fluidRow(
      column(12,
        plotlyOutput(ns("forecast_plot"), height = "580px", width = "100%")
      )
    ),
    tags$div(
      style = "margin-top:8px; font-size:10px; color:#888; padding:0 4px;
               line-height:1.6; border-top:1px solid #eee; padding-top:6px;",
      tags$strong("Note:"),
      " This chart is a statistical estimation tool based on historical",
      " relationships between early-season storage and subsequent runoff.",
      " It is not an official forecast.",
      tags$br(),
      "For official seasonal volume forecasts consult the ",
      tags$a("Northwest River Forecast Center (NWRFC)",
             href   = "https://www.nwrfc.noaa.gov",
             target = "_blank"),
      " and the ",
      tags$a("Natural Resources Conservation Service (NRCS).",
             href   = "https://nwcc-apps.sc.egov.usda.gov/forecast-plots/?state=WA",
             target = "_blank"),
      tags$br(),
      "Runoff = sum of five NWRFC stations below Yakima Basin dams.  ",
      "Shaded band = 95% confidence interval.  ",
      "Water years 2004-present."
    )
  )
}

# -- Server --------------------------------------------------------------------

mod_forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$as_of_label <- renderText({
      paste0("Runoff Outlook as of: ", format(max_date, "%B %d, %Y"))
    })

    output$forecast_plot <- renderPlotly({
      withProgress(message = "Building runoff outlook...", {
        build_forecast_plot()
      })
    })

  })
}

# -- Plot builder --------------------------------------------------------------

build_forecast_plot <- function() {

  fmt_kaf <- function(x) {
    formatC(round(x / 1000, 0), format = "d", big.mark = ",")
  }

  seas_months <- 4:9
  season      <- "Apr-Sep"
  today_label <- format(max_date, "%B %d, %Y")

  x_title <- paste0("Combined Storage on ", today_label, " (thousand AF)")
  y_title <- paste0(season, " Runoff Volume (thousand AF)")
  plot_title <- paste0(
    "<b>Early-Season Storage vs ", season,
    " Runoff Outlook -- Yakima Basin</b><br>",
    "<sup>Current storage as of ", today_label,
    "  |  Historical water years 2004-",
    current_wy - 1, "  |  Runoff: sum of 5 NWRFC stations</sup>"
  )

  # -- Historical predictor: storage on today's calendar date each past WY ----
  today_mo_day <- format(max_date, "%m-%d")

  hist_pred <- daily %>%
    mutate(
      water_year = if_else(month(Date) >= 10,
                           year(Date) + 1L, year(Date)),
      mo_day     = format(Date, "%m-%d")
    ) %>%
    filter(
      mo_day     == today_mo_day,
      water_year >= 2004,
      water_year <  current_wy
    ) %>%
    select(water_year, pred_date = Date,
           combined_af_pred = combined_af) %>%
    distinct(water_year, .keep_all = TRUE)

  # Fallback: if exact date not found use nearest within 3 days
  if (nrow(hist_pred) < 3) {
    hist_pred <- daily %>%
      mutate(
        water_year = if_else(month(Date) >= 10,
                             year(Date) + 1L, year(Date)),
        ref_date   = as.Date(paste0(
          if_else(month(max_date) >= 10,
                  water_year - 1L, water_year),
          format(max_date, "-%m-%d")
        )),
        days_diff  = abs(as.integer(Date - ref_date))
      ) %>%
      filter(days_diff <= 5,
             water_year >= 2004,
             water_year <  current_wy) %>%
      group_by(water_year) %>%
      arrange(days_diff) %>%
      slice(1) %>%
      ungroup() %>%
      select(water_year, pred_date = Date,
             combined_af_pred = combined_af)
  }

  # -- Response: Apr-Sep runoff sum for each past water year -------------------
  runoff_seas <- runoff_raw %>%
    filter(
      reservoir == "ALL",
      month     %in% seas_months,
      year      >= 2004
    ) %>%
    mutate(water_year = if_else(month >= 10,
                                year + 1L, as.integer(year))) %>%
    group_by(water_year) %>%
    summarise(
      runoff_af = sum(volume_af, na.rm = TRUE),
      n_months  = n(),
      .groups   = "drop"
    ) %>%
    filter(n_months == length(seas_months))

  # -- Historical scatter data -------------------------------------------------
  hist_df <- hist_pred %>%
    inner_join(runoff_seas, by = "water_year") %>%
    mutate(
      x_kaf = combined_af_pred / 1000,
      y_kaf = runoff_af / 1000,
      label = paste0(
        "<b>WY", water_year, "</b><br>",
        format(pred_date, "%b %d"), " storage: ",
        fmt_kaf(combined_af_pred), " KAF<br>",
        season, " runoff: ", fmt_kaf(runoff_af), " KAF"
      )
    )

  if (nrow(hist_df) < 4) {
    return(plot_ly() %>%
      plotly::layout(
        title = "Insufficient historical data for current date",
        annotations = list(list(
          text      = "Not enough historical years have data for this date.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 14)
        ))
      ))
  }

  # -- Current storage: today's actual combined storage ------------------------
  curr_storage_af <- daily %>%
    arrange(desc(Date)) %>%
    slice(1) %>%
    pull(combined_af)
  curr_x_kaf <- curr_storage_af / 1000

  # -- Apr-Sep runoff normal (1991-2020) ---------------------------------------
  runoff_normal_aprsep <- runoff_raw %>%
    filter(
      reservoir == "ALL",
      month     %in% seas_months,
      year      >= NORMAL_START,
      year      <= NORMAL_END
    ) %>%
    group_by(year) %>%
    summarise(annual_af = sum(volume_af, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    filter(n == length(seas_months)) %>%
    summarise(normal_af = mean(annual_af)) %>%
    pull(normal_af)

  normal_kaf <- runoff_normal_aprsep / 1000

  # -- Regression + 95% CI (historical data only) ------------------------------
  fit   <- lm(y_kaf ~ x_kaf, data = hist_df)
  r2    <- summary(fit)$r.squared
  slope <- coef(fit)[["x_kaf"]]

  x_range <- range(c(hist_df$x_kaf, curr_x_kaf))
  x_seq   <- seq(x_range[1] * 0.90, x_range[2] * 1.10, length.out = 120)

  pred_ci <- predict(fit,
                     newdata  = data.frame(x_kaf = x_seq),
                     interval = "confidence",
                     level    = 0.95) %>%
    as.data.frame() %>%
    mutate(x_kaf = x_seq)

  curr_pred     <- predict(fit,
                           newdata  = data.frame(x_kaf = curr_x_kaf),
                           interval = "confidence",
                           level    = 0.95)
  curr_est_kaf  <- curr_pred[1]
  curr_ci_lo    <- curr_pred[2]
  curr_ci_hi    <- curr_pred[3]

  reg_label <- sprintf(
    "Regression  R2 = %.2f  |  slope = %.2f KAF runoff per KAF storage",
    r2, slope
  )

  # -- Colors ------------------------------------------------------------------
  hist_color   <- "#2166ac"
  curr_color   <- "#d73027"
  normal_color <- "#555555"
  ci_fill      <- "rgba(33, 102, 172, 0.12)"

  # -- Build plot --------------------------------------------------------------
  fig <- plot_ly()

  # Confidence interval band
  fig <- fig %>%
    add_ribbons(
      data       = pred_ci,
      x          = ~x_kaf,
      ymin       = ~lwr,
      ymax        = ~upr,
      fillcolor  = ci_fill,
      line       = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo  = "skip"
    ) %>%
    add_lines(
      data       = pred_ci,
      x          = ~x_kaf,
      y          = ~fit,
      line       = list(color = hist_color, width = 1.5),
      showlegend = TRUE,
      name       = reg_label,
      hoverinfo  = "skip"
    )

  # Apr-Sep normal reference line
  fig <- fig %>%
    add_segments(
      x    = x_range[1] * 0.90, xend = x_range[2] * 1.10,
      y    = normal_kaf,         yend = normal_kaf,
      line = list(color = normal_color, width = 1.2, dash = "dash"),
      showlegend = TRUE,
      name = paste0(season, " normal (1991-2020): ",
                    round(normal_kaf), " KAF"),
      text = paste0(season, " normal (1991-2020): ",
                    round(normal_kaf), " KAF"),
      hovertemplate = "%{text}<extra></extra>"
    )

  # Historical dots
  fig <- fig %>%
    add_markers(
      data          = hist_df,
      x             = ~x_kaf,
      y             = ~y_kaf,
      marker        = list(
        color   = hist_color,
        size    = 9,
        opacity = 0.75,
        line    = list(color = "white", width = 1)
      ),
      text          = ~label,
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = TRUE,
      name          = paste0("Historical WY (storage ~", today_label, ")")
    )

  # Current storage triangle on x axis
  fig <- fig %>%
    add_markers(
      x    = curr_x_kaf,
      y    = 0,
      marker = list(
        color  = curr_color,
        size   = 14,
        symbol = "triangle-down",
        line   = list(color = "white", width = 1.5)
      ),
      text = paste0(
        "<b>WY", current_wy, " -- Current Storage</b><br>",
        "As of: ", today_label, "<br>",
        "Combined storage: ", fmt_kaf(curr_storage_af), " KAF<br>",
        "Estimated ", season, " runoff: ", round(curr_est_kaf), " KAF<br>",
        "95% CI: ", round(curr_ci_lo), " - ", round(curr_ci_hi), " KAF<br>",
        season, " normal: ", round(normal_kaf), " KAF"
      ),
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = TRUE,
      name          = paste0("WY", current_wy, " current storage")
    )

  # Dashed vertical line from triangle to regression estimate
  fig <- fig %>%
    add_segments(
      x    = curr_x_kaf, xend = curr_x_kaf,
      y    = 0,           yend = curr_est_kaf,
      line = list(color = curr_color, width = 1.2, dash = "dot"),
      showlegend = FALSE,
      hoverinfo  = "skip"
    )

  # Annotation at estimated value
  fig <- fig %>%
    add_annotations(
      x           = curr_x_kaf,
      y           = curr_est_kaf,
      text        = paste0(
        "<b>Estimated ", season, " runoff:<br>",
        round(curr_est_kaf), " KAF</b><br>",
        "(95% CI: ", round(curr_ci_lo), " - ",
        round(curr_ci_hi), " KAF)"
      ),
      showarrow   = TRUE,
      arrowhead   = 2,
      arrowsize   = 0.8,
      arrowcolor  = curr_color,
      ax          = 65,
      ay          = -55,
      font        = list(size = 11, color = curr_color),
      bgcolor     = "white",
      bordercolor = curr_color,
      borderwidth = 1,
      borderpad   = 4
    )

  fig %>%
    plotly::layout(
      title  = list(text = plot_title, x = 0.5, font = list(size = 13)),
      xaxis  = list(
        title    = x_title,
        showgrid = TRUE, gridcolor = col_grid,
        tickfont = list(size = 10),
        zeroline = FALSE
      ),
      yaxis  = list(
        title     = y_title,
        showgrid  = TRUE, gridcolor = col_grid,
        tickfont  = list(size = 10),
        rangemode = "tozero"
      ),
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.18,
        font = list(size = 10)
      ),
      margin        = list(t = 80, b = 100, l = 80, r = 20),
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