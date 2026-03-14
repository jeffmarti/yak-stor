# ==============================================================================
# modules/mod_conditions.R
#
# Shiny module: Current Conditions Summary Table
# One row per reservoir + system total showing:
#   Dam Storage | vs Normal | Snow Storage | vs Normal | Combined | vs Normal
#
# Normals:
#   Dam:  1991-2020 average for current water year day
#   Snow: 2004-2025 average for current water year day
#
# All data objects come from global.R.
# ==============================================================================

# -- UI ------------------------------------------------------------------------

mod_conditions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "control-row",
      div(style = "display:flex; align-items:center; gap:20px; flex-wrap:wrap;",
        div(
          style = "font-size:13px; font-weight:600; color:#1a3a5c;",
          textOutput(ns("as_of_date"), inline = TRUE)
        ),
        div(
          style = "font-size:11px; color:#666;",
          "Dam normals: 1991-2020 average | Snow normals: 2004-2025 average"
        )
      ),
      div(
        downloadButton(ns("download_csv"), "Download CSV",
                       style = "font-size:11px; padding:4px 10px;")
      )
    ),
    fluidRow(
      column(12,
        div(style = "overflow-x:auto;",
          tableOutput(ns("conditions_table"))
        )
      )
    ),
    tags$div(
      style = "margin-top:8px; font-size:10px; color:#888; padding:0 4px;",
      "All values in acre-feet. Green = above normal. Red = below normal. ",
      "% of Normal = current value as a percentage of the historical average ",
      "for this date."
    )
  )
}

# -- Server --------------------------------------------------------------------

mod_conditions_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Build the conditions data frame
    conditions_df <- reactive({
      build_conditions()
    })

    output$as_of_date <- renderText({
      paste0("Current Conditions as of: ", format(max_date, "%B %d, %Y"))
    })

    output$conditions_table <- renderTable({
      df <- conditions_df()
      df$display   # return display-formatted version
    },
    sanitize.text.function = function(x) x,  # allow HTML color spans
    bordered   = TRUE,
    striped    = TRUE,
    hover      = TRUE,
    width      = "100%",
    align      = "lrrrrrr"
    )

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("yakima_current_conditions_",
               format(max_date, "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        write.csv(conditions_df()$download, file, row.names = FALSE)
      }
    )

  })
}

# -- Data builder --------------------------------------------------------------

build_conditions <- function() {

  fmt_af  <- function(x) formatC(round(x), format = "d", big.mark = ",")
  fmt_dev <- function(x) {
    sign_str <- if_else(x >= 0, "+", "")
    paste0(sign_str, formatC(round(x), format = "d", big.mark = ","))
  }
  fmt_pct <- function(x) paste0(round(x), "%")
  color_span <- function(val, text) {
    col <- if_else(val >= 0, "#1a7a1a", "#c0392b")
    paste0('<span style="color:', col, '; font-weight:600;">', text, '</span>')
  }

  # Current water year day
  current_wy_day <- as.integer(
    max_date - as.Date(paste0(current_wy - 1, "-10-01"))
  ) + 1L

  # Current values -- per reservoir and ALL
  current <- plot_data %>%
    filter(water_year == current_wy,
           wy_day == current_wy_day) %>%
    select(reservoir, dam_storage_acre_feet, snow_storage_acre_feet) %>%
    mutate(combined = dam_storage_acre_feet + snow_storage_acre_feet)

  # Dam normal for current wy_day (1991-2020)
  dam_norm_today <- dam_normal %>%
    filter(wy_day == current_wy_day) %>%
    select(reservoir, dam_normal_af)

  # Snow normal for current wy_day (2004-2025)
  snow_norm_today <- snow_normal %>%
    filter(wy_day == current_wy_day) %>%
    select(reservoir, snow_normal_af)

  # Combined normal
  norms <- dam_norm_today %>%
    left_join(snow_norm_today, by = "reservoir") %>%
    mutate(combined_normal_af = dam_normal_af + snow_normal_af)

  # Join current + normals
  tbl <- current %>%
    left_join(norms, by = "reservoir") %>%
    mutate(
      dam_dev      = dam_storage_acre_feet  - dam_normal_af,
      snow_dev     = snow_storage_acre_feet - snow_normal_af,
      combined_dev = combined               - combined_normal_af,
      dam_pct      = round(100 * dam_storage_acre_feet  / dam_normal_af),
      snow_pct     = round(100 * snow_storage_acre_feet / snow_normal_af),
      combined_pct = round(100 * combined               / combined_normal_af),
      reservoir    = factor(reservoir, levels = res_order)
    ) %>%
    arrange(reservoir) %>%
    mutate(
      res_label = reservoir_labels[as.character(reservoir)]
    )

  # -- Display version (HTML color spans) -------------------------------------
  display <- tbl %>%
    transmute(
      Reservoir      = res_label,

      `Dam Storage`  = fmt_af(dam_storage_acre_feet),
      `vs Normal`    = color_span(dam_dev,
                         paste0(fmt_dev(dam_dev), " AF (",
                                fmt_pct(dam_pct), ")")),

      `Snow Storage` = fmt_af(snow_storage_acre_feet),
      `vs Normal `   = color_span(snow_dev,
                         paste0(fmt_dev(snow_dev), " AF (",
                                fmt_pct(snow_pct), ")")),

      `Combined`     = fmt_af(combined),
      `vs Normal  `  = color_span(combined_dev,
                         paste0(fmt_dev(combined_dev), " AF (",
                                fmt_pct(combined_pct), ")"))
    )

  # -- Download version (plain text) ------------------------------------------
  download <- tbl %>%
    transmute(
      Reservoir              = res_label,
      Date                   = format(max_date, "%Y-%m-%d"),

      Dam_Storage_AF         = round(dam_storage_acre_feet),
      Dam_Normal_AF          = round(dam_normal_af),
      Dam_Deviation_AF       = round(dam_dev),
      Dam_Pct_of_Normal      = dam_pct,

      Snow_Storage_AF        = round(snow_storage_acre_feet),
      Snow_Normal_AF         = round(snow_normal_af),
      Snow_Deviation_AF      = round(snow_dev),
      Snow_Pct_of_Normal     = snow_pct,

      Combined_AF            = round(combined),
      Combined_Normal_AF     = round(combined_normal_af),
      Combined_Deviation_AF  = round(combined_dev),
      Combined_Pct_of_Normal = combined_pct
    )

  list(display = display, download = download)
}
