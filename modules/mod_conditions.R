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
  
  # Available dates for the date picker — restrict to dates in the data
  available_dates <- sort(unique(plot_data$Date))
  full_range      <- seq(min(available_dates), max(available_dates), by = "day")
  missing_dates   <- as.character(full_range[!full_range %in% available_dates])
  
  tagList(
    div(class = "control-row",
        div(style = "display:flex; align-items:center; gap:20px; flex-wrap:wrap;",
            
            # Date picker
            div(
              dateInput(
                ns("conditions_date"),
                label   = "Conditions Date",
                value   = max_date,
                min     = min(available_dates),
                max     = max_date,
                format  = "MM d, yyyy",
                datesdisabled = missing_dates
              )
            ),
            
            # Reset to latest button
            div(style = "margin-top: 25px;",
                actionButton(
                  ns("reset_date"),
                  label = "\u21ba Latest",
                  class = "btn btn-default btn-sm"
                )
            ),
            
            # As-of label
            div(
              style = "font-size:13px; font-weight:600; color:#1a3a5c; margin-top:25px;",
              textOutput(ns("as_of_date"), inline = TRUE)
            ),
            
            div(
              style = "font-size:11px; color:#666; margin-top:25px;",
              "Dam normals: 1991\u20132020 average | Snow normals: 2004\u20132025 average"
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
    
    # Reset to latest date when button clicked
    observeEvent(input$reset_date, {
      updateDateInput(session, "conditions_date", value = max_date)
    })
    
    # Build the conditions data frame for the selected date
    conditions_df <- reactive({
      req(input$conditions_date)
      build_conditions(input$conditions_date)
    })
    
    output$as_of_date <- renderText({
      req(input$conditions_date)
      lbl <- if (input$conditions_date == max_date) {
        "Current Conditions as of: "
      } else {
        "Historical Conditions as of: "
      }
      paste0(lbl, format(input$conditions_date, "%B %d, %Y"))
    })
    
    output$conditions_table <- renderTable({
      df <- conditions_df()
      df$display
    },
    sanitize.text.function = function(x) x,
    bordered   = TRUE,
    striped    = TRUE,
    hover      = TRUE,
    width      = "100%",
    align      = "lrrrrrr"
    )
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("yakima_conditions_",
               format(input$conditions_date, "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        write.csv(conditions_df()$download, file, row.names = FALSE)
      }
    )
    
  })
}

# -- Data builder --------------------------------------------------------------

build_conditions <- function(selected_date) {
  
  fmt_af  <- function(x) formatC(round(x), format = "d", big.mark = ",")
  fmt_dev <- function(x) {
    sign_str <- if_else(x >= 0, "+", "")
    paste0(sign_str, formatC(round(x), format = "d", big.mark = ","))
  }
  fmt_pct    <- function(x) paste0(round(x), "%")
  color_span <- function(val, text) {
    col <- if_else(val >= 0, "#1a7a1a", "#c0392b")
    paste0('<span style="color:', col, '; font-weight:600;">', text, '</span>')
  }
  
  sel_wy <- if_else(month(selected_date) >= 10,
                    year(selected_date) + 1L,
                    year(selected_date))
  
  sel_wy_day <- as.integer(
    selected_date - as.Date(paste0(sel_wy - 1, "-10-01"))
  ) + 1L
  
  # Current values for selected date
  current <- plot_data %>%
    filter(water_year == sel_wy,
           wy_day     == sel_wy_day) %>%
    select(reservoir, dam_storage_acre_feet, snow_storage_acre_feet) %>%
    mutate(combined = dam_storage_acre_feet + snow_storage_acre_feet)
  
  # Dam normal for this wy_day (1991-2020)
  dam_norm_today <- dam_normal %>%
    filter(wy_day == sel_wy_day) %>%
    select(reservoir, dam_normal_af)
  
  # Snow normal for this wy_day (2004-2025)
  snow_norm_today <- snow_normal %>%
    filter(wy_day == sel_wy_day) %>%
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
    mutate(res_label = reservoir_labels[as.character(reservoir)])
  
  # Display version (HTML color spans)
  display <- tbl %>%
    transmute(
      Reservoir      = res_label,
      `Dam Storage`  = fmt_af(dam_storage_acre_feet),
      `vs Normal`    = color_span(dam_dev,
                                  paste0(fmt_dev(dam_dev), " AF (", fmt_pct(dam_pct), ")")),
      `Snow Storage` = fmt_af(snow_storage_acre_feet),
      `vs Normal `   = color_span(snow_dev,
                                  paste0(fmt_dev(snow_dev), " AF (", fmt_pct(snow_pct), ")")),
      `Combined`     = fmt_af(combined),
      `vs Normal  `  = color_span(combined_dev,
                                  paste0(fmt_dev(combined_dev), " AF (",
                                         fmt_pct(combined_pct), ")"))
    )
  
  # Download version (plain text)
  download <- tbl %>%
    transmute(
      Reservoir              = res_label,
      Date                   = format(selected_date, "%Y-%m-%d"),
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