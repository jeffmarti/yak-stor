# ==============================================================================
# historical_conditions.R
#
# Local-only app to reproduce the Yakima Current Conditions table
# for any historical date.
#
# Run with: shiny::runApp("historical_conditions.R")
# or open in RStudio and click Run App.
#
# Data pulled directly from the yak-stor GitHub repo CSVs.
# ==============================================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(conflicted)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::lag)

# ------------------------------------------------------------------------------
# CONSTANTS (copied from global.R)
# ------------------------------------------------------------------------------

NORMAL_START  <- 1991
NORMAL_END    <- 2020
SNOW_YR_START <- 2004

reservoir_labels <- c(
  BUM = "Bumping Lake",
  CLE = "Cle Elum Lake",
  KAC = "Kachess Dam",
  KEE = "Keechelus Dam",
  RIM = "Rimrock Lake",
  ALL = "System Total"
)
res_order <- c("BUM", "CLE", "KAC", "KEE", "RIM", "ALL")

# ------------------------------------------------------------------------------
# LOAD DATA (once at startup from GitHub)
# ------------------------------------------------------------------------------

# Point this at wherever your yak-stor project lives locally
YAK_DATA <- "F:/OneDrive/OneDriveDocuments/R/yak-stor/yak-stor-local/data"

message("Loading SWE data...")
swe_raw <- read_csv(file.path(YAK_DATA, "yakima_swe_combined.csv"),
                    show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

message("Loading dam data...")
dam_csv <- read_csv(file.path(YAK_DATA, "yakima_dam_daily.csv"),
                    show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

# Align streams to same end date (mirrors global.R logic)
max_available <- min(
  max(swe_raw$Date, na.rm = TRUE),
  max(dam_csv$Date, na.rm = TRUE)
)
swe_raw <- swe_raw %>% filter(Date <= max_available)
dam_csv <- dam_csv %>% filter(Date <= max_available)

message(sprintf("Data loaded. Available date range: %s to %s",
                format(min(swe_raw$Date), "%b %d, %Y"),
                format(max_available,     "%b %d, %Y")))

# ------------------------------------------------------------------------------
# BUILD plot_data, dam_normal, snow_normal (mirrors global.R logic exactly)
# ------------------------------------------------------------------------------

per_res <- swe_raw %>%
  filter(reservoir != "ALL") %>%
  select(Date, reservoir, water_year, snow_storage_acre_feet) %>%
  left_join(
    dam_csv %>%
      filter(reservoir != "ALL") %>%
      select(Date, reservoir, dam_storage_acre_feet),
    by = c("Date", "reservoir")
  ) %>%
  mutate(
    dam_storage_acre_feet  = replace_na(dam_storage_acre_feet,  0),
    snow_storage_acre_feet = replace_na(snow_storage_acre_feet, 0)
  )

system_total_stor <- per_res %>%
  group_by(Date, water_year) %>%
  summarise(
    snow_storage_acre_feet = sum(snow_storage_acre_feet, na.rm = TRUE),
    dam_storage_acre_feet  = sum(dam_storage_acre_feet,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(reservoir = "ALL")

plot_data <- bind_rows(per_res, system_total_stor) %>%
  mutate(
    reservoir = factor(reservoir, levels = res_order),
    wy_day    = as.integer(Date - as.Date(paste0(water_year - 1, "-10-01"))) + 1L,
    dam_top   = dam_storage_acre_feet,
    snow_top  = dam_storage_acre_feet + snow_storage_acre_feet
  )

# Dam normals (1991-2020)
dam_system_normal_rows <- plot_data %>%
  filter(water_year >= NORMAL_START, water_year <= NORMAL_END,
         reservoir != "ALL") %>%
  group_by(Date, water_year, wy_day) %>%
  summarise(dam_top = sum(dam_top, na.rm = TRUE), .groups = "drop") %>%
  mutate(reservoir = factor("ALL", levels = res_order))

dam_normal <- plot_data %>%
  filter(water_year >= NORMAL_START, water_year <= NORMAL_END,
         reservoir != "ALL") %>%
  select(Date, water_year, wy_day, reservoir, dam_top) %>%
  bind_rows(dam_system_normal_rows) %>%
  group_by(reservoir, wy_day) %>%
  summarise(dam_normal_af = mean(dam_top, na.rm = TRUE), .groups = "drop")

# Snow normals (2004-2025)
snow_sys_norm_rows <- plot_data %>%
  filter(water_year >= SNOW_YR_START, water_year <= 2025,
         reservoir != "ALL") %>%
  group_by(Date, water_year, wy_day) %>%
  summarise(snow_storage_acre_feet = sum(snow_storage_acre_feet, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(reservoir = factor("ALL", levels = res_order))

snow_normal <- plot_data %>%
  filter(water_year >= SNOW_YR_START, water_year <= 2025,
         reservoir != "ALL") %>%
  select(Date, water_year, wy_day, reservoir, snow_storage_acre_feet) %>%
  bind_rows(snow_sys_norm_rows) %>%
  group_by(reservoir, wy_day) %>%
  summarise(snow_normal_af = mean(snow_storage_acre_feet, na.rm = TRUE),
            .groups = "drop")

# ------------------------------------------------------------------------------
# BUILD CONDITIONS FUNCTION (mirrors mod_conditions.R exactly)
# ------------------------------------------------------------------------------

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
  
  # Normals for this wy_day
  dam_norm_today  <- dam_normal  %>% filter(wy_day == sel_wy_day) %>%
    select(reservoir, dam_normal_af)
  snow_norm_today <- snow_normal %>% filter(wy_day == sel_wy_day) %>%
    select(reservoir, snow_normal_af)
  
  norms <- dam_norm_today %>%
    left_join(snow_norm_today, by = "reservoir") %>%
    mutate(combined_normal_af = dam_normal_af + snow_normal_af)
  
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
  
  # Display version
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
  
  # Download version
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

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
           padding: 20px; }
    .title-bar { background: #1a3a5c; color: white; padding: 12px 20px;
                 margin-bottom: 20px; border-radius: 4px; }
    .title-bar h4 { margin: 0; font-size: 17px; }
    .title-bar p  { margin: 4px 0 0 0; font-size: 12px; opacity: 0.8; }
    .footnote { font-size: 10px; color: #888; margin-top: 8px; }
  "))),
  
  div(class = "title-bar",
      tags$h4("\U0001f4ca Yakima Basin — Historical Conditions Table"),
      tags$p("Local tool \u00b7 Reproduces the Current Conditions table for any historical date")
  ),
  
  fluidRow(
    column(3,
           dateInput("selected_date",
                     label    = "Select Date",
                     value    = as.Date("2005-04-01"),
                     min      = min(swe_raw$Date),
                     max      = max_available,
                     format   = "MM d, yyyy")
    ),
    column(3,
           tags$div(style = "margin-top: 25px;",
                    downloadButton("download_csv", "Download CSV",
                                   style = "font-size:11px; padding:4px 10px;"))
    )
  ),
  
  tags$div(style = "margin: 6px 0 10px 0; font-size: 12px; color: #555;",
           textOutput("norm_note")),
  
  div(style = "overflow-x: auto;",
      tableOutput("conditions_table")),
  
  tags$div(class = "footnote",
           "All values in acre-feet. Green = above normal. Red = below normal. ",
           "% of Normal = current value as a percentage of the historical average ",
           "for this day of the water year. ",
           "Dam normals: 1991\u20132020 | Snow normals: 2004\u20132025")
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  result <- reactive({
    req(input$selected_date)
    build_conditions(input$selected_date)
  })
  
  output$norm_note <- renderText({
    req(input$selected_date)
    sel_wy <- if_else(month(input$selected_date) >= 10,
                      year(input$selected_date) + 1L,
                      year(input$selected_date))
    sprintf("Conditions for %s  \u2014  Water Year %d  \u2014  Day %d of water year",
            format(input$selected_date, "%B %d, %Y"),
            sel_wy,
            as.integer(input$selected_date -
                         as.Date(paste0(sel_wy - 1, "-10-01"))) + 1L)
  })
  
  output$conditions_table <- renderTable({
    result()$display
  },
  sanitize.text.function = function(x) x,
  bordered = TRUE,
  striped  = TRUE,
  hover    = TRUE,
  width    = "100%",
  align    = "lrrrrrr"
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("yakima_conditions_",
             format(input$selected_date, "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(result()$download, file, row.names = FALSE)
    }
  )
}

# ------------------------------------------------------------------------------
shinyApp(ui, server)