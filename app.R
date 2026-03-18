# ==============================================================================
# app.R  --  Yakima Basin Water Dashboard
# https://waterwater.shinyapps.io/yakima-storage
#
# Structure:
#   global.R                 -- loads all pre-computed CSVs, shared data objects
#   modules/mod_conditions.R -- Current Conditions tab
#   modules/mod_storage.R    -- Storage Dashboard tab
#   modules/mod_explorer.R   -- Climate Explorer tab
#   modules/mod_forecast.R   -- Runoff Outlook tab
# ==============================================================================

source("global.R")
source("modules/mod_conditions.R")
source("modules/mod_storage.R")
source("modules/mod_explorer.R")
source("modules/mod_forecast.R")

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Page header
  div(class = "page-header",
    tags$h2("Yakima Basin Water Dashboard"),
    tags$p(
      "Reservoir storage | Snowpack | Climate context",
      style = "margin:0; font-size:12px; opacity:0.75;"
    )
  ),

  # Two-column layout: sidebar + main
  fluidRow(

    # Sidebar
    column(3,
      div(class = "sidebar-panel",

        tags$img(
          src   = "yakima_basin_map.png",
          width = "100%",
          style = "border-radius:4px; margin-bottom:12px; border:1px solid #ddd;"
        ),

        tags$h4("About This Dashboard", style = "margin-top:0;"),

        tags$p(
          "The Yakima Basin contains five federal reservoirs operated by the",
          "U.S. Bureau of Reclamation: Bumping Lake, Cle Elum Lake,",
          "Kachess Dam, Keechelus Lake, and Rimrock Lake.",
          "Total system capacity is approximately 1.07 million acre-feet."
        ),
        tags$p(
          "Mountain snowpack functions as a sixth natural reservoir, storing winter",
          "precipitation and releasing it gradually through the spring and summer",
          "months when irrigation demand is highest."
        ),
        tags$p(
          "This dashboard combines daily reservoir storage from USBR Hydromet with",
          "basin snowpack estimates derived from SNODAS data provided by the National",
          "Operational Hydrologic Remote Sensing Center (NOHRSC) and Climate Engine.",
          "Together these represent total constructed and natural water storage.",
          "Note that this estimate does not include groundwater or soil moisture",
          "recharged by rain and snowmelt infiltration."
        ),
        tags$p(
          "The Climate Explorer places current conditions in historical context",
          "using East Cascades temperature and precipitation anomalies from NOAA NCEI."
        ),

        tags$hr(style = "margin: 10px 0;"),

        tags$div(
          style = "font-size:10px; color:#666; line-height:1.6;",
          tags$strong("Data updated daily"),
          tags$br(),
          paste0("Snow: ", data_freshness$swe_through),
          tags$br(),
          paste0("Storage: ", data_freshness$dam_through),
          tags$br(),
          paste0("Climate: NCEI through ", data_freshness$ncei_through)
        ),

        tags$hr(style = "margin: 10px 0;"),

        tags$div(
          style = "font-size:10px; color:#888; line-height:1.6;",
          tags$strong("Data Sources"), tags$br(),
          "USBR Hydromet (dam storage)", tags$br(),
          "NOHRSC / SNODAS (snowpack SWE)", tags$br(),
          "NOAA NCEI Climate-at-a-Glance", tags$br(),
          "NWRFC (monthly runoff)"
        ),

        tags$hr(style = "margin: 10px 0;"),

        tags$div(
          style = "font-size:10px; color:#888; line-height:1.6;",
          tags$strong("Contact"), tags$br(),
          "jeffjmarti at gmail.com", tags$br()
        )

      )  # closes sidebar-panel div
    ),   # closes column(3)

    # Main content
    column(9,
      div(class = "main-panel",
        tabsetPanel(
          id   = "main_tabs",
          type = "tabs",

          tabPanel(
            title = "Storage Dashboard",
            value = "storage",
            br(),
            mod_storage_ui("storage")
          ),

          tabPanel(
            title = "Current Conditions",
            value = "conditions",
            br(),
            mod_conditions_ui("conditions")
          ),

          tabPanel(
            title = "Climate Explorer",
            value = "explorer",
            br(),
            mod_explorer_ui("explorer")
          ),

          tabPanel(
            title = "Runoff Outlook",
            value = "forecast",
            br(),
            mod_forecast_ui("forecast")
          )

        )  # closes tabsetPanel
      )    # closes main-panel div
    )      # closes column(9)

  )  # closes fluidRow

)  # closes fluidPage

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  mod_storage_server("storage")
  mod_conditions_server("conditions")
  mod_explorer_server("explorer")
  mod_forecast_server("forecast")
}

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
