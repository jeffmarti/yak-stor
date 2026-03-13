# ==============================================================================
# app.R  --  Yakima Basin Water Dashboard
# https://waterwater.shinyapps.io/yakima-storage
#
# Structure:
#   global.R               -- loads all pre-computed CSVs, shared data objects
#   modules/mod_storage.R  -- Storage Dashboard tab
#   modules/mod_explorer.R -- Climate Explorer tab
# ==============================================================================

source("global.R")

library(shiny)
library(plotly)
library(tidyverse)
library(conflicted)


source("modules/mod_storage.R")
source("modules/mod_explorer.R")

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

        # Basin map
        tags$img(
          src   = "yakima_basin_map.png",
          width = "100%",
          style = "border-radius:4px; margin-bottom:12px; border:1px solid #ddd;"
        ),

        tags$h4("About This Dashboard", style = "margin-top:0;"),

        tags$p(
          "The Yakima Basin contains five federal reservoirs operated by the",
          " U.S. Bureau of Reclamation: Bumping Lake, Cle Elum Lake,",
          " Kachess Dam, Keechelus Lake, and Rimrock ",
          "Total system capacity is approximately 1.07 million acre-feet."
        ),
        tags$p(
          "In addition to the five major constructed reservoirs, the basin depends",
          "heavily on mountain snowpack. In the Yakima watershed, snowpack",
          "has been referred to as the sixth reservoir."
        ),
        tags$p(
          "This dashboard combines daily reservoir storage values from USBR Hydromet with ",
          "modeled estimates of snowpack volume using SNODAS data from the National",
"Operational Hydrologic Remote Sensing Center (NOHRSC) and Climate Engine.",
          "This provides an overall estimate of total available water storage: snow storage +",
           "reservoir storage. The Climate Explorer places current ",
          "conditions in historical context using East Cascades temperature and ",
          "precipitation anomalies from NOAA NCEI."
        ),

        tags$hr(style = "margin: 10px 0;"),

        # Data freshness
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

        # Sources
        tags$div(
          style = "font-size:10px; color:#888; line-height:1.6;",
          tags$strong("Data Sources"), tags$br(),
          "USBR Hydromet (dam storage)", tags$br(),
          "NOHRSC / SNODAS (snowpack SWE)", tags$br(),
          "NOAA NCEI Climate-at-a-Glance",
          tags$br(),
          "Contact: jeffjmarti at gmail.com"
        )
      )
    ),

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
            title = "Climate Explorer",
            value = "explorer",
            br(),
            mod_explorer_ui("explorer")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  mod_storage_server("storage")
  mod_explorer_server("explorer")
}

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
