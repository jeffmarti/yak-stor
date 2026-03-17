# ==============================================================================
# fetch_nwrfc_runoff.R
#
# Fetches monthly water supply volumes from NWRFC for five Yakima Basin
# stations below each reservoir. Combines into a single long-format CSV.
#
# Source: https://www.nwrfc.noaa.gov/water_supply/ws_norm_text.cgi?id=RIMW1
# Units: KAF (thousands of acre-feet) -- converted to AF on output
#
# Output schema:
#   year, month, station_id, reservoir, volume_af, water_year
#
# Saves to: data/nwrfc_runoff_monthly.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# -- Station definitions -------------------------------------------------------

nwrfc_stations <- tibble(
  station_id = c("KEEW1", "KACW1", "CLEW1", "BUMW1", "RIMW1"),
  reservoir  = c("KEE",   "KAC",   "CLE",   "BUM",   "RIM"),
  label      = c(
    "Yakima - Near Martin",
    "Kachess - Near Easton",
    "Cle Elum - Near Roslyn",
    "Bumping - Below Bumping Dam",
    "Tieton - At Tieton Dam"
  )
)

# -- Fetch function ------------------------------------------------------------

fetch_one_station <- function(station_id, reservoir,
                              retries  = 3,
                              wait_sec = 10) {
  
  url <- paste0(
    "https://www.nwrfc.noaa.gov/water_supply/ws_norm_text.cgi?id=",
    station_id
  )
  
  message(sprintf("  Fetching %s (%s)...", station_id, reservoir))
  
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      
      # Fetch HTML response -- data is embedded in a <pre> block
      # with <br> as line separators rather than actual newlines
      lines <- readLines(url, warn = FALSE)
      
      # Extract the <pre> block and split on <br> tags
      pre_line <- lines[grepl("<pre>", lines, fixed = TRUE)]
      if (length(pre_line) == 0)
        stop("No <pre> block found in response.")
      
      raw  <- gsub("</?pre>", "", pre_line[1])
      rows <- unlist(strsplit(raw, "<br>"))
      
      # Keep only actual data lines -- station ID followed by 4-digit year
      data_lines <- rows[grepl("^[A-Z0-9]+,[0-9]{4},", rows)]
      
      if (length(data_lines) < 1)
        stop("No data lines found matching expected station/year pattern.")
      
      # Parse as CSV using our own column names
      csv_text <- paste(data_lines, collapse = "\n")
      
      df <- read_csv(
        I(csv_text),
        col_names = c("station_id", "year",
                      "Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec"),
        col_types = cols(
          station_id = col_character(),
          year       = col_double(),
          .default   = col_double()
        ),
        show_col_types = FALSE
      )
      
      # Pivot to long format
      df_long <- df %>%
        pivot_longer(
          cols      = Jan:Dec,
          names_to  = "month_name",
          values_to = "volume_kaf"
        ) %>%
        filter(!is.na(volume_kaf)) %>%
        mutate(
          month      = match(month_name, month.abb),
          year       = as.integer(year),
          volume_af  = volume_kaf * 1000,
          reservoir  = reservoir,
          water_year = if_else(month >= 10L, year + 1L, year)
        ) %>%
        select(year, month, station_id, reservoir, volume_af, water_year) %>%
        arrange(year, month)
      
      message(sprintf("    %s: %d monthly rows (%d to %d)",
                      station_id, nrow(df_long),
                      min(df_long$year), max(df_long$year)))
      df_long
      
    }, error = function(e) {
      message(sprintf("  Attempt %d/%d failed for %s: %s",
                      attempt, retries, station_id, conditionMessage(e)))
      NULL
    })
    
    if (!is.null(result)) return(result)
    if (attempt < retries) {
      message(sprintf("  Waiting %d sec before retry...", wait_sec))
      Sys.sleep(wait_sec)
    }
  }
  
  warning(sprintf("All retries exhausted for station %s", station_id))
  NULL
}

# -- Main fetch function -------------------------------------------------------

fetch_nwrfc_runoff <- function(data_dir   = "data",
                               start_year = 1991,
                               end_year   = as.integer(format(Sys.Date(), "%Y"))) {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  output_file <- file.path(data_dir, "nwrfc_runoff_monthly.csv")
  
  message(sprintf(
    "\nFetching NWRFC runoff data for %d stations (%d-%d)...",
    nrow(nwrfc_stations), start_year, end_year
  ))
  
  all_data <- list()
  
  for (i in seq_len(nrow(nwrfc_stations))) {
    df <- fetch_one_station(
      station_id = nwrfc_stations$station_id[i],
      reservoir  = nwrfc_stations$reservoir[i]
    )
    if (!is.null(df)) {
      all_data[[i]] <- df %>%
        filter(year >= start_year, year <= end_year)
    }
    Sys.sleep(runif(1, 1, 2))
  }
  
  if (length(all_data) == 0) {
    warning("No runoff data retrieved from any station.")
    return(invisible(NULL))
  }
  
  # Combine all stations
  combined <- bind_rows(all_data) %>%
    arrange(reservoir, year, month)
  
  # System total -- sum across all five stations by year/month
  system_total <- combined %>%
    group_by(year, month, water_year) %>%
    summarise(
      volume_af = sum(volume_af, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      station_id = "SYSTEM",
      reservoir  = "ALL"
    ) %>%
    select(year, month, station_id, reservoir, volume_af, water_year)
  
  final <- bind_rows(combined, system_total) %>%
    arrange(reservoir, year, month)
  
  write_csv(final, output_file)
  
  message(sprintf("\nRunoff data saved: %s", output_file))
  message(sprintf("  Rows     : %d", nrow(final)))
  message(sprintf("  Stations : %s",
                  paste(sort(unique(final$reservoir)), collapse = ", ")))
  message(sprintf("  Years    : %d to %d",
                  min(final$year), max(final$year)))
  
  invisible(final)
}

# -- Standalone execution ------------------------------------------------------

if (sys.nframe() == 0L) {
  args     <- commandArgs(trailingOnly = TRUE)
  data_dir <- if (length(args) >= 1) args[1] else "data"
  message(sprintf("Running standalone -- data_dir = '%s'", data_dir))
  fetch_nwrfc_runoff(data_dir = data_dir)
}