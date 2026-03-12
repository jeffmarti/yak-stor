# =============================================================================
# Yakima Basin — USBR Reservoir Storage Fetch Function
# Pulls daily acre-feet from USBR Hydromet API for all five reservoirs
# Returns a tidy data frame ready to join against yakima_swe_combined.csv
#
# Output schema:
#   Date, reservoir, dam_storage_acre_feet, source, water_year
# =============================================================================

library(httr)
library(readr)
library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# USBR STATION CONFIG
# -----------------------------------------------------------------------------

usbr_stations <- tibble::tribble(
  ~station_id, ~reservoir, ~description,
  "BUM",       "BUM",      "Bumping Lake",
  "CLE",       "CLE",      "Cle Elum Lake",
  "KAC",       "KAC",      "Kachess Dam",
  "KEE",       "KEE",      "Keechelus Dam",
  "RIM",       "RIM",      "Rimrock Lake (Tieton Dam)"
)

# -----------------------------------------------------------------------------
# HELPER: FETCH ONE STATION
# -----------------------------------------------------------------------------

fetch_usbr_station <- function(station_id, reservoir, description,
                               start_date = as.Date("2003-10-01"),
                               end_date   = Sys.Date(),
                               retries    = 3,
                               timeout_sec = 60) {
  
  url <- paste0(
    "https://www.usbr.gov/pn-bin/daily.pl",
    "?station=", station_id,
    "&format=csv",
    "&year=",  year(start_date),
    "&month=", month(start_date),
    "&day=",   day(start_date),
    "&year=",  year(end_date),
    "&month=", month(end_date),
    "&day=",   day(end_date),
    "&pcode=AF"
  )
  
  message(sprintf("  Fetching %s (%s)...", reservoir, description))
  
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      
      resp <- GET(url, timeout(timeout_sec))
      if (http_error(resp)) stop("HTTP error: ", status_code(resp))
      
      raw_text <- content(resp, as = "text", encoding = "UTF-8")
      
      # Skip header lines — find first line that looks like a date
      lines     <- strsplit(raw_text, "\n")[[1]]
      data_start <- which(grepl("^\\d{4}-\\d{2}-\\d{2}|^\\d{1,2}/\\d{1,2}/\\d{4}",
                                lines))[1]
      if (is.na(data_start)) stop("Could not find data rows in response")
      
      df <- read_csv(
        paste(lines[data_start:length(lines)], collapse = "\n"),
        col_names = c("Date", "dam_storage_acre_feet"),
        col_types = cols(
          Date                  = col_character(),
          dam_storage_acre_feet = col_double()
        ),
        show_col_types = FALSE
      ) %>%
        filter(!is.na(Date), Date != "") %>%
        mutate(
          # Handle both YYYY-MM-DD and M/D/YYYY formats
          Date = case_when(
            grepl("^\\d{4}-\\d{2}-\\d{2}$", Date) ~ as.Date(Date),
            grepl("/", Date) ~ as.Date(Date, format = "%m/%d/%Y"),
            TRUE ~ NA_Date_
          )
        ) %>%
        filter(!is.na(Date), !is.na(dam_storage_acre_feet)) %>%
        mutate(
          reservoir  = reservoir,
          source     = "USBR"
        ) %>%
        select(Date, reservoir, dam_storage_acre_feet, source)
      
      message(sprintf("    %s: %d rows (%s to %s)",
                      reservoir, nrow(df),
                      format(min(df$Date)), format(max(df$Date))))
      df
      
    }, error = function(e) {
      message(sprintf("  Attempt %d/%d failed for %s: %s",
                      attempt, retries, station_id, conditionMessage(e)))
      NULL
    })
    
    if (!is.null(result)) return(result)
    if (attempt < retries) {
      message(sprintf("  Waiting 5 sec before retry..."))
      Sys.sleep(5)
    }
  }
  
  warning(sprintf("All retries exhausted for station %s", station_id))
  return(NULL)
}

# -----------------------------------------------------------------------------
# MAIN FUNCTION: FETCH ALL FIVE RESERVOIRS
# -----------------------------------------------------------------------------

#' Fetch USBR daily reservoir storage for all five Yakima Basin reservoirs
#'
#' @param start_date  Date — fetch from this date (default 2003-10-01)
#' @param end_date    Date — fetch through this date (default today)
#' @return Tidy data frame: Date, reservoir, dam_storage_acre_feet,
#'         source, water_year

fetch_usbr_storage <- function(start_date = as.Date("2003-10-01"),
                               end_date   = Sys.Date()) {
  
  message(sprintf("Fetching USBR storage: %s to %s", start_date, end_date))
  message(sprintf("Reservoirs: %s",
                  paste(usbr_stations$reservoir, collapse = ", ")))
  
  results <- vector("list", nrow(usbr_stations))
  
  for (i in seq_len(nrow(usbr_stations))) {
    results[[i]] <- fetch_usbr_station(
      station_id  = usbr_stations$station_id[i],
      reservoir   = usbr_stations$reservoir[i],
      description = usbr_stations$description[i],
      start_date  = start_date,
      end_date    = end_date
    )
    if (i < nrow(usbr_stations)) Sys.sleep(runif(1, 1, 2))  # polite delay
  }
  
  # Bind and add water year
  combined <- bind_rows(results) %>%
    filter(!is.na(Date)) %>%
    mutate(
      water_year = if_else(month(Date) >= 10,
                           year(Date) + 1L, year(Date))
    ) %>%
    arrange(reservoir, Date)
  
  # Summary
  message(sprintf("\nFetch complete:"))
  message(sprintf("  Total rows  : %d", nrow(combined)))
  message(sprintf("  Date range  : %s to %s",
                  min(combined$Date), max(combined$Date)))
  message(sprintf("  Reservoirs  : %s",
                  paste(sort(unique(combined$reservoir)), collapse = ", ")))
  message(sprintf("  Water years : %d to %d",
                  min(combined$water_year), max(combined$water_year)))
  
  # Warn on any missing reservoirs
  missing <- setdiff(usbr_stations$reservoir, unique(combined$reservoir))
  if (length(missing) > 0)
    warning(sprintf("Missing reservoirs: %s", paste(missing, collapse = ", ")))
  
  combined
}

# -----------------------------------------------------------------------------
# EXAMPLE USAGE (run interactively to test)
# -----------------------------------------------------------------------------

# # Fetch full period of record
# dam_storage <- fetch_usbr_storage()
#
# # Fetch just the current water year
# dam_storage <- fetch_usbr_storage(start_date = as.Date("2025-10-01"))
#
# # Join with SWE combined data
# library(readr)
# swe <- read_csv("yakima_swe_combined.csv")
#
# combined <- swe %>%
#   left_join(dam_storage %>% select(Date, reservoir,
#                                    dam_storage_acre_feet, water_year),
#             by = c("Date", "reservoir")) %>%
#   mutate(
#     total_storage_acre_feet = snow_storage_acre_feet + dam_storage_acre_feet
#   )