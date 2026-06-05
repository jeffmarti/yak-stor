# ==============================================================================
# fetch_bor_discharge.R
#
# Fetches daily observed (QD) and unregulated (QU) discharge from USBR
# Hydromet for five Yakima Basin reservoirs. Aggregates to monthly means
# and totals for comparison against NWRFC natural flow data.
#
# Source : https://www.usbr.gov/pn-bin/daily.pl
# Units  : cfs (daily) → converted to acre-feet/month on output
#
# QD = Average Stream Discharge (observed/regulated, cfs)
# QU = Estimated Average Unregulated Flow (naturalized, cfs)
#
# Validation note: NWRFC monthly natural flow values have been confirmed
# to track BOR QU very closely (within ~1-2%), validating NWRFC as an
# unregulated flow source. QD is retained here for the regulated outlook.
#
# Output schema:
#   year, month, reservoir, pcode, mean_cfs, n_days, volume_af, water_year
#
# Saves to: data/bor_discharge_monthly.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(readr)
  library(dplyr)
  library(lubridate)
})

# -- Station definitions -------------------------------------------------------
# Must match reservoir codes used in nwrfc_runoff_monthly.csv and
# yakima_dam_daily.csv for clean joins downstream.

bor_discharge_stations <- tibble::tribble(
  ~reservoir, ~description,
  "BUM",      "Bumping Lake",
  "CLE",      "Cle Elum Lake",
  "KAC",      "Kachess Dam",
  "KEE",      "Keechelus Dam",
  "RIM",      "Rimrock Lake (Tieton Dam)"
)

# Conversion factor: mean daily cfs × days → acre-feet
# 1 cfs × 1 day = 86,400 ft³ ÷ 43,560 ft³/AF = 1.98347 AF
CFS_TO_AF <- 86400 / 43560

# -- Helper: fetch one reservoir -----------------------------------------------

fetch_bor_discharge_station <- function(reservoir, description,
                                        start_date  = as.Date("2003-10-01"),
                                        end_date    = Sys.Date(),
                                        retries     = 3,
                                        timeout_sec = 60) {
  
  url <- paste0(
    "https://www.usbr.gov/pn-bin/daily.pl",
    "?station=",  reservoir,
    "&format=csv",
    "&flags=false",
    "&description=false",
    "&year=",  year(start_date),
    "&month=", month(start_date),
    "&day=",   day(start_date),
    "&year=",  year(end_date),
    "&month=", month(end_date),
    "&day=",   day(end_date),
    "&pcode=QD&pcode=QU"
  )
  
  message(sprintf("  Fetching discharge %s (%s)...", reservoir, description))
  
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      
      resp <- GET(url, timeout(timeout_sec))
      if (http_error(resp)) stop("HTTP error: ", status_code(resp))
      
      raw_text <- content(resp, as = "text", encoding = "UTF-8")
      
      # BOR CSV format: line 1 is always the header (DateTime, {res}_qd, {res}_qu),
      # line 2 onward is data — no header-skip needed.
      # Verify response has content before parsing.
      if (nchar(trimws(raw_text)) == 0) stop("Empty response from BOR endpoint")
      
      # Column names from BOR CSV: DateTime, {res}_qd, {res}_qu
      # e.g. for BUM: DateTime, bum_qd, bum_qu
      df_wide <- read_csv(
        I(raw_text),
        show_col_types = FALSE
      ) %>%
        rename_with(tolower)
      
      # Pivot wide → long so each row is one date × pcode
      res_lower <- tolower(reservoir)
      
      df_long <- df_wide %>%
        select(
          datetime,
          qd = matches(paste0(res_lower, "_qd")),
          qu = matches(paste0(res_lower, "_qu"))
        ) %>%
        tidyr::pivot_longer(
          cols      = c(qd, qu),
          names_to  = "pcode",
          values_to = "value_cfs"
        ) %>%
        mutate(
          date      = as.Date(datetime),
          reservoir = reservoir,
          pcode     = toupper(pcode)        # "qd" → "QD", "qu" → "QU"
        ) %>%
        filter(!is.na(date), !is.na(value_cfs)) %>%
        select(date, reservoir, pcode, value_cfs)
      
      message(sprintf("    %s: %d daily rows (%s to %s)",
                      reservoir, nrow(df_long),
                      format(min(df_long$date)), format(max(df_long$date))))
      df_long
      
    }, error = function(e) {
      message(sprintf("  Attempt %d/%d failed for %s: %s",
                      attempt, retries, reservoir, conditionMessage(e)))
      NULL
    })
    
    if (!is.null(result)) return(result)
    if (attempt < retries) {
      message("  Waiting 5 sec before retry...")
      Sys.sleep(5)
    }
  }
  
  warning(sprintf("All retries exhausted for station %s", reservoir))
  return(NULL)
}

# -- Main fetch function -------------------------------------------------------

fetch_bor_discharge <- function(data_dir   = "data",
                                start_date = as.Date("2003-10-01"),
                                end_date   = Sys.Date()) {
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  output_file <- file.path(data_dir, "bor_discharge_monthly.csv")
  
  message(sprintf(
    "\nFetching BOR discharge (QD + QU) for %d stations (%s to %s)...",
    nrow(bor_discharge_stations),
    format(start_date), format(end_date)
  ))
  
  # -- Fetch all five reservoirs daily data ------------------------------------
  all_daily <- vector("list", nrow(bor_discharge_stations))
  
  for (i in seq_len(nrow(bor_discharge_stations))) {
    all_daily[[i]] <- fetch_bor_discharge_station(
      reservoir   = bor_discharge_stations$reservoir[i],
      description = bor_discharge_stations$description[i],
      start_date  = start_date,
      end_date    = end_date
    )
    if (i < nrow(bor_discharge_stations)) Sys.sleep(runif(1, 1, 2))  # polite delay
  }
  
  daily_combined <- bind_rows(all_daily)
  
  if (nrow(daily_combined) == 0) {
    warning("No discharge data retrieved from any station.")
    return(invisible(NULL))
  }
  
  # -- Aggregate daily cfs → monthly acre-feet ----------------------------------
  # n_days tracks how many daily observations went into each monthly mean,
  # which flags any months with missing days.
  
  monthly <- daily_combined %>%
    mutate(
      year  = year(date),
      month = month(date)
    ) %>%
    group_by(reservoir, pcode, year, month) %>%
    summarise(
      mean_cfs  = mean(value_cfs, na.rm = TRUE),
      n_days    = n(),
      volume_af = mean_cfs * n_days * CFS_TO_AF,
      .groups   = "drop"
    ) %>%
    mutate(
      water_year = if_else(month >= 10L, year + 1L, year)
    ) %>%
    arrange(reservoir, pcode, year, month)
  
  # -- Save --------------------------------------------------------------------
  write_csv(monthly, output_file)
  
  message(sprintf("\nBOR discharge data saved: %s", output_file))
  message(sprintf("  Rows       : %d", nrow(monthly)))
  message(sprintf("  Reservoirs : %s",
                  paste(sort(unique(monthly$reservoir)), collapse = ", ")))
  message(sprintf("  Pcodes     : %s",
                  paste(sort(unique(monthly$pcode)), collapse = ", ")))
  message(sprintf("  Years      : %d to %d",
                  min(monthly$year), max(monthly$year)))
  
  # Warn on any missing reservoirs
  missing <- setdiff(bor_discharge_stations$reservoir, unique(monthly$reservoir))
  if (length(missing) > 0)
    warning(sprintf("Missing reservoirs: %s", paste(missing, collapse = ", ")))
  
  invisible(monthly)
}

# -- Standalone execution ------------------------------------------------------

if (sys.nframe() == 0L) {
  args     <- commandArgs(trailingOnly = TRUE)
  data_dir <- if (length(args) >= 1) args[1] else "data"
  message(sprintf("Running standalone -- data_dir = '%s'", data_dir))
  fetch_bor_discharge(data_dir = data_dir)
}