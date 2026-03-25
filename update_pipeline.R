Sys.setenv(RENV_CONFIG_AUTO_ACTIVATE = "FALSE")
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

# ==============================================================================
# update_pipeline.R
#
# Daily data refresh -- run by GitHub Actions at 05:42 UTC.
# Fetches fresh data from USBR, NOHRSC, and NCEI; writes pre-computed CSVs
# to the data/ folder. The Shiny app reads these CSVs on startup (no API calls).
#
# Usage (local):
#   Rscript update_pipeline.R
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(jsonlite)
})

# -- Source fetch functions ----------------------------------------------------
source("fetch_usbr_storage.R")
source("nohrsc_yakima_incremental.R")
source("fetch_nwrfc_runoff.R")

cat(sprintf("\n=== Yakima data pipeline -- %s ===\n\n", Sys.time()))

data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

# ==============================================================================
# 1. USBR DAM STORAGE
# ==============================================================================

cat("Step 1: Fetching USBR dam storage...\n")

dam_all <- tryCatch({
  fetch_usbr_storage(
    start_date = as.Date("1990-10-01"),
    end_date   = Sys.Date()
  )
}, error = function(e) {
  cat("  ERROR fetching USBR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(dam_all) && nrow(dam_all) > 0) {
  write_csv(dam_all, file.path(data_dir, "yakima_dam_daily.csv"))
  cat(sprintf("  Saved %d rows  (%s to %s)\n",
              nrow(dam_all),
              format(min(dam_all$Date), "%Y-%m-%d"),
              format(max(dam_all$Date), "%Y-%m-%d")))
} else {
  cat("  SKIPPED -- fetch returned no data\n")
}

# ==============================================================================
# 2. NOHRSC SWE INCREMENTAL UPDATE
# ==============================================================================

cat("\nStep 2: Updating NOHRSC SWE data...\n")

tryCatch({
  update_nohrsc_data(data_dir = data_dir)
  cat("  NOHRSC update complete\n")
}, error = function(e) {
  cat("  ERROR in NOHRSC update:", conditionMessage(e), "\n")
})

# ==============================================================================
# 3. NCEI MONTHLY CLIMATE
# ==============================================================================

cat("\nStep 3: Fetching NCEI monthly climate...\n")

fetch_ncei_csv <- function(variable) {
  url <- sprintf(
    paste0("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
           "divisional/time-series/4506/%s/1/0/1895-2026/data.csv",
           "?base_prd=true&begbaseyear=1991&endbaseyear=2020"),
    variable
  )

  resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning(sprintf("NCEI CSV fetch failed for variable: %s", variable))
    return(NULL)
  }

  text <- httr::content(resp, as = "text", encoding = "UTF-8")

  # Strip comment lines (start with #)
  lines      <- strsplit(text, "\n")[[1]]
  data_lines <- lines[!startsWith(trimws(lines), "#") & nchar(trimws(lines)) > 0]
  clean_text <- paste(data_lines, collapse = "\n")

  df <- tryCatch(
    read.csv(text = clean_text, stringsAsFactors = FALSE),
    error = function(e) { warning("CSV parse failed: ", e$message); NULL }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # CSV columns: Date (YYYYMM integer), Value, Anomaly
  df %>%
    rename(yyyymm = Date, value = Value, anomaly = Anomaly) %>%
    mutate(
      yyyymm   = sprintf("%06d", as.integer(yyyymm)),  # zero-pad e.g. "198901"
      variable = variable,
      year     = as.integer(substr(yyyymm, 1, 4)),
      month    = as.integer(substr(yyyymm, 5, 6))      # "09" -> 9, "01" -> 1
    ) %>%
    filter(!is.na(value), value > -99)
}

ncei_raw <- tryCatch({
  tavg <- fetch_ncei_csv("tavg")
  Sys.sleep(0.5)
  pcp  <- fetch_ncei_csv("pcp")
  bind_rows(tavg, pcp)
}, error = function(e) {
  cat("  ERROR fetching NCEI:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(ncei_raw) && nrow(ncei_raw) > 0) {
  write_csv(ncei_raw, file.path(data_dir, "ncei_climate_monthly.csv"))
  cat(sprintf("  Saved %d rows  (tavg + pcp, all months, 1895-%d)\n",
              nrow(ncei_raw), max(ncei_raw$year)))
  cat(sprintf("  Months present: %s\n",
              paste(sort(unique(ncei_raw$month)), collapse = ", ")))
} else {
  cat("  SKIPPED -- fetch returned no data\n")
}
# ==============================================================================
# 4. NWRFC MONTHLY RUNOFF
# ==============================================================================

cat("\nStep 4: Fetching NWRFC monthly runoff...\n")

tryCatch({
  fetch_nwrfc_runoff(data_dir = data_dir)
  cat("  NWRFC runoff update complete\n")
}, error = function(e) {
  cat("  ERROR in NWRFC runoff fetch:", conditionMessage(e), "\n")
})
# ==============================================================================
# SUMMARY
# ==============================================================================

cat(sprintf("\n=== Pipeline complete -- %s ===\n\n", Sys.time()))

for (f in c("yakima_dam_daily.csv", "yakima_swe_combined.csv",
            "ncei_climate_monthly.csv")) {
  fp <- file.path(data_dir, f)
  if (file.exists(fp)) {
    info <- file.info(fp)
    cat(sprintf("  %-35s  %.0f KB\n", f, info$size / 1024))
  } else {
    cat(sprintf("  %-35s  MISSING\n", f))
  }
}