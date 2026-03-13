 
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
#
# GitHub Actions runs this from the repo root. Working directory must be
# the repo root so relative paths resolve correctly.
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

DIVISION <- "4506"

fetch_ncei_month <- function(variable, month,
                              start_yr = 1895,
                              end_yr   = as.integer(format(Sys.Date(), "%Y"))) {
  url <- sprintf(
    paste0("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
           "divisional/time-series/%s/%s/1/%d/%d-%d/data.json"),
    DIVISION, variable, month, start_yr, end_yr
  )
  resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning(sprintf("NCEI fetch failed: %s month %02d", variable, month))
    return(NULL)
  }
  obj <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  tibble(
    yyyymm   = names(obj$data),
    value    = purrr::map_dbl(obj$data, ~ .x$value),
    variable = variable,
    month    = as.integer(substr(names(obj$data), 5, 6)),
    year     = as.integer(substr(names(obj$data), 1, 4))
  ) %>% dplyr::filter(!is.na(value))
}

ncei_raw <- tryCatch({
  bind_rows(
    purrr::map_dfr(1:12, ~ { Sys.sleep(0.15); fetch_ncei_month("tavg", .x) }),
    purrr::map_dfr(1:12, ~ { Sys.sleep(0.15); fetch_ncei_month("pcp",  .x) })
  )
}, error = function(e) {
  cat("  ERROR fetching NCEI:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(ncei_raw) && nrow(ncei_raw) > 0) {
  write_csv(ncei_raw, file.path(data_dir, "ncei_climate_monthly.csv"))
  cat(sprintf("  Saved %d rows  (tavg + pcp, 1895-%d)\n",
              nrow(ncei_raw), max(ncei_raw$year)))
} else {
  cat("  SKIPPED -- fetch returned no data\n")
}

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
