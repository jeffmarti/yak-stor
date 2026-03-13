# =============================================================================
# nohrsc_yakima_incremental.R
#
# NOHRSC Yakima Basin SWE Scraper -- Incremental Updater
# Uses graph_only.php endpoint (fast, ~32KB vs ~507KB for graph.html)
# English units (inches) via units=0
# Hardcoded basin acres derived from NOHRSC snapshot 2026-03-11
#
# Exposes: update_nohrsc_data(data_dir = "data")
#   -- called daily by update_pipeline.R via GitHub Actions
#   -- can also be sourced and called interactively
#   -- or run standalone: Rscript nohrsc_yakima_incremental.R
#
# Data flow:
#   NOHRSC graph_only.php
#       -> hourly HTML tables
#       -> daily sub-basin means      -> nohrsc_yakima_subbasin_daily.csv
#       -> reservoir-level rollup     -> nohrsc_yakima_reservoir_daily.csv
#       -> stitch with CE history     -> yakima_swe_combined.csv
#
# Output schema (reservoir + combined files):
#   Date, reservoir, swe_mean_in, acres, snow_storage_acre_feet, source, water_year
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
  library(lubridate)
  library(readr)
})

# =============================================================================
# CONSTANTS (module-level, available when sourced)
# =============================================================================

.yakima_basins <- c(
  "BUDW1IL", "BUDW1IU", "CLUW1IL", "CLUW1IU",
  "KADW1IL", "KADW1IU", "KEDW1IL", "KEDW1IU",
  "RILW1IG", "RILW1IL", "RILW1IU"
)

.basin_to_reservoir <- c(
  BUDW1IL = "BUM", BUDW1IU = "BUM",
  CLUW1IL = "CLE", CLUW1IU = "CLE",
  KADW1IL = "KAC", KADW1IU = "KAC",
  KEDW1IL = "KEE", KEDW1IU = "KEE",
  RILW1IL = "RIM", RILW1IG = "RIM", RILW1IU = "RIM"
)

# Sub-basin acres derived from NOHRSC snapshot 2026-03-11
# Calculated as: AcreFeet * 12 / Mean_SWE_inches
.basin_acres <- c(
  BUDW1IL = 16732,
  BUDW1IU = 28826,
  CLUW1IL = 46923,
  CLUW1IU = 78639,
  KADW1IL = 20598,
  KADW1IU = 16270,
  KEDW1IL = 20603,
  KEDW1IU = 11231,
  RILW1IG =  3807,
  RILW1IL = 43942,
  RILW1IU = 70273
)

# Reservoir acres = sum of constituent sub-basin acres
.reservoir_acres <- c(
  BUM = 16732 + 28826,           #  45558
  CLE = 46923 + 78639,           # 125562
  KAC = 20598 + 16270,           #  36868
  KEE = 20603 + 11231,           #  31834
  RIM =  3807 + 43942 + 70273    # 118022
)

# CE (Climate Engine) source acres -- used when rebuilding combined CSV
# These differ slightly from NOHRSC acres due to basin delineation method
.ce_acres <- c(
  BUM = 44415,
  CLE = 129466,
  KAC = 40473,
  KEE = 34843,
  RIM = 119663
)

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.build_url <- function(basin, start_date, end_date) {
  paste0(
    "https://www.nohrsc.noaa.gov/interactive/html/graph_only.php",
    "?w=600&h=400",
    "&by=",  year(start_date),
    "&bm=",  sprintf("%02d", month(start_date)),
    "&bd=",  sprintf("%02d", day(start_date)),
    "&bh=00",
    "&ey=",  year(end_date),
    "&em=",  sprintf("%02d", month(end_date)),
    "&ed=",  sprintf("%02d", day(end_date)),
    "&eh=23",
    "&data=1&region=us&units=0&brfc=nwrfc&basin=", basin
  )
}

.fetch_basin <- function(basin, start_date, end_date,
                          retries = 3, wait_sec = 15, timeout_sec = 120) {
  url <- .build_url(basin, start_date, end_date)
  message(sprintf("  Fetching %s (%s to %s)...", basin, start_date, end_date))

  for (attempt in seq_len(retries)) {
    result <- tryCatch({

      tmp <- tempfile(fileext = ".html")
      on.exit(unlink(tmp), add = TRUE)

      resp <- GET(url, write_disk(tmp, overwrite = TRUE), timeout(timeout_sec))
      if (http_error(resp)) stop("HTTP error: ", status_code(resp))

      page     <- read_html(tmp)
      tbl_node <- html_element(page, "table.data_table")

      if (inherits(tbl_node, "xml_missing") || is.null(tbl_node))
        stop("Table 'data_table' not found in page response.")

      data_table <- html_table(tbl_node, fill = TRUE)

      if (is.null(data_table) || nrow(data_table) < 12)
        stop("No data table or too few rows.")

      # Data rows start at row 11; 8 columns
      # Column order: datetime, snow_cover_pct,
      #               swe_min, swe_mean, swe_max,
      #               depth_min, depth_mean, depth_max
      df_raw <- data_table[11:nrow(data_table), 1:8]
      names(df_raw) <- c("datetime",
                         "snow_cover_pct",
                         "swe_min_in",   "swe_mean_in",   "swe_max_in",
                         "depth_min_in", "depth_mean_in", "depth_max_in")

      df <- df_raw %>%
        filter(!is.na(datetime), datetime != "",
               !grepl("^Date", datetime)) %>%
        mutate(
          datetime = as.POSIXct(datetime,
                                format = "%Y-%m-%d %H", tz = "UTC"),
          across(snow_cover_pct:depth_max_in,
                 ~ suppressWarnings(as.numeric(.)))
        ) %>%
        filter(!is.na(datetime)) %>%
        mutate(Basin = basin)

      message(sprintf("    %s: %d hourly rows (%s to %s)",
                      basin, nrow(df),
                      format(min(df$datetime)),
                      format(max(df$datetime))))
      df

    }, error = function(e) {
      message(sprintf("  Attempt %d/%d failed: %s",
                      attempt, retries, conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) return(result)
    if (attempt < retries) {
      message(sprintf("  Waiting %d sec before retry...", wait_sec))
      Sys.sleep(wait_sec)
    }
  }
  warning(sprintf("All retries exhausted for basin %s", basin))
  NULL
}

# =============================================================================
# MAIN EXPORTED FUNCTION
# =============================================================================

update_nohrsc_data <- function(data_dir = "data") {

  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  # File paths -- all relative to data_dir
  history_file  <- file.path(data_dir, "nohrsc_yakima_daily_FINAL.csv")
  subbasin_file <- file.path(data_dir, "nohrsc_yakima_subbasin_daily.csv")
  output_file   <- file.path(data_dir, "nohrsc_yakima_reservoir_daily.csv")
  new_data_file <- file.path(data_dir, "nohrsc_yakima_new_data.csv")
  combined_file <- file.path(data_dir, "yakima_swe_combined.csv")

  # ---------------------------------------------------------------------------
  # DETERMINE START DATE
  # Priority: (1) existing reservoir file, (2) old history file, (3) default
  # ---------------------------------------------------------------------------

  if (file.exists(output_file)) {
    existing   <- read_csv(output_file, show_col_types = FALSE) %>%
      mutate(Date = as.Date(Date))
    date_start <- as.Date(max(existing$Date, na.rm = TRUE)) + 1L
    message(sprintf("Reservoir file found through %s -- fetching from %s onward",
                    date_start - 1L, date_start))
    message(sprintf("Rows in existing file: %d", nrow(existing)))

  } else if (file.exists(history_file)) {
    hist_df    <- read_csv(history_file, show_col_types = FALSE) %>%
      mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
    date_start <- as.Date(max(hist_df$Date, na.rm = TRUE)) + 1L
    existing   <- NULL
    message(sprintf(
      "Old history file found through %s -- fetching from %s onward",
      date_start - 1L, date_start))
    message("Note: old file schema incompatible; writing fresh reservoir file")

  } else {
    existing   <- NULL
    date_start <- as.Date("2012-10-01")
    message("No existing files found -- fetching full record from 2012-10-01")
  }

  date_end <- Sys.Date()

  if (date_start > date_end) {
    message("Already up to date. Nothing to fetch.")
    return(invisible(NULL))
  }

  message(sprintf("Date range to fetch: %s to %s  (%d days)",
                  date_start, date_end,
                  as.integer(date_end - date_start) + 1L))

  # ---------------------------------------------------------------------------
  # FETCH -- split into yearly chunks (NOHRSC limit ~1 year per request)
  # ---------------------------------------------------------------------------

  year_starts <- seq(date_start, date_end, by = "year")
  year_ends   <- c(year_starts[-1] - 1L, date_end)

  message(sprintf("\n%d yearly chunk(s) \u00d7 %d basins = %d total requests",
                  length(year_starts), length(.yakima_basins),
                  length(year_starts) * length(.yakima_basins)))

  all_hourly <- list()

  for (basin in .yakima_basins) {
    message(sprintf("\n===== Basin: %s =====", basin))
    basin_chunks <- list()

    for (i in seq_along(year_starts)) {
      chunk <- .fetch_basin(basin, year_starts[i], year_ends[i])
      if (!is.null(chunk)) basin_chunks[[i]] <- chunk
      Sys.sleep(runif(1, 2, 4))
    }

    if (length(basin_chunks) > 0)
      all_hourly[[basin]] <- bind_rows(basin_chunks)
  }

  if (length(all_hourly) == 0) {
    message("No data retrieved from NOHRSC. Exiting without updating files.")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------------
  # STEP 1: Aggregate hourly -> daily at sub-basin level
  # ---------------------------------------------------------------------------

  new_subbasin <- bind_rows(all_hourly) %>%
    mutate(Date = as.Date(datetime)) %>%
    group_by(Date, Basin) %>%
    summarise(
      snow_cover_pct = mean(snow_cover_pct, na.rm = TRUE),
      swe_min_in     = min(swe_min_in,      na.rm = TRUE),
      swe_mean_in    = mean(swe_mean_in,    na.rm = TRUE),
      swe_max_in     = max(swe_max_in,      na.rm = TRUE),
      depth_min_in   = min(depth_min_in,    na.rm = TRUE),
      depth_mean_in  = mean(depth_mean_in,  na.rm = TRUE),
      depth_max_in   = max(depth_max_in,    na.rm = TRUE),
      n_hours        = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_acres             = .basin_acres[Basin],
      snow_storage_acre_feet = swe_mean_in / 12 * area_acres,
      reservoir              = .basin_to_reservoir[Basin]
    ) %>%
    arrange(Basin, Date)

  message(sprintf("\nSub-basin daily rows: %d  (%s to %s)",
                  nrow(new_subbasin),
                  min(new_subbasin$Date), max(new_subbasin$Date)))

  write_csv(new_subbasin, new_data_file)
  message(sprintf("Sub-basin staging written to: %s", new_data_file))

  # ---------------------------------------------------------------------------
  # STEP 2: Roll up sub-basin -> reservoir level
  # ---------------------------------------------------------------------------

  new_reservoir <- new_subbasin %>%
    group_by(Date, reservoir) %>%
    summarise(
      snow_storage_acre_feet = sum(snow_storage_acre_feet, na.rm = TRUE),
      n_subbasins            = n(),
      .groups = "drop"
    ) %>%
    mutate(
      acres       = .reservoir_acres[reservoir],
      swe_mean_in = snow_storage_acre_feet / acres * 12,
      source      = "NOHRSC",
      water_year  = if_else(month(Date) >= 10,
                            year(Date) + 1L, year(Date))
    ) %>%
    select(Date, reservoir, swe_mean_in, acres,
           snow_storage_acre_feet, source, water_year) %>%
    arrange(reservoir, Date)

  # ---------------------------------------------------------------------------
  # STEP 3: Append to master reservoir file
  # ---------------------------------------------------------------------------

  if (!is.null(existing) && nrow(existing) > 0) {
    combined_res <- bind_rows(existing, new_reservoir) %>%
      distinct(Date, reservoir, .keep_all = TRUE) %>%
      arrange(reservoir, Date)
    message(sprintf(
      "\nMerge: %d existing + %d new = %d combined reservoir rows",
      nrow(existing), nrow(new_reservoir), nrow(combined_res)))
  } else {
    combined_res <- new_reservoir
    message(sprintf("\nNew reservoir file: %d rows", nrow(combined_res)))
  }

  # Append to sub-basin file
  if (file.exists(subbasin_file)) {
    existing_sub <- read_csv(subbasin_file, show_col_types = FALSE) %>%
      mutate(Date = as.Date(Date))
    combined_sub <- bind_rows(existing_sub, new_subbasin) %>%
      distinct(Date, Basin, .keep_all = TRUE) %>%
      arrange(Basin, Date)
  } else {
    combined_sub <- new_subbasin
  }

  write_csv(combined_res, output_file)
  write_csv(combined_sub, subbasin_file)

  message(sprintf("Reservoir file updated: %s  (%d rows, %s to %s)",
                  output_file, nrow(combined_res),
                  min(combined_res$Date), max(combined_res$Date)))
  message(sprintf("Sub-basin file updated: %s  (%d rows)",
                  subbasin_file, nrow(combined_sub)))

  # ---------------------------------------------------------------------------
  # STEP 4: Rebuild yakima_swe_combined.csv
  # Stitches existing combined (may contain CE data pre-2012) with the
  # updated NOHRSC reservoir file. CE rows are preserved as-is; NOHRSC
  # rows replace everything from 2012-10-01 onward.
  # ---------------------------------------------------------------------------

  message("\nRebuilding yakima_swe_combined.csv...")

  nohrsc_start <- as.Date("2012-10-01")

  # Keep any CE rows that predate the NOHRSC period
ce_seed_file <- file.path(data_dir, "yakima_swe_ce_seed.csv")

if (file.exists(ce_seed_file)) {
  ce_rows <- read_csv(ce_seed_file, show_col_types = FALSE) %>%
    mutate(Date = as.Date(Date))
  message(sprintf("  CE seed: %d rows (%s to %s)",
                  nrow(ce_rows), min(ce_rows$Date), max(ce_rows$Date)))
} else {
  ce_rows <- tibble()
  message("  No CE seed file found -- NOHRSC data only")
}

  # Add system ALL totals to the reservoir file
  all_total <- combined_res %>%
    group_by(Date, water_year) %>%
    summarise(
      snow_storage_acre_feet = sum(snow_storage_acre_feet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      reservoir   = "ALL",
      acres       = sum(.reservoir_acres),
      swe_mean_in = snow_storage_acre_feet / acres * 12,
      source      = "NOHRSC"
    ) %>%
    select(Date, reservoir, swe_mean_in, acres,
           snow_storage_acre_feet, source, water_year)

  nohrsc_full <- bind_rows(combined_res, all_total) %>%
    arrange(reservoir, Date)

  # Stitch: CE rows (pre-2012) + NOHRSC rows (2012+)
  if (nrow(ce_rows) > 0) {
    ce_clean <- ce_rows %>%
      filter(Date < nohrsc_start) %>%
      select(Date, reservoir, swe_mean_in, acres,
             snow_storage_acre_feet, source, water_year)

    new_combined <- bind_rows(ce_clean, nohrsc_full) %>%
      distinct(Date, reservoir, .keep_all = TRUE) %>%
      arrange(reservoir, Date)
  } else {
    new_combined <- nohrsc_full
  }

  write_csv(new_combined, combined_file)

  message(sprintf("Combined file written: %s", combined_file))
  message(sprintf("  Rows  : %d", nrow(new_combined)))
  message(sprintf("  Range : %s to %s",
                  min(new_combined$Date), max(new_combined$Date)))
  message(sprintf("  Sources: %s",
                  paste(unique(new_combined$source), collapse = ", ")))

  message("\n===== update_nohrsc_data() complete =====\n")
  invisible(new_combined)
}

# =============================================================================
# STANDALONE EXECUTION
# Run directly with: Rscript nohrsc_yakima_incremental.R [data_dir]
# =============================================================================

if (sys.nframe() == 0L) {
  args     <- commandArgs(trailingOnly = TRUE)
  data_dir <- if (length(args) >= 1) args[1] else "data"
  message(sprintf("Running standalone -- data_dir = '%s'", data_dir))
  update_nohrsc_data(data_dir = data_dir)
}
