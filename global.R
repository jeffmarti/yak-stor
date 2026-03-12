# =============================================================================
# global.R  —  Yakima Basin Water Dashboard
#
# Runs ONCE per session. Reads pre-computed CSVs only (no API calls).
# Produces all shared data objects consumed by mod_storage and mod_explorer.
# CSVs are refreshed daily by update_pipeline.R via GitHub Actions.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(tidyverse)
  library(lubridate)
  library(conflicted)
})

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(plotly::layout)
conflicted::conflicts_prefer(plotly::config)

# -----------------------------------------------------------------------------
# PATHS
# -----------------------------------------------------------------------------

data_dir <- "data"   # relative to app root; GitHub Actions writes here

swe_file   <- file.path(data_dir, "yakima_swe_combined.csv")
dam_file   <- file.path(data_dir, "yakima_dam_daily.csv")
ncei_file  <- file.path(data_dir, "ncei_climate_monthly.csv")

# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

NORMAL_START  <- 1991
NORMAL_END    <- 2020
SNOW_YR_START <- 2004
PLOT_START    <- as.Date("2003-10-01")
DIVISION      <- "4506"   # WA Climate Division 6 — East Cascades

reservoir_labels <- c(
  BUM = "Bumping Lake",
  CLE = "Cle Elum Lake",
  KAC = "Kachess Dam",
  KEE = "Keechelus Dam",
  RIM = "Rimrock Lake",
  ALL = "System Total"
)
res_order <- c("BUM", "CLE", "KAC", "KEE", "RIM", "ALL")

reservoir_capacity <- c(
  BUM =   33970,
  CLE =  436900,
  KAC =  239000,
  KEE =  157800,
  RIM =  198000,
  ALL =   33970 + 436900 + 239000 + 157800 + 198000   # 1,065,670
)
system_capacity <- reservoir_capacity[["ALL"]]

# Colors — shared by both modules
col_dam          <- "#2166ac"   # blue        — dam storage
col_snow         <- "#92c5de"   # light blue  — snow storage (SWE)
col_capacity     <- "#d73027"   # red dashed  — dam capacity
col_dam_normal   <- "#000000"   # black dashed — dam avg 1991–2020
col_snow_normal  <- "#4dac26"   # green dashed — snow avg 2004–2025
col_combined     <- "#762a83"   # purple      — combined storage
col_warm         <- "#d73027"   # red         — warm temp anomaly
col_cool         <- "#2166ac"   # blue        — cool temp anomaly
col_wet          <- "#1a9641"   # green       — wet precip anomaly
col_dry          <- "#c8a951"   # tan         — dry precip anomaly
col_grid         <- "#eeeeee"

# X-axis: water year day → month label (for Storage module)
wy_months <- tibble(
  label     = c("Oct","Nov","Dec","Jan","Feb","Mar",
                "Apr","May","Jun","Jul","Aug","Sep"),
  day_start = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 335)
)

# -----------------------------------------------------------------------------
# LOAD CSVs
# -----------------------------------------------------------------------------

message("global.R: loading pre-computed data files...")

# ── SWE combined (Climate Engine 2003–2012 + NOHRSC 2012–present)
if (!file.exists(swe_file)) stop("Missing: ", swe_file,
  " — run update_pipeline.R first.")

swe_raw <- read_csv(swe_file, show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

# ── USBR dam storage (1990-10-01 to present, all reservoirs + ALL)
if (!file.exists(dam_file)) stop("Missing: ", dam_file,
  " — run update_pipeline.R first.")

dam_csv <- read_csv(dam_file, show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

# ── NCEI monthly climate (raw values, all 12 months, tavg + pcp)
if (!file.exists(ncei_file)) stop("Missing: ", ncei_file,
  " — run update_pipeline.R first.")

ncei_raw <- read_csv(ncei_file, show_col_types = FALSE)

# -----------------------------------------------------------------------------
# STORAGE MODULE DATA
# Produces: plot_data, dam_normal, snow_normal, all_wy, current_wy
# -----------------------------------------------------------------------------

message("global.R: building storage module data...")

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

# 1991–2020 dam normal (by reservoir + wy_day)
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

# 2004–2025 snow normal (by reservoir + wy_day)
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

all_wy     <- sort(unique(plot_data$water_year))
current_wy <- if_else(month(Sys.Date()) >= 10,
                      year(Sys.Date()) + 1L,
                      year(Sys.Date()))

# -----------------------------------------------------------------------------
# EXPLORER MODULE DATA
# Produces: daily, dam_daily_full, carryover, carryover_median,
#           temp_anom, precip_anom
# -----------------------------------------------------------------------------

message("global.R: building explorer module data...")

# ── System daily (PLOT_START to present)
swe_daily <- swe_raw %>%
  filter(reservoir != "ALL", Date >= PLOT_START) %>%
  group_by(Date) %>%
  summarise(snow_af = sum(snow_storage_acre_feet, na.rm = TRUE),
            .groups = "drop")

dam_daily_plot <- dam_csv %>%
  filter(reservoir != "ALL", Date >= PLOT_START) %>%
  group_by(Date) %>%
  summarise(dam_af = sum(dam_storage_acre_feet, na.rm = TRUE),
            .groups = "drop")

daily <- swe_daily %>%
  full_join(dam_daily_plot, by = "Date") %>%
  mutate(
    snow_af     = replace_na(snow_af, 0),
    dam_af      = replace_na(dam_af,  0),
    combined_af = snow_af + dam_af
  ) %>%
  arrange(Date)

# ── Full dam daily from 1990 (for carryover median)
dam_daily_full <- dam_csv %>%
  filter(reservoir != "ALL") %>%
  group_by(Date) %>%
  summarise(dam_af = sum(dam_storage_acre_feet, na.rm = TRUE),
            .groups = "drop")

# Oct 1 carryover — nearest available date within first 5 days of October
extract_oct1 <- function(df_dam) {
  df_dam %>%
    mutate(water_year = if_else(month(Date) >= 10,
                                year(Date) + 1L, year(Date))) %>%
    group_by(water_year) %>%
    filter(month(Date) == 10, day(Date) <= 5) %>%
    slice_min(Date, n = 1) %>%
    ungroup() %>%
    select(Date, water_year, dam_af)
}

carryover_normal_df <- extract_oct1(dam_daily_full) %>%
  filter(water_year >= NORMAL_START, water_year <= NORMAL_END)
carryover_median    <- median(carryover_normal_df$dam_af, na.rm = TRUE)

carryover <- daily %>%
  mutate(water_year = if_else(month(Date) >= 10,
                              year(Date) + 1L, year(Date))) %>%
  group_by(water_year) %>%
  filter(month(Date) == 10, day(Date) <= 5) %>%
  slice_min(Date, n = 1) %>%
  ungroup() %>%
  select(Date, water_year, combined_af, dam_af) %>%
  mutate(
    above_median = dam_af >= carryover_median,
    pct_median   = round(100 * dam_af / carryover_median),
    dot_color    = if_else(above_median, col_cool, col_warm)
  )

# ── NCEI monthly anomalies
monthly_normals <- ncei_raw %>%
  filter(year >= NORMAL_START, year <= NORMAL_END) %>%
  group_by(variable, month) %>%
  summarise(normal = mean(value, na.rm = TRUE), .groups = "drop")

monthly_anom <- ncei_raw %>%
  left_join(monthly_normals, by = c("variable", "month")) %>%
  mutate(
    anomaly  = value - normal,
    bar_date = as.Date(ISOdate(year, month, 15))
  ) %>%
  filter(bar_date >= PLOT_START) %>%
  arrange(variable, bar_date)

temp_anom   <- filter(monthly_anom, variable == "tavg")
precip_anom <- filter(monthly_anom, variable == "pcp")

# ── Data freshness labels (used in UI footer)
data_freshness <- list(
  swe_through = format(max(swe_raw$Date, na.rm = TRUE), "%b %d, %Y"),
  dam_through = format(max(dam_csv$Date,  na.rm = TRUE), "%b %d, %Y"),
  ncei_through = paste0(
    month.abb[max(ncei_raw$month[ncei_raw$variable == "tavg"])], " ",
    max(ncei_raw$year[ncei_raw$variable == "tavg"])
  )
)

message(sprintf(
  "global.R: ready — SWE through %s | Dam through %s | %d water years",
  data_freshness$swe_through,
  data_freshness$dam_through,
  length(all_wy)
))
