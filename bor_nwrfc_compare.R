# bor_nwrfc_compare.R  (v3 - final working version)
#
# KEY FINDINGS FROM DATA EXPLORATION:
#   - BOR daily.pl returns only ONE reservoir per request (not all together)
#   - NWRFC data is in acre-feet/month; BOR is in mean daily cfs
#   - Must fetch BOR 5x (one per reservoir) and convert units before joining
#
# UNIT CONVERSION:
#   acre-feet = mean_cfs × n_days × 86400 sec/day ÷ 43560 ft³/acre-ft
#   Simplified coefficient: cfs × days × 1.98347

library(tidyverse)
library(httr2)
library(lubridate)

# ── Station crosswalk ──────────────────────────────────────────────────────────
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

# ── Parameters ─────────────────────────────────────────────────────────────────
start_date <- as.Date("2024-10-01")
end_date   <- as.Date("2026-05-31")

CFS_TO_AF_PER_DAY <- 86400 / 43560   # = 1.98347

# ── Step 1: Fetch BOR daily data, one reservoir at a time ─────────────────────
# We learned that the endpoint returns only the requested station,
# so we loop through all five and bind the results together.

fetch_bor_one <- function(reservoir_code, start_date, end_date) {
  
  url <- paste0(
    "https://www.usbr.gov/pn-bin/daily.pl",
    "?format=csv",
    "&flags=false",
    "&description=false",
    "&station=", reservoir_code,
    "&year=",  year(start_date),
    "&month=", month(start_date),
    "&day=",   day(start_date),
    "&year=",  year(end_date),
    "&month=", month(end_date),
    "&day=",   day(end_date),
    "&pcode=QD&pcode=QU"
  )
  
  message("Fetching BOR: ", reservoir_code)
  
  resp <- request(url) |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  raw_text <- resp_body_string(resp)
  
  df <- read_csv(I(raw_text), show_col_types = FALSE, comment = "#") |>
    rename_with(tolower)
  
  # The columns come back as e.g. "kee_qd", "kee_qu"
  # Pivot to long format right here, adding reservoir identity
  df_long <- df |>
    pivot_longer(
      cols      = -datetime,
      names_to  = c("res_check", "pcode"),
      names_sep = "_",
      values_to = "value_cfs"
    ) |>
    mutate(
      reservoir = toupper(reservoir_code),  # use the code we requested
      pcode     = toupper(pcode),
      date      = as.Date(datetime)
    ) |>
    select(date, reservoir, pcode, value_cfs) |>
    filter(!is.na(value_cfs))
  
  return(df_long)
}

# Loop through all 5 reservoirs and stack into one long data frame
bor_daily_long <- map_dfr(
  nwrfc_stations$reservoir,            # "KEE", "KAC", "CLE", "BUM", "RIM"
  ~ fetch_bor_one(.x, start_date, end_date)
)

# Quick check — should be ~5 reservoirs × 2 pcodes × ~608 days ≈ 6,000 rows
glimpse(bor_daily_long)
count(bor_daily_long, reservoir, pcode)   # verify all 5 reservoirs present

# ── Step 2: Aggregate BOR daily cfs → monthly acre-feet ───────────────────────
# This puts BOR on the same footing as NWRFC's volume_af

bor_monthly <- bor_daily_long |>
  mutate(
    year  = year(date),
    month = month(date)
  ) |>
  group_by(reservoir, pcode, year, month) |>
  summarise(
    mean_cfs   = mean(value_cfs, na.rm = TRUE),
    n_days     = n(),
    # Convert mean daily cfs → total monthly acre-feet
    volume_af  = mean_cfs * n_days * CFS_TO_AF_PER_DAY,
    .groups    = "drop"
  ) |>
  mutate(
    year_month = make_date(year, month, 1)
  )

glimpse(bor_monthly)
# Expected: 5 reservoirs × 2 pcodes × ~20 months = ~200 rows

# ── Step 3: Prepare NWRFC data for joining ─────────────────────────────────────
# Filter to individual reservoirs only (drop "ALL" system total)
# and to the overlap period with BOR data

nwrfc_compare <- nwrfc_raw |>
  filter(
    reservoir != "ALL",
    year  >= year(start_date),
    !(year == year(start_date) & month < month(start_date))
  ) |>
  mutate(
    year_month = make_date(year, month, 1)
  ) |>
  select(reservoir, year, month, year_month, nwrfc_af = volume_af)

glimpse(nwrfc_compare)
count(nwrfc_compare, reservoir)   # should show KEE, KAC, CLE, BUM, RIM

# ── Step 4: Pivot BOR wide (QD and QU as separate columns) then join ───────────
# This gives us one row per reservoir-month with columns for QD, QU, and NWRFC

bor_wide <- bor_monthly |>
  select(reservoir, year, month, year_month, pcode, volume_af) |>
  pivot_wider(
    names_from  = pcode,
    values_from = volume_af,
    names_prefix = "bor_"        # → bor_QD, bor_QU
  ) |>
  rename(
    bor_qd = bor_QD,
    bor_qu = bor_QU
  )

# Join BOR + NWRFC on reservoir + year + month
comparison <- bor_wide |>
  left_join(nwrfc_compare, by = c("reservoir", "year", "month", "year_month")) |>
  # Add reservoir labels from crosswalk
  left_join(
    nwrfc_stations |> select(reservoir, label),
    by = "reservoir"
  ) |>
  arrange(reservoir, year_month)

glimpse(comparison)

# ── Step 5: View the comparison table ─────────────────────────────────────────
# Columns: reservoir, year_month, bor_qd (AF), bor_qu (AF), nwrfc_af (AF)
# All values in acre-feet for direct comparison