#' Preprocess data
#'
#' Load, clean and recode data.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-07

# import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(readxl)
library(tsibble)
library(imputeTS)

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
# Urgent care data
df_occ <- read_excel(
                  paste0(
                         data_path,
                         "2022-01-01-to-2025-01-31-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)


# Prepare data ----------------------------------------------------------------
# Keep BRI & Southmead, generate index, rename variables
df_occ <- 
  df_occ |>
    filter(
      provider == "BRI" | provider == "NBT") |>
    select(-metric_id) |> 
    pivot_wider(names_from = metric_name, values_from = value) |>
    mutate(
      index = as.Date(report_date, tz = "GMT"),
      site = case_match(
        provider,
        "BRI" ~ "BRI", 
        "NBT" ~ "Southmead"),
      adm = `Number of Admissions`,
      dis = `Number of Discharges`,
      bed_escal = `Escalation beds open`,
      bed_occ = `Bed occupancy`,
      provider = NULL,
      `Number of Admissions` = NULL,
      `Number of Discharges` = NULL,
      `Escalation beds open` = NULL,
      `Bed occupancy` = NULL,
      report_date = NULL
    ) |>
    relocate(c(index, site)) |>
    arrange(index, site)

# Covert to timeseries (tsibble object)
ts_occ <- df_occ |> as_tsibble(index = index, key = site)

# Convert implicit gaps into explicit missing values
# Checked: only 21 missing data from Urgent Care Southmead
# + 2 NA already existing
ts_occ <- 
  ts_occ |>
    fill_gaps(.full = TRUE) # fully balanced data

# Impute missing values
ts_occ <- # impute
  ts_occ %>% 
  group_by(site) %>% 
  mutate(
    # Save data with missing values
    bed_occ_m = bed_occ,
    dis_m = dis,
    adm_m = adm,
    bed_escal_m = bed_escal,
    # Impute (simple moving average, window=7)
    bed_occ = bed_occ %>% na_ma(k = 3, weighting = "simple"),
    dis = dis %>% na_ma(k = 3, weighting = "simple"),
    adm = adm %>% na_ma(k = 3, weighting = "simple"),
    # Impute (Kalman smoothing)
    bed_escal = bed_escal %>% na_kalman()
  ) %>% 
  ungroup()


# Add z-scored value
ts_occ <- 
  ts_occ |>
    group_by_key() |>
    mutate(
      bed_occ_z = bed_occ |> 
        {\(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}()
    ) |>
    ungroup()

# Add days of week and time index
ts_occ <- 
  ts_occ |>  
    mutate(
      days_ = format(index, "%a") |> 
        factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      t_ax = rep(1:(length(days_)/2), 2)
    )


# Save ts ---------------------------------------------------------------------
save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(ts_occ, file = paste0(save_path, 'bed_occupancy.RDS'))