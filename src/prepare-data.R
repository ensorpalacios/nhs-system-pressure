#' Preprocess data
#'
#' Load, clean and recode data.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-07

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript

# import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(readxl)
library(tsibble)

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges - 2023
df1 <- readRDS(file = paste0(data_path, "dat.RDS"))

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
df2 <- read_excel(
                  paste0(
                         data_path,
                         "2024-09-01-to-2024-11-24-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)

# Compute bed occupancy from df1 ----------------------------------------------
# Set up time series to evaluate occupancy at
tseq <- seq.POSIXt(
  from = as.POSIXct("2023-01-01"),
  to = as.POSIXct("2023-12-13"), 
  by = "day"
)
# |>
#   trunc(units = "days")

# Use only BRI and Southmead hospitals
sites <- c("BRI" = "RA701", "Southmead" = "RVJ01")
bed_occ <- do.call("bind_rows", map(tseq, function(x) {
  map_int(sites, function(y) {
    df1 |>
      filter(site == y) |>
      filter(arr <= x & dep >= x) |>
      nrow()
  })
}))

# Prepare data from df2 -------------------------------------------------------
# Reorganise tibble
bed_occ2 <- df2 |> 
  filter(
    org_name == "Bristol Royal Infirmary" | 
      org_name == "North Bristol NHS Trust") |> 
  pivot_wider(names_from = metric_name, values_from = value) |>
  mutate(
    index = as.Date(date, tz = "GMT"),
    site = case_match(
      org_name, 
      "Bristol Royal Infirmary" ~ "BRI", 
      "North Bristol NHS Trust" ~ "Southmead"),
    bed_escal = `BNSSG Escalation beds`,
    bed_occ= `Bed Occupancy`,
    date = NULL,
    org_name = NULL,
    `BNSSG Escalation beds` = NULL,
    `Bed Occupancy` = NULL
  ) |>
  relocate(c(index, site))


# Data as timeseries (tsibble object) -----------------------------------------
# Convert tsibble object - weekly seasonality
sites <- sites |> names()
ts_occ <- data.frame(index = as.Date(tseq, tz = "GMT"), bed_occ) |> 
  pivot_longer(!index, names_to = "site", values_to = "bed_occ") |> 
  as_tsibble(index = index, key = site)

ts_occ2 <- bed_occ2 |> as_tsibble(index = index, key = site)

# Add z-scored value
ls_occ <- map(list(ts_occ, ts_occ2), \(x) {
  x |>
    group_by_key() |>
    mutate(
      bed_occ_z = bed_occ |> {\(x) (x - mean(x)) / sd(x)}()
    ) |>
    ungroup()
})

# Add day and time index
ls_occ <- map(ls_occ, \(x) {
  x |>  
    mutate(
      days_ = format(index, "%a") |> 
        factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      t_ax = rep(1:(length(days_)/2), 2)
    )
})

# Name datasets based on data provider
names(ls_occ) <- c("provider_level", "frontier")

# Rename admissions and discharges
ls_occ$frontier <- ls_occ$frontier |>
  rename(
    adm = Admissions,
    dis = Discharges
      )


# Save ts ---------------------------------------------------------------------
save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(ls_occ, file = paste0(save_path, 'bed_occupancy.RDS'))
