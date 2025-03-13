#!/usr/bin/env Rscript

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

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/raw/")

# Admissions/discharges - 2023
# Provider-level data
df1 <- readRDS(file = paste0(data_path, "dat.RDS"))

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
# Frontier data
df2 <- read_excel(
                  paste0(
                         data_path,
                         "2024-09-01-to-2024-11-24-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)

# Admissions/discharges, acute bed occupancy, escalation beds - 2024
# Urgent care data
df3 <- read_excel(
                  paste0(
                         data_path,
                         "2022-01-01-to-2025-01-31-acute-occupancy.xlsx"
                         ),
                  sheet = 2
)

# Compute bed occupancy from provider-level data ------------------------------
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

# Prepare Frontier data -------------------------------------------------------
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

# Prepare urgent care data ----------------------------------------------------
bed_occ3 <-
  df3 |>
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
      Admissions = `Number of Admissions`,
      Discharges = `Number of Discharges`,
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


# Data as timeseries (tsibble object) -----------------------------------------
# Convert tsibble object - weekly seasonality
sites <- sites |> names()
ts_occ <- data.frame(index = as.Date(tseq, tz = "GMT"), bed_occ) |> 
  pivot_longer(!index, names_to = "site", values_to = "bed_occ") |> 
  as_tsibble(index = index, key = site)

ts_occ2 <- bed_occ2 |> as_tsibble(index = index, key = site)
ts_occ3 <- bed_occ3 |> as_tsibble(index = index, key = site)

# Convert implicit gaps into explicit missing values ----------------------------
# Checked: only 21 missing data from Urgent Care Southmead
# + 2 NA already existing
ts_occ3 <- 
  ts_occ3 |>
    fill_gaps(.full = TRUE) # fully balanced data

# Add z-scored value
ls_occ <- map(list(ts_occ, ts_occ2, ts_occ3), \(x) {
  x |>
    group_by_key() |>
    mutate(
      bed_occ_z = bed_occ |> 
        {\(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}()
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
names(ls_occ) <- c("provider_level", "frontier", "urgent_care")

# Rename admissions and discharges
ls_occ$frontier <- ls_occ$frontier |>
  rename(
    adm = Admissions,
    dis = Discharges
      )
ls_occ$urgent_care <- ls_occ$urgent_care |>
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
