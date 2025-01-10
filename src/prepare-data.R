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
library(patchwork)
library(plotly)

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

# Compute bed occupancy -------------------------------------------------------
# Set up time series to evaluate occupancy at
tseq <- seq.POSIXt(
  from = as.POSIXct("2023-01-01"),
  to = as.POSIXct("2023-12-13"), 
  by = "day"
)

# Use only BRI and Southmead hospitals
# sites <- unique(df1_r[["site"]]) |> set_names() 
sites <- c("BRI" = "RA701", "Southmead" = "RVJ01")
bed_occ <- do.call("bind_rows", map(tseq, function(x) {
  map_int(sites, function(y) {
    df1 |>
      filter(site == y) |>
      filter(arr <= x & dep >= x) |>
      nrow()
  })
}))

# As df
bed_occ <- data.frame(dates = tseq, bed_occ)

# Save df ---------------------------------------------------------------------
save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(bed_occ, file = paste0(save_path, 'bed_occupancy.RDS'))
