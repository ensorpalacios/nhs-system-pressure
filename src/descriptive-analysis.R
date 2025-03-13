#!/usr/bin/env Rscript

#' Descriptive analysis
#'
#' Descriptive analysis of bed occupancy time series, useful as preliminary 
#' step for time-series analysis, in particular for building an ARIMA model.
#' Steps involve:
#' - plot data
#' - evaluate possible data transformations
#' - evaluate existing trends (e.g., using differentiation)
#' - compute ACF and PACF (partial/autocorrelation function)
#' Objective of these preliminary steps it to have an idea of the 
#' order of the arima model (p, d, q)
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-10

# Import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(fable)
library(feasts)
library(tsibble)
library(slider)
library(lm)

# Load occupancy data ---------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ls_occ <- readRDS(file = data_path)
sites <- ls_occ[[1]]$site |> unique()


# Helper function -------------------------------------------------------------
# Replace NA with mean (only 22 urgent_care Southmead items):
# useful for some functions
na2mean <- function(bed_occ_data) {
  bed_occ_data |>
    group_by_key() |>
    mutate(
      bed_occ =
      if_else(
        is.na(bed_occ),
        mean(bed_occ, na.rm = TRUE),
        bed_occ
      )
    ) |>
    ungroup()
}


# Feature of time series ------------------------------------------------------
ts_features <- map(ls_occ, \(ts_occ) {
  ts_occ |> features(
    bed_occ, 
    features = list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      quantile = ~ quantile(., na.rm = TRUE),
      feature_set(pkgs = "feasts")
    )
  )
})

# STL decomposition -----------------------------------------------------------
stl_occ <- map(ls_occ, \(ts_occ) {
  # replace NA with mean
  ts_occ = na2mean(ts_occ)

  # STL
  ts_occ |> 
    model(stl = STL(bed_occ)) |>
    components() |>
    select(!.model) |>
    mutate(
      trend_adjust = bed_occ - trend
    )
})

# Decompose trend -------------------------------------------------------------
# Weekly  moving average
trend_ma <- map(ls_occ, \(ts_occ) {
  ts_occ |> 
    group_by_key() |>
    mutate(
      `7-ma` = slide_dbl(
        bed_occ, 
        mean, 
        .before = 3, 
        .after = 3, 
        .complete = TRUE),
      residuals = bed_occ - `7-ma`
    ) |>
    ungroup()
})

# Lowess
trend_lowess <- map(ls_occ, \(ts_occ) {
  ts_occ |> 
    group_by_key() |>  
    mutate(
      lowess = lowess(bed_occ, f = 0.175)$y,
      residuals = bed_occ - lowess
    ) |>
    ungroup()
})

# Polynomial (2nd order)
trend_poly <- map(ls_occ, \(ts_occ) {
  # Replace NA with mean
  ts_occ = na2mean(ts_occ)

  # Linear regression
  tmp_poly <- ts_occ |>  
    group_by_key() %>%
    do(model = lm(data = ., bed_occ~ t_ax + I(t_ax^2), na.action = na.exclude))

  # Extract fitted values and residuals
  # Note: sites is a var defined globally
  tmp_fit <- do.call(cbind, map(sites, \(x) {
    tmp_polyfit = tmp_poly[sites == x, "model", drop = TRUE][[1]]$fitted.values
  })) 
  tmp_res <- do.call(cbind, map(sites, \(x) {
    tmp_polyres = tmp_poly[sites == x, "model", drop = TRUE][[1]]$residuals
  })) 
  tmp_poly <- ts_occ
  tmp_poly["fit"] <- tmp_fit |> as.vector()
  tmp_poly["res"] <- tmp_res |> as.vector()
  tmp_poly
})


# Decompose seasonality -------------------------------------------------------
# Weekly linear regression
season_week <- map(ls_occ, \(ts_occ) {
  # Replace NA with mean
  ts_occ = na2mean(ts_occ)

  # Linear regression
  tmp_week <- ts_occ |>  
    group_by_key() %>%
    do(model = lm(data = ., bed_occ ~ days_))
  # do(model = lm(data = ., bed_occ ~ t_ax + I(t_ax^2) + days_))

  # Extract fitted values and residuals
  # Note: sites is a var defined globally
  tmp_fit <- do.call(cbind, map(sites, \(x) {
    tmp_weekfit = tmp_week[sites == x, "model", drop = TRUE][[1]]$fitted.values
  })) 
  tmp_res <- do.call(cbind, map(sites, \(x) {
    tmp_weekres = tmp_week[sites == x, "model", drop = TRUE][[1]]$residuals
  })) 
  tmp_week <- ts_occ
  tmp_week["fit"] <- tmp_fit |> as.vector()
  tmp_week["res"] <- tmp_res |> as.vector()
  tmp_week
})


# Differencing ----------------------------------------------------------------
# Trend (1st order)
trend_diff <- map(ls_occ, \(ts_occ) {
  ts_occ |>
    group_by_key() |> 
    mutate(
      diff= difference(bed_occ),
    ) |> 
    ungroup()
})

# Season (lag = week)
season_diff <- map(ls_occ, \(ts_occ) {
  ts_occ |>
    group_by_key() |> 
    mutate(
      diff = difference(bed_occ, lag = 7)
    ) |>
    ungroup()
})

# Seasonal + 1st order differencing
second_diff <- map(ls_occ, \(ts_occ) {
  ts_occ |>
    group_by_key() |> 
    mutate(
      diff = difference(difference(bed_occ, lag = 7))
    ) |> 
    ungroup()
})


# Save descriptive analysis for plot -----------------------------------------_
des <- list(
  "sites" = sites,
  "ts_occ" = ls_occ,
  "ts_features" = ts_features,
  "stl_occ" = stl_occ, 
  "trend_ma" = trend_ma, 
  "trend_lowess" = trend_lowess, 
  "trend_poly" = trend_poly, 
  "trend_diff" = trend_diff, 
  "season_week" = season_week, 
  "season_diff" = season_diff,
  "second_diff" = second_diff
)

save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(des, file = paste0(save_path, 'descriptive_analysis.RDS'))
