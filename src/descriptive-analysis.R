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
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-01-10

# Load occupancy data ---------------------------------------------------------
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
sites <- ts_occ$site |> unique()


# Feature of time series ------------------------------------------------------
ts_features <- 
  ts_occ |> 
  features(
    bed_occ, 
    features = list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      quantile = ~ quantile(., na.rm = TRUE),
      feature_set(pkgs = "feasts")
    )
  )

# STL decomposition -----------------------------------------------------------
stl_occ <- 
  ts_occ |> 
  model(stl = STL(bed_occ)) |>
  components() |>
  select(!.model) |>
  mutate(
    trend_adjust = bed_occ - trend
  )

# Decompose trend -------------------------------------------------------------
# Weekly  moving average
trend_ma <-
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

# Lowess
trend_lowess <-
  ts_occ |> 
  group_by_key() |>  
  mutate(
    lowess = lowess(bed_occ, f = 0.175)$y,
    residuals = bed_occ - lowess
  ) |>
  ungroup()


# Decompose seasonality -------------------------------------------------------
# Weekly linear regression
fitweek <- function(ts_data) {
  # Fit model
  tmp_week <- ts_data |>  
    group_by_key() %>%
    do(model = lm(data = ., bed_occ ~ days_))
  
  # Extract fitted values and residuals
  # site is global var
  tmp_fit <- do.call(cbind, map(sites, \(x) { 
    tmp_weekfit = tmp_week[sites == x, "model", drop = TRUE][[1]]$fitted.values
  })) 
  tmp_res <- do.call(cbind, map(sites, \(x) {
    tmp_weekres = tmp_week[sites == x, "model", drop = TRUE][[1]]$residuals
  })) 
  tmp_week <- ts_data
  tmp_week["fit"] <- tmp_fit |> as.vector()
  tmp_week["res"] <- tmp_res |> as.vector()
  tmp_week
}

season_week <- fitweek(ts_occ)


# Differencing ----------------------------------------------------------------
# Trend (1st order)
trend_diff <- 
  ts_occ |>
  group_by_key() |> 
  mutate(
    diff= difference(bed_occ),
  ) |> 
  ungroup()

# Season (lag = week)
season_diff <-
  ts_occ |>
  group_by_key() |> 
  mutate(
    diff = difference(bed_occ, lag = 7)
  ) |>
  ungroup()

# Seasonal + 1st order differencing
second_diff <-
  ts_occ |>
  group_by_key() |> 
  mutate(
    diff = difference(difference(bed_occ, lag = 7))
  ) |> 
  ungroup()


# Save descriptive analysis for plot ------------------------------------------
descriptive <- 
  list(
    "ts_occ" = ts_occ,
    "sites" = sites,
    "ts_features" = ts_features,
    "stl_occ" = stl_occ, 
    "trend_ma" = trend_ma, 
    "trend_lowess" = trend_lowess, 
    "trend_diff" = trend_diff, 
    "season_week" = season_week, 
    "season_diff" = season_diff,
    "second_diff" = second_diff
  )

save_path <- here("data/processed/")
if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

saveRDS(descriptive, file = paste0(save_path, 'descriptive_analysis.RDS'))