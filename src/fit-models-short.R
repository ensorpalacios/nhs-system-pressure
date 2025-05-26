#' Fit models
#'
#' Fit different models, including baseline (mean, naive, snaive),
#' ARIMA and exponential smoothing; use different exogenous predictors such as
#' days of the week, escalation beds, discharges and admissions.

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
library(conflicted)
library(here)
library(data.table)
library(dplyr)
library(purrr)
library(fable)
library(smooth)
library(distributional)
source("src/split-data.R")
import::from(here, here)
import::from(stringr, str_sub)
import::from(tsibble, tsibble)

# Manage conflicts
conflicts_prefer(
  dplyr::filter,
  fabletools::accuracy, # used in computing metrics for fable fc
)


# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ts_occ <- readRDS(file = data_path)
ts_occ <- # drop original data with missing values
  ts_occ %>% select(-(ts_occ %>% names %>% grep("_m", .)))
sites <- ts_occ$site |> unique()


# Split dataset ----------------------------------------------------------------
split_data_tt <- # Train/test set
  split_tt(ts_occ)

initial <- "16 weeks" 
assess <- "2 weeks"
skip <- "8 weeks"
split_data_cv <- # Cv train/validation sets
  split_cv(split_data_tt, initial, assess, skip)

splits <- split_data_cv$split %>% unique() # save cv splits names


# Fit models -------------------------------------------------------------------
# Baseline and ARIMA models
fit_fable <- 
  split_data_cv %>% 
  filter(type == "train") %>% 
  tsibble(index = index, key = c(split, site)) %>% # (for fable models)
  model(
    # Baseline models (for comparison)
    mean = MEAN(bed_occ),
    naive = NAIVE(bed_occ),
    snaive = SNAIVE(bed_occ ~ lag("week")),
    # Arima models
    arima = ARIMA(bed_occ),
    arima_d = ARIMA(bed_occ ~ days_),
    arima_dad = ARIMA(bed_occ ~ days_ + adm + dis),
    arima_de = ARIMA(bed_occ ~ days_ + bed_escal),
    arima_dade = ARIMA(bed_occ ~ days_ + adm + dis + bed_escal),
  )

# Exponential smoothing with predictors (bed escalation)
es_model <- # define esx fit
  function(.data_es) {
    .data_es %>% as.ts() %>% es(model = "ZXZ",  lags = c(1, 1, 7))
  }

list_var <- c("bed_occ", "bed_escal")

fit_ese <- # fit ese (exponential smoothing with bed escalation)
  cv_wrap(split_data_cv, select_training,  es_model, list_var)

fit_all = # save all fits in list
  list(
    "fable" = fit_fable,
    "es_e" = fit_ese)


# Forecast ---------------------------------------------------------------------
# Baseline, ARIMA and exponential smoothing models
fc_fable <- 
  fit_fable %>% 
  forecast(new_data = 
             split_data_cv %>% 
             filter(type == "test") %>% 
             tsibble(index = index, key = c(split, site)))

# Exponential smoothing with predictors (esx)
select_model <- # select model fit (by site and split)
  function(.data, .site, .split, ...)  {
    # .data is list (site) of list (split) of model fits
    list(
      "model" = .data[[.site]][[.split]],
      "index" = split_data_cv %>% filter(site == .site, split == .split)
    )
  }

es_forecast <- # define esx forecast
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type=="test")
    tmp_fc = # compute forecasts
      forecast(.data$model, h = 14,
               interval = "prediction",
               level = .95,
               newdata = .data$index
      )
    tmp_fc = # add forecast distributions
      reduce(
        list(
          # .data$index %>% select(split, site, type, index),
          .data$index,
          tmp_fc$mean %>% as.data.frame() %>% set_names("mean"), 
          tmp_fc$lower %>% as.data.frame(), 
          tmp_fc$upper %>% as.data.frame()
        ),
        cbind
      ) %>% 
      mutate( # create forecast distribution (assuming nid error)
        .model = "es_e",
        bed_occ = dist_normal(mean, sd = (`Upper bound (97.5%)` - mean) / 2),
        .mean = mean, # necessary for fable::autoplot
        mean = NULL,
        `Lower bound (2.5%)` = NULL,
        `Upper bound (97.5%)` = NULL
      )
  }

fc_ese <- # forecast with esx model
  cv_wrap(fit_ese, select_model, es_forecast)

fc_ese <- # convert to tsibble
  fc_ese %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))

# Join fc
dimnames(fc_ese$bed_occ) <- "bed_occ" # add rowname to match fc_fable
fc_all <- fc_fable %>% bind_rows(fc_ese)


# Save fits and forecasts ------------------------------------------------------
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


saveRDS(split_data_cv, file = paste0(save_path, "splits_short.RDS"))
saveRDS(fit_all, file = paste0(save_path, "fits_short.RDS"))
saveRDS(fc_all, file = paste0(save_path, "forecasts_short.RDS"))