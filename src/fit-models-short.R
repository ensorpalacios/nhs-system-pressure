#' Fit models
#'
#' Fit different models, including baseline (mean, naive, snaive),
#' ARIMA, exponential smoothing and random forests; use different 
#' exogenous predictors such as days of the week, escalation beds, discharges 
#' and admissions.

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ts_occ <- readRDS(file = data_path)
ts_occ <- # drop original data with missing values
  ts_occ %>% select(-(ts_occ %>% names %>% grep("_m", .)))
sites <- ts_occ$site |> unique()


# Lag/split dataset ------------------------------------------------------------
ts_occ_lag <- lag_fun(ts_occ) # lag data

split_data_tt <- # Train/test set
  split_tt(ts_occ_lag)

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

list_var_ese <- c("bed_occ", "bed_escal")

fit_ese <- # fit ese (exponential smoothing with bed escalation)
  cv_wrap(split_data_cv, select_training,  es_model, list_var_ese)

# Random forest
list_var_rf <-
  split_data_cv %>% 
  select(-c(split, type, site, index, t_ax, days_, bed_occ_z)) %>% 
  names()

rf_reg <- 
  function(.data_rf, .lag = 1, .horizon = 14) {
    # Fit random forests from lagged variables and generate forecasts. Use
    # predictors of increasing lag to predict bed occupancy with increasing time
    # horizon .h. This is equivalent to last observation carried forward, where
    # which observation is used depends on .lag: .lag = 1 equivalent to last 
    # day; if .lag > 1 use several days.
    
    # Train set
    data_train = 
      .data_rf %>% filter(type == "train")
    y_train = 
      data_train %>% select(bed_occ)
    xl_train = # lagged data
      data_train %>%  select(-contains("bed_occ_z") & contains("lag"))
    xd_train = # days
      data_train %>% select(starts_with("days_"))
    
    # Test set
    data_test = 
      .data_rf %>% filter(type == "test")
    y_test = 
      data_test %>% select(bed_occ)
    xl_test = # lagged data 
      data_test %>%  select(-contains("bed_occ_z") & contains("lag"))
    xd_test = # days
      data_test %>% select(starts_with("days_"))
    
    # Loop over horizons
    rf_save = 
      map(seq(.horizon), \(.h) {
      # Select data
      tmp_lag = str_glue("lag{.h}")
      tmp_train = 
        bind_cols(y_train, xl_train %>% select(ends_with(tmp_lag)), xd_train)
      tmp_test = 
        bind_cols(y_test, xl_test %>% select(ends_with(tmp_lag)), xd_test) %>% 
        slice(.h) # predict only the .h day ahead from last observed .lag days
      
      # Compute
      tmp_fit = randomForest(bed_occ ~ ., data = tmp_train, ntree = 1000)
      tmp_fc = predict(tmp_fit,  tmp_test, predict.all = TRUE)

      # Forecast parameters
      tmp_par = 
        list(
          "mean" = tmp_fc$aggregate, 
          "sd" = tmp_fit$mse %>% sqrt() %>% mean() # from oob errors
        )
      
      # Return as list
      tmp_ls = list("fit" = tmp_fit, "fc" = tmp_fc, "par" = tmp_par)
    }) %>% 
      set_names(seq(.horizon))
  }

ls_rf <- # fits + fc + parameters fc distribution
  cv_wrap(split_data_cv, select_training, rf_reg, list_var_rf, "all")

fit_rf <-  # extract fits
  ls_rf %>% 
  map(., ~ # site
        map(.x, ~ # split
              map(.x, ~ # horizon
                    pluck(.x, "fit"))))
ls_par <- # extract parameters fc distribution
  ls_rf %>% 
  map(., ~ # site
        map(.x, ~ # split
              map(.x, ~ # horizon
                    pluck(.x, "par"))))

# Save all fits in list
fit_all = 
  list(
    "fable" = fit_fable,
    "es_e" = fit_ese,
    "rf" = fit_rf
    )


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
      .data$index %>% filter(type =="test")
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

# Random forest
fc_forecast =
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = .data$model %>% map_dfr(~ as.data.frame(.x)) %>% as_tibble()
    
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = "rf",
        .mean = mean, # necessary for fable::autoplot
        bed_occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, bed_occ, .mean), .after = index)
  }

fc_rf <- 
  cv_wrap(ls_par, select_model, fc_forecast)

fc_rf <- # convert to tsibble
  fc_rf %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))


# Join fc
dimnames(fc_ese$bed_occ) <- "bed_occ" # add rowname to match fc_fable
dimnames(fc_rf$bed_occ) <- "bed_occ"
fc_all <- list(fc_fable, fc_ese, fc_rf) %>% reduce(bind_rows)


# Save fits and forecasts ------------------------------------------------------
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


saveRDS(split_data_cv, file = paste0(save_path, "splits_short.RDS"))
saveRDS(fit_all, file = paste0(save_path, "fits_short.RDS"))
saveRDS(fc_all, file = paste0(save_path, "forecasts_short.RDS"))