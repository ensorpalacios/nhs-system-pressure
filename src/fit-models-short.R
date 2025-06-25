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
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
sites <- ts_occ$site |> unique()



# Select relevant variables ----------------------------------------------------
ts_occ <- 
  ts_occ %>%
  select(
    -(ts_occ %>% names %>% grep("_m", .)), # original data with missing values
    -adm, -dis,
    -escal, -core,
    # -escal, -core, -occ_i,
    # -ad_diff, -ad_diff2, -ad_diff3
    )



# Lag/split dataset ------------------------------------------------------------
horizon = 7
ts_occ_lag <- lag_fun(ts_occ, .lag = horizon) # lag data

split_data_tt <- # Train/test set
  split_tt(ts_occ_lag)

initial <- "16 weeks" 
assess <- "1 weeks"
skip <- "6 weeks"
split_data_cv <- # Cv train/validation sets
  split_cv(split_data_tt, initial, assess, skip)

splits <- split_data_cv$split %>% unique() # save cv splits names
idx_start_test <- split_data_cv$type %>% grep("test", .) %>% head(1)



# Predict test exogenous -------------------------------------------------------
# Last observation carried forward
data_locf <- 
  locf_fun(split_data_cv, c("occ", "ad_diff"), idx_start_test, "locf")



# Fit models -------------------------------------------------------------------
# Baseline and ARIMA models
fit_fable <- 
  split_data_cv %>% 
  filter(type == "train" & !is_aggregated(site)) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    # Baseline models (for comparison)
    mean = MEAN(occ),
    naive = NAIVE(occ),
    snaive = SNAIVE(occ ~ lag("week")),
    # Arima models
    arima = ARIMA(occ),
    arima_dad = ARIMA(occ ~ days_ + ad_diff_f + ad_diff2_f + ad_diff3_f),
    arima_dad_nof = ARIMA(occ ~ days_ + ad_diff + ad_diff2 + ad_diff3),
    arima_dad_l = 
      ARIMA(
        occ ~ 
          days_ +
          
          ad_diff_f + ad_diff2_f + ad_diff3_f +
          ad_diff_f_lag1 + ad_diff2_f_lag1 + ad_diff3_f_lag1 +
          ad_diff_f_lag2 + ad_diff2_f_lag2 + ad_diff3_f_lag2 +
          ad_diff_f_lag3 + ad_diff2_f_lag3 + ad_diff3_f_lag3 +
          ad_diff_f_lag4 + ad_diff2_f_lag4 + ad_diff3_f_lag4 +
          ad_diff_f_lag5 + ad_diff2_f_lag5 + ad_diff3_f_lag5 +
          ad_diff_f_lag6 + ad_diff2_f_lag6 + ad_diff3_f_lag6 +
          ad_diff_f_lag7 + ad_diff2_f_lag7 + ad_diff3_f_lag7
      ),
    arima_dado = 
      ARIMA(
        occ ~ days_ + ad_diff_f + ad_diff2_f + ad_diff3_f + occ_other
      ), 
    arima_dado_l = 
      ARIMA(
        occ ~ 
          days_ + 
          ad_diff_f + ad_diff2_f + ad_diff3_f + occ_other +
          ad_diff_f_lag1 + ad_diff2_f_lag1 + ad_diff3_f_lag1 + occ_other_lag1 +
          ad_diff_f_lag2 + ad_diff2_f_lag2 + ad_diff3_f_lag2 + occ_other_lag2 +
          ad_diff_f_lag3 + ad_diff2_f_lag3 + ad_diff3_f_lag3 + occ_other_lag3 +
          ad_diff_f_lag4 + ad_diff2_f_lag4 + ad_diff3_f_lag4 + occ_other_lag4 +
          ad_diff_f_lag5 + ad_diff2_f_lag5 + ad_diff3_f_lag5 + occ_other_lag5 +
          ad_diff_f_lag6 + ad_diff2_f_lag6 + ad_diff3_f_lag6 + occ_other_lag6 +
          ad_diff_f_lag7 + ad_diff2_f_lag7 + ad_diff3_f_lag7 + occ_other_lag7
      )
  )


# ARIMA aggregated (exclude occ_other!)
fit_fable_agg <- 
  split_data_cv %>% 
  filter(type == "train") %>%
  tsibble(index = index, key = c(split, site)) %>%
  model(
    arima_dad_agg = 
      ARIMA(
        occ ~ 
          days_ + 
          ad_diff_f + ad_diff2_f + ad_diff3_f
      ),
    arima_dad_l_nof_agg = 
      ARIMA(
        occ ~ 
          days_ + 
          ad_diff + ad_diff2 + ad_diff3 +
          ad_diff_lag1 + ad_diff2_lag1 + ad_diff3_lag1 +
          ad_diff_lag2 + ad_diff2_lag2 + ad_diff3_lag2 +
          ad_diff_lag3 + ad_diff2_lag3 + ad_diff3_lag3 +
          ad_diff_lag4 + ad_diff2_lag4 + ad_diff3_lag4 +
          ad_diff_lag5 + ad_diff2_lag5 + ad_diff3_lag5 +
          ad_diff_lag6 + ad_diff2_lag6 + ad_diff3_lag6 +
          ad_diff_lag7 + ad_diff2_lag7 + ad_diff3_lag7
      )
  )


# Vector autoregressive models
fit_fable_var_ad <- # adm - dis
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad = VAR(vars(occ, ad_diff_f) ~ season(period = "week"))
  )

fit_fable_var_ad_nof <- # adm - dis not filtered
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad_nof = VAR(vars(occ, ad_diff) ~ season(period = "week"))
  )

fit_fable_var_ad2 <- # diff(adm - dis)
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad2 = VAR(vars(occ, ad_diff2_f) ~ season(period = "week"))
  )

fit_fable_var_ad2_nof <- # diff(adm - dis) not filtered
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad2_nof = VAR(vars(occ, ad_diff2) ~ season(period = "week"))
  )


fit_fable_var_ad3 <- # diff(diff(adm - dis)
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad3 = VAR(vars(occ, ad_diff3_f) ~ season(period = "week"))
  )

fit_fable_var_ad3_nof <- # diff(diff(adm - dis) not filtered
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad3_nof = VAR(vars(occ, ad_diff3) ~ season(period = "week"))
  )


fit_fable_var_other <- # BRI vs Southmead
  split_data_cv %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  select(occ) %>% 
  pivot_wider(names_from = site, values_from = occ) %>% 
  model(
    var_other = VAR(vars(BRI, Southmead) ~ season(period = "week"))
  )


# Exponential smoothing with predictors (bed escalation)
es_model <- # define esx fit
  function(.data_es) {
    .data_es %>% as.ts() %>% 
      es(model = "ZXZ",  lags = c(1, 1, 7))
  }

list_var_ese <- 
  c("occ", 
    "ad_diff_f", "ad_diff2_f", "ad_diff3_f",
    "occ_other")
# list_var_ese <-
#   split_data_cv %>%
#   select(
#     contains("occ"), -occ_other,
#     matches("ad_diff.*_f") , -ad_diff_f, -ad_diff2_f, -ad_diff3_f,
#     contains("days_"), -days_) %>%
#   names()

fit_es <- # fit es
  cv_wrap(
    split_data_cv %>% filter(type == "train" & !is_aggregated(site)),
    select_training,  
    es_model, 
    list_var_ese)


# Random forest
list_var_rf <-
  split_data_cv %>%
  select(
    contains("occ"), -occ_other,
    matches("ad_diff.*_f"), -ad_diff_f, -ad_diff2_f, -ad_diff3_f,
    contains("days_"), -days_) %>%
  names()

rf_reg <- 
  function(.data_rf, .lag = 1, .horizon = horizon) {
    # Fit random forests from lagged variables and generate forecasts. Use
    # predictors of increasing lag to predict bed occupancy with increasing time
    # horizon .h.
    
    # Train set
    data_train = 
      .data_rf %>% filter(type == "train")
    y_train = 
      data_train %>% select(occ)
    xl_train = # lagged data
      data_train %>%  select(contains("lag"))
    xd_train = # days
      data_train %>% select(starts_with("days_"))
    
    # Test set
    data_test = 
      .data_rf %>% filter(type == "test")
    y_test = 
      data_test %>% select(occ)
    xl_test = # lagged data 
      data_test %>%  select(contains("lag"))
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
      tmp_fit = randomForest(occ ~ ., data = tmp_train, ntree = 1000)
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
  cv_wrap(
    split_data_cv %>% filter(!is_aggregated(site)), 
    select_training,  rf_reg,  list_var_rf, "all"
    )

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
    "fable_agg" = fit_fable_agg,
    "fable_var_ad" = fit_fable_var_ad,
    "fable_var_ad_nof" = fit_fable_var_ad,
    "fable_var_ad2" = fit_fable_var_ad2,
    "fable_var_ad2_nof" = fit_fable_var_ad2_nof,
    "fable_var_ad3" = fit_fable_var_ad3,
    "fable_var_ad3_nof" = fit_fable_var_ad3_nof,
    "fable_var_other" = fit_fable_var_other,
    "es_ae_f" = fit_es,
    # "rf_dae_f" = fit_rf,
    "rf_dae_f_par" = ls_par
    )
# fit_fable = fit_all$fable
# fit_fable_agg = fit_all$fable_agg
# fit_fable_agg = fit_all$fable_agg
# fit_es = fit_all$es_ae_f
# ls_par = fit_all$rf_dae_f_par



# Forecast ---------------------------------------------------------------------
# Baseline, ARIMA and exponential smoothing models
fc_fable <- 
  fit_fable %>% 
  forecast(new_data = 
             split_data_cv %>% 
             filter(type == "test") %>% 
             tsibble(index = index, key = c(split, site)))

# ARIMA with predicted exogenous
fc_fable_locf <- 
  fit_fable %>% 
  select(contains("arima_da")) %>% 
  forecast(
    new_data = 
      data_locf %>% 
      filter(type == "test") %>% 
      tsibble(index = index, key = c(split, site))
  ) %>% 
  mutate( # change name locf models
    .model = paste0("locf_",.model)
  )

# ARIMA reconciled
fc_fable_locf_rec <- 
  fit_fable_agg %>% 
  reconcile(
    arima_dad_rec = min_trace(arima_dad_agg, method = "mint_cov"),
    arima_dad_l_nof_rec = min_trace(arima_dad_l_nof_agg, method = "mint_cov")
    ) %>% 
  select(-arima_dad_agg, -arima_dad_l_nof_agg) %>%
  forecast(
    new_data = data_locf %>% 
      filter(type == "test") %>% 
      tsibble(index = index, key = c(split, site))
  ) %>% 
  mutate( # change name locf models
    .model = paste0("locf_",.model)
  )

# Vector autoregressive models
fc_fable_var_ad <- 
  fit_fable_var_ad %>% 
  forecast(h = horizon)

fc_fable_var_ad_nof <- 
  fit_fable_var_ad_nof %>% 
  forecast(h = horizon)

fc_fable_var_ad2 <- 
  fit_fable_var_ad2 %>% 
  forecast(h = horizon)

fc_fable_var_ad2_nof <- 
  fit_fable_var_ad2_nof %>% 
  forecast(h = horizon)

fc_fable_var_ad3 <- 
  fit_fable_var_ad3 %>% 
  forecast(h = horizon)

fc_fable_var_ad3_nof <- 
  fit_fable_var_ad3_nof %>% 
  forecast(h = horizon)

fc_fable_var_other <- 
  fit_fable_var_other %>% 
  forecast(h = horizon)


fc_var <- # bind VAR fc
  map(
    list(
      fc_fable_var_ad, fc_fable_var_ad_nof,
      fc_fable_var_ad2, fc_fable_var_ad2_nof,
      fc_fable_var_ad3, fc_fable_var_ad3_nof
      ),
    \(.x) {
      .x %>% 
        as_tibble() %>% 
        mutate(
          occ = 
            dist_normal(
              mean = .distribution %>% mean() %>% .[, "occ"],
              sigma = .distribution %>% variance() %>% sqrt() %>% .[, "occ"]
            ),
          .mean = occ %>% mean(),
          # .mean = NULL,
          .distribution = NULL
        ) %>% 
        as_tsibble(index = index, key = c(split, site, .model))
    }
  ) %>% 
  bind_rows() %>% 
  bind_rows(
    imap(
      fc_fable_var_other %>% pluck(".distribution") %>%  dimnames(),
      \(.x, .y) {
        fc_fable_var_other %>% 
          as_tibble() %>% 
          mutate(
            occ = 
              dist_normal(
                mean = .distribution %>% mean() %>% .[, .x],
                sigma = .distribution %>% variance() %>% sqrt() %>% .[, .x]
              ),
            site = .x,
            .model = paste0("var_", .x),
            .mean = .mean[.y],
            .distribution = NULL
          ) %>% 
          as_tsibble(index = index, key = c(split, site, .model))
      }
    ) %>% 
      bind_rows()
  )

# Exponential smoothing with predictors (esx)
select_model <- # select model fit (by site and split)
  function(.data, .site, .split, ...)  {
    # .data is list (site) of list (split) of model fits
    list(
      "model" = .data[[.site]][[.split]],
      "index" = data_locf %>% filter(site == .site, split == .split)
    )
  }

es_forecast <- # define esx forecast
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    tmp_fc = # compute forecasts
      forecast(.data$model, h = horizon,
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
        .model = "locf_es_ado_f",
        occ = dist_normal(mean, sd = (`Upper bound (97.5%)` - mean) / 2),
        .mean = mean, # necessary for fable::autoplot
        mean = NULL,
        `Lower bound (2.5%)` = NULL,
        `Upper bound (97.5%)` = NULL
      )
  }

fc_ese <- # forecast with esx model
  cv_wrap(fit_es, select_model, es_forecast)

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
        .model = "rf_dado_f",
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }

fc_rf <- 
  cv_wrap(ls_par, select_model, fc_forecast)

fc_rf <- # convert to tsibble
  fc_rf %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))


# Join fc
dimnames(fc_var$occ) <- "occ" # add name to column to match fc_fable
dimnames(fc_ese$occ) <- "occ" 
dimnames(fc_rf$occ) <- "occ"
fc_all <- 
  list(
    fc_fable, fc_fable_locf, fc_fable_locf_rec, fc_var , fc_ese, fc_rf) %>% 
  reduce(bind_rows)



# Save fits and forecasts ------------------------------------------------------
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


saveRDS(split_data_cv, file = paste0(save_path, "splits_short.RDS"))
saveRDS(fit_all, file = paste0(save_path, "fits_short.RDS"))
saveRDS(fc_all, file = paste0(save_path, "forecasts_short.RDS"))
# fit_all <- readRDS(paste0(save_path, "fits_short.RDS"))
