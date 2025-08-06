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



# Block Bootstrap --------------------------------------------------------------
# Use ts_occ_b for getting lagged and splitted bootstrapped training data (no need of xreg though as using bootstrapped data only for training)
n_boot <- 20
block_size <- 14
ts_occ_b <-
  block_boot(
    ts_occ %>% filter(!is_aggregated(site)),
    .n_boot = n_boot,
    .b_size = block_size
    )



# Lag/split dataset ------------------------------------------------------------
horizon = 7
ts_occ_lag <- lag_fun(ts_occ, .lag = horizon) # lag data
ts_occ_lag_b <- # on bootstrapped data
  map(ts_occ_b, \(x) lag_fun(x, .lag = horizon))


split_data_tt <- # Train/test set
  split_tt(ts_occ_lag)
split_data_tt_b <- # on bootstrapped data
  map(ts_occ_lag_b, \(x) split_tt(x))


initial <- "16 weeks" 
assess <- "1 weeks"
skip <- "6 weeks"
split_data_cv <- # Cv train/validation sets
  split_cv(split_data_tt, initial, assess, skip)
split_data_cv_b <- # on bootstrapped data
  map(split_data_tt_b, \(x) split_cv(x, initial, assess, skip))


splits <- split_data_cv$split %>% unique() # save cv splits names
idx_start_test <- split_data_cv$type %>% grep("test", .) %>% head(1)



# Predict test exogenous -------------------------------------------------------
# Exclude occ_other (possibly add too much noise)
xpredict_method = "pull" # mean, naive, snaive, arima, ets
data_xpredict <- 
  xpredict_fun(
    split_data_cv %>% select(-contains("occ_other")), 
    c("occ", "ad_diff"), 
    idx_start_test, 
    xpredict_method
    )



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
        occ ~ days_ + ad_diff_f + ad_diff2_f + ad_diff3_f# + occ_other
      ), 
    arima_dado_l = 
      ARIMA(
        occ ~ 
          days_ + 
          ad_diff_f + ad_diff2_f + ad_diff3_f +
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
      )#,
    # arima_dad_l_nof_agg = 
    #   ARIMA(
    #     occ ~ 
    #       days_ + 
    #       ad_diff + ad_diff2 + ad_diff3 +
    #       ad_diff_lag1 + ad_diff2_lag1 + ad_diff3_lag1 +
    #       ad_diff_lag2 + ad_diff2_lag2 + ad_diff3_lag2 +
    #       ad_diff_lag3 + ad_diff2_lag3 + ad_diff3_lag3 +
    #       ad_diff_lag4 + ad_diff2_lag4 + ad_diff3_lag4 +
    #       ad_diff_lag5 + ad_diff2_lag5 + ad_diff3_lag5 +
    #       ad_diff_lag6 + ad_diff2_lag6 + ad_diff3_lag6 +
    #       ad_diff_lag7 + ad_diff2_lag7 + ad_diff3_lag7
    #   )
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
    "ad_diff_f", "ad_diff2_f", "ad_diff3_f")#,
    # "occ_other")
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
# Attention: here excluding same-day predictors; plus using occ_other as no
# need to predict it
list_var_rf <-
  split_data_cv %>%
  select(
    contains("occ"), -occ_other,
    matches("ad_diff.*_f"), -ad_diff_f, -ad_diff2_f, -ad_diff3_f,
    contains("days_"), -days_) %>%
  names()

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

ls_rf_par <- # extract parameters fc distribution
  ls_rf %>% 
  map(., ~ # site
        map(.x, ~ # split
              map(.x, ~ # horizon
                    pluck(.x, "par"))))


# Random forest - interaction
# Attention: here excluding same-day predictors and occ_other
list_var_rf <- # exclude occ_other
  list_var_rf[!grepl("occ_other*.", list_var_rf)]

data_xpredict_int <- # long format data with lag column
  data_xpredict %>%
  select(split, type, site, index, all_of(list_var_rf)) %>%
  rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>% 
  rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>% 
  pivot_longer(
    cols = c(contains("lag")),
    names_to = c(".value", "lag"),
    names_sep = "-"
  )

list_var_rf_int <- # list predictors 
  data_xpredict_int %>% select(-c(split, type, site, index)) %>% names()


ls_rf_int <- # fits + fc + parameters fc distribution
  cv_wrap(
    data_xpredict_int %>% filter(!is_aggregated(site)), 
    select_training,  rf_reg_int,  list_var_rf_int, "all"
  )

fit_rf_int <-  # extract fits
  ls_rf_int %>% 
  map(., ~ # site
        map(.x, ~ # split
              pluck(.x, "fit")))

ls_rf_int_par <- # extract parameters fc distribution
  ls_rf_int %>% 
  map(., ~ # site
        map(.x, ~ # split
              pluck(.x, "par")))


# XGBoosting - interaction
# Join bootstrapped samples in one tibble; convert wide to long (for lag
# as for rf_int data); join real (data_xpredict_int) with bootstrapped data
split_data_cv_b <- # joint 
  split_data_cv_b %>% 
  imap(\(x, idx) {
    x %>% mutate(boot = idx)
  }) %>%
  list_rbind()

data_xpredict_int_b <- # long format data with lag column
  split_data_cv_b %>% 
  select(split, type, site, index, boot, all_of(list_var_rf)) %>%
  rename_with(~ sub("occ_lag", "occ_same_lag", .x)) %>% 
  rename_with(~ sub("_lag", "-lag", .x, fixed = TRUE)) %>% 
  pivot_longer(
    cols = c(contains("lag")),
    names_to = c(".value", "lag"),
    names_sep = "-"
  )

data_xpredict_int_b <- # join real/bootstrapped data
  data_xpredict_int %>% 
  mutate(boot = 0) %>% 
  bind_rows(data_xpredict_int_b)

list_var_xgb <- # list predictors + boot identifier
  data_xpredict_int_b %>% select(-c(split, type, site, index)) %>% names()

ls_xgb_par <- # fits + fc + parameters fc distribution
  cv_wrap(
    data_xpredict_int_b %>% filter(!is_aggregated(site)),
    select_training,  xgb_reg_int,  list_var_xgb, "all"
  )


# Save all fits in list
fit_all = 
  list(
    "fable" = fit_fable,
    "fable_agg" = fit_fable_agg,
    "fable_var_ad" = fit_fable_var_ad,
    "fable_var_ad_nof" = fit_fable_var_ad_nof,
    "fable_var_ad2" = fit_fable_var_ad2,
    "fable_var_ad2_nof" = fit_fable_var_ad2_nof,
    "fable_var_ad3" = fit_fable_var_ad3,
    "fable_var_ad3_nof" = fit_fable_var_ad3_nof,
    "fable_var_other" = fit_fable_var_other,
    "es" = fit_es,
    "rf_dae_f_par" = ls_rf_par,
    "rf_dae_f_int_par" = ls_rf_int_par,
    "xgb_par" = ls_xgb_par
    )
# fit_fable = fit_all$fable
# fit_fable_agg = fit_all$fable_agg
# fit_fable_var_ad = fit_all$fable_var_ad
# fit_fable_var_ad_nof = fit_all$fable_var_ad_nof
# fit_fable_var_ad2 = fit_all$fable_var_ad2
# fit_fable_var_ad2_nof = fit_all$fable_var_ad2_nof
# fit_fable_var_ad3 = fit_all$fable_var_ad3
# fit_fable_var_ad3_nof = fit_all$fable_var_ad3_nof
# fit_fable_var_other = fit_all$fable_var_other
# fit_es = fit_all$es_ae_f
# ls_par = fit_all$rf_dae_f_par
# ls_par_int = fit_all$rf_dae_f_int_par



# Forecast ---------------------------------------------------------------------
# Baseline, ARIMA and exponential smoothing models
fc_fable <- 
  fit_fable %>% 
  forecast(new_data = 
             split_data_cv %>% 
             filter(type == "test") %>% 
             tsibble(index = index, key = c(split, site)))

# ARIMA with predicted exogenous
fc_fable_xpred <- 
  fit_fable %>% 
  select(contains("arima_da")) %>% 
  forecast(
    new_data = 
      data_xpredict %>% 
      filter(type == "test") %>% 
      tsibble(index = index, key = c(split, site))
  ) %>% 
  mutate( # rename models
    .model = paste0("xpred_",.model)
  )

# ARIMA reconciled
fc_fable_xpred_rec <- 
  fit_fable_agg %>% 
  reconcile(
    arima_dad_rec = min_trace(arima_dad_agg, method = "mint_cov")#,
    # arima_dad_l_nof_rec = min_trace(arima_dad_l_nof_agg, method = "mint_cov")
    ) %>% 
  # select(-arima_dad_agg, -arima_dad_l_nof_agg) %>%
  select(-arima_dad_agg) %>%
  forecast(
    new_data = data_xpredict %>% 
      filter(type == "test") %>% 
      tsibble(index = index, key = c(split, site))
  ) %>% 
  mutate( # rename models
    .model = paste0("xpred_",.model)
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
# select_model <- # select model fit (by site and split)
#   function(.data, .site, .split, ...)  {
#     # .data is list (site) of list (split) of model fits
#     list(
#       "model" = .data[[.site]][[.split]],
#       "index" = data_xpredict %>% filter(site == .site, split == .split)
#     )
#   }

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
        .model = "xpred_es",
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
rf_forecast =
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% map_dfr(~ as.data.frame(.x)) %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = "xpred_rf",
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }

fc_rf <- 
  cv_wrap(ls_rf_par, select_model, rf_forecast)

fc_rf <- # convert to tsibble
  fc_rf %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))


# Random forest - interaction
rf_forecast_int =
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = "xpred_rf_int",
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }

fc_rf_int <- 
  cv_wrap(ls_rf_int_par, select_model, rf_forecast_int)

fc_rf_int <- # convert to tsibble
  fc_rf_int %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))


# XGBoost
xgb_forecast =
  function(.data) {
    .data$index = # get test data
      .data$index %>% filter(type == "test")
    .data$model = 
      .data$model %>% as_tibble()
    tmp_fc =
      bind_cols(.data$model, .data$index) %>% 
      mutate(
        .model = "xpred_xgb",
        .mean = mean, # necessary for fable::autoplot
        occ = dist_normal(mu = mean, sd = sd),
        mean = NULL,
        sd = NULL
      ) %>%
      relocate(c(.model, occ, .mean), .after = index)
  }

fc_xgb <- 
  cv_wrap(ls_xgb_par, select_model, xgb_forecast)

fc_xgb <- # convert to tsibble
  fc_xgb %>% 
  flatten() %>% 
  bind_rows() %>% 
  as_tsibble(index = index, key = c("split", "site", ".model"))


# Join fc
dimnames(fc_var$occ) <- "occ" # add name to column to match fc_fable
dimnames(fc_ese$occ) <- "occ" 
dimnames(fc_rf$occ) <- "occ"
dimnames(fc_rf_int$occ) <- "occ"
dimnames(fc_xgb$occ) <- "occ"
fc_all <- 
  list(
    fc_fable, fc_fable_xpred, fc_fable_xpred_rec, fc_var, 
    fc_ese, fc_rf, fc_rf_int, fc_xgb
    ) %>% 
  reduce(bind_rows)



# Save fits and forecasts ------------------------------------------------------
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


saveRDS(split_data_cv, file = paste0(save_path, "splits_short.RDS"))
saveRDS(data_xpredict, file = paste0(save_path, "data_xpredict.RDS"))
saveRDS(fit_all, file = paste0(save_path, "fits_short.RDS"))
saveRDS(fc_all, file = paste0(save_path, "forecasts_short.RDS"))
# fit_all <- readRDS(paste0(save_path, "fits_short.RDS"))
