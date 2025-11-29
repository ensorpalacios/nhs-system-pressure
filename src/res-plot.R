#' Residuals plot
#'
#' Plot residuals over time, their autocorrelation and distribution
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-06-27
# Prepare environment ----------------------------------------------------------
rm(list = ls())
source("src/environment.R")



## THIS SHOULD BE SUBSTITUTED WITH loading fits from fit_short.R
# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
sites <- 
  ts_occ$site %>% unique() %>% 
  grep("<aggregated>", ., value = TRUE, invert = TRUE)



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



# Lag/split dataset (no cv) ----------------------------------------------------
horizon = 7
ts_occ_lag <- lag_fun(ts_occ, .lag = horizon) # lag data

split_data_tt <- # Train/test set
  split_tt(ts_occ_lag) %>% 
  mutate( # add split for compatibility
    split = '01'
  )



# Fit models -------------------------------------------------------------------
# Baseline and ARIMA models
fit_fable <- 
  split_data_tt %>% 
  filter(type == "train" & !is_aggregated(site)) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    # Baseline models (for comparison)
    mean = TSLM(occ),
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
  split_data_tt %>% 
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
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad = VAR(vars(occ, ad_diff_f) ~ season(period = "week"))
  )

fit_fable_var_ad_nof <- # adm - dis not filtered
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad_nof = VAR(vars(occ, ad_diff) ~ season(period = "week"))
  )

fit_fable_var_ad2 <- # diff(adm - dis)
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad2 = VAR(vars(occ, ad_diff2_f) ~ season(period = "week"))
  )

fit_fable_var_ad2_nof <- # diff(adm - dis) not filtered
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad2_nof = VAR(vars(occ, ad_diff2) ~ season(period = "week"))
  )


fit_fable_var_ad3 <- # diff(diff(adm - dis)
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad3 = VAR(vars(occ, ad_diff3_f) ~ season(period = "week"))
  )

fit_fable_var_ad3_nof <- # diff(diff(adm - dis) not filtered
  split_data_tt %>% 
  filter(type == "train", !is_aggregated(site)) %>%
  mutate(site = site %>% as.character()) %>% 
  tsibble(index = index, key = c(split, site)) %>%
  model(
    var_ad3_nof = VAR(vars(occ, ad_diff3) ~ season(period = "week"))
  )


fit_fable_var_other <- # BRI vs Southmead
  split_data_tt %>% 
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

fit_es <- # fit es
  cv_wrap(
    split_data_tt %>% filter(type == "train" & !is_aggregated(site)),
    select_training,  
    es_model, 
    list_var_ese)


# Random forest
list_var_rf <-
  split_data_tt %>%
  select(
    contains("occ"), -occ_other,
    matches("ad_diff.*_f"), -ad_diff_f, -ad_diff2_f, -ad_diff3_f,
    contains("days_"), -days_) %>%
  names()


ls_rf <- # fits + fc + parameters fc distribution
  cv_wrap(
    split_data_tt %>% filter(!is_aggregated(site)), 
    select_training,  rf_reg,  list_var_rf, "all"
  )

fit_rf <-  # extract fits
  ls_rf %>% 
  map(., ~ # site
        map(.x, ~ # split
              map(.x, ~ # horizon
                    pluck(.x, "fit"))))


# Save in list
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
    "rf_dae_f" = fit_rf
    )



# Plot residuals ---------------------------------------------------------------
# Helper function
plot_res_fun <- 
  function(.tmp_res, .var) {
    tmp_ts = 
      .tmp_res %>% autoplot()
    tmp_acf = 
      .tmp_res %>% ACF() %>% autoplot()
    tmp_hist =
      .tmp_res %>% ggplot(aes(x = !!sym(.var))) + geom_histogram()
    tmp_ts / (tmp_acf + tmp_hist)
  }

# Plot
plot_res = 
  map(sites, \(.site) {
  fit_all %>% names() %>% 
    map(\(.type_model) {
      if (grepl("fable", .type_model)) { # fable* models
        tmp_fit =
          fit_all %>% pluck(.type_model) 
        tmp_names = 
          tmp_fit %>% names() %>%
          grep(c("split|site"), ., value = TRUE, invert = TRUE)
        map(tmp_names, \(.model) {
          if  (grepl("other", .model)) {
            tmp_res = 
              tmp_fit %>% resid() %>% select(all_of(.site))
            plot_res_fun(tmp_res, .site)
          } else if (grepl("var", .model)) {
            tmp_res = 
              tmp_fit %>% filter(site ==.site) %>% resid() %>% select(occ)
            plot_res_fun(tmp_res, "occ")
          } else {
            tmp_fit %>% filter(site == .site) %>% select(all_of(.model)) %>% 
              gg_tsresiduals()
          }
        }) %>%
          set_names(tmp_names)
      } else if (grepl("es", .type_model)) { # exponential smoothing models
        tmp_res = 
          fit_all %>% pluck(.type_model, .site, "01", "residuals") %>%
          as_tibble() %>% 
          cbind( 
            split_data_tt %>% 
              filter(type == "train", site == .site) %>% select(index)
          ) %>%
          mutate(occ = x, x = NULL) %>% 
          as_tsibble(index = index)
        
        plot_res_fun(tmp_res, "occ") %>% 
          list("es" = .)
      } else if (grepl("rf", .type_model)) { # random forest models
        tmp_fit =
          fit_all %>% pluck(.type_model, .site, "01")
        tmp_fit %>% 
          map(\(.rf_lag){
            tmp_res =
              split_data_tt %>% 
              filter(type == "train", site == .site) %>% select(index, occ) %>% 
              mutate(
                occ = occ - predict(.rf_lag)
              ) %>% 
              as_tsibble(index = index)
            
            plot_res_fun(tmp_res, "occ")
            
          }) %>% 
          set_names(tmp_fit %>% names() %>% paste0("rf", "_", .))
      }
    }) %>% set_names(fit_all %>% names())
}) %>% set_names(sites)



# Save -------------------------------------------------------------------------
save_path = here("output/plots/residuals/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

walk(sites, \(.site) {
  fit_all %>% names() %>% 
    walk(\(.type_model) {
      tmp_name = 
        plot_res %>% pluck(.site, .type_model) %>% names()
      walk(tmp_name, \(.name_model) {
        tmp_path = str_glue("{save_path}{.site}_{.name_model}.eps")
        plot_res %>% pluck(.site, .type_model, .name_model) %>% 
          ggsave(file = tmp_path, width = 11, height = 7)
      })
    })
})

    