#' Metrics computations
#'
#'  This scrips does two things:
#' 1) Compute Wilker and crps scores on cross-validated forecasts for each model:
#' - the modified Wilker score is the average score across different ci width
#' (100(1-\alpha)% confidence) for a specific time point interval. Compute the 
#' wilker score for different penalties, namely upper, symmetric and lower, 
#' where each penalty differently differently weights prediction errors for 
#' observations that fall above and below the upper and lower confidence 
#' interval, respectively. 
#' - the continuous ranked probability score (crps) is basically is the
#' difference between the cumulative probability distribution of the forecast 
#' and the Heaviside step function.
#' Use different bias for errors on the left/right side of the forecast
#' distributions.
#' 
#' 2) Combine forecasts from different models and (re-)compute metrics including
#' fc_comb; this is done here because combination depends on CRPS and average 
#' Wilker scores from individual models.
#'
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
# and Practice; https: //www.lokad.com/continuous-ranked-probability-score/;
#' Gneiting_JBES_2011
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-14


# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data --------------------------------------------------------------------
split_path <- here("output/fits/splits_short.RDS")
fc_path <- here("output/fits/forecasts_short.RDS")

split_data_cv <- readRDS(file = split_path)
fc_all <- readRDS(file = fc_path)

# Remove aggregated data
split_data_cv <- 
  split_data_cv %>% filter(!is_aggregated(site))
fc_all <- 
  fc_all %>% filter(!is_aggregated(site))



# Compute metrics --------------------------------------------------------------
# Compute
list_models <- # select models
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpt_l",
    "arima_dadpl_l",
    "arima_dadplt_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "arima_dadplt_rec",
    "var_ad",
    "var_ad2",
    "var_paed",
    "var_los",
    "var_h",
    "nn",
    "es",
    "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "xgb_not",
    "tslm",
    "snaive")


metrics <- # compute metrics
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all), 
    select_fc,
    wrap_metric,
    list_models
  ) %>% 
  flatten() %>%
  bind_rows()

metrics <- process_metrics(metrics)



# Combine forecasts and recompute metrics --------------------------------------
# Combine fc
fc_all_c <- 
  fc_comb_wrap(fc_all, metrics)


# Compute metrics
list_models_comb <- # select models (including combined)
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpt_l",
    "arima_dadpl_l",
    "arima_dadplt_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "arima_dadplt_rec",
    "var_ad",
    "var_ad2",
    "var_paed",
    "var_los",
    "var_h",
    "nn",
    "es",
    "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "xgb_not",
    "tslm",
    "snaive",
    "equal",
    "crps",
    "wilker"
  )


metrics_c <- # compute metrics
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all_c), 
    select_fc,
    wrap_metric,
    list_models_comb
  ) %>% 
  flatten() %>%
  bind_rows()

metrics_c <- process_metrics(metrics_c)



# Summarise metrics ------------------------------------------------------------
var_summary <- # not all models included for clarity
  c(
    "tslm",
    "snaive",
    "var_ad",
    "var_ad2",
    "var_los",
    "var_paed",
    "var_h",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpt_l",
    "arima_dadpl_l",
    "arima_dadplt_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "arima_dadplt_rec",
    "es",
    "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "xgb_not",
    "equal",
    "crps",
    "wilker"
  )

tmp_metrics <- 
  metrics_c %>%  
  filter(models %in% var_summary) # select variables for summary

metrics_summary <- 
  tmp_metrics %>%  
  filter(models %in% var_summary) %>%
  group_by(split, site, penalty, index, metric) %>% 
  summarise( # take best model
    "value_min" = min(value_s),
    "best_model" = 
      models[which.min(value_s)] %>% 
      {if (grepl("tslm|snaive", .)) "baseline_min" else .},
  ) %>% 
  ungroup() %>% 
  inner_join(tmp_metrics) %>% # join to tibble
  group_by(split, site, penalty, index, metric) %>% 
  group_modify( # add baseline_model
    \(.x, .y) {
      tmp = .x %>% head(1)
      tmp$models = "baseline_min"
      tmp$value_s = 
        min(.x$value_s[grepl("tslm|snaive", .x$models)])
      .x %>% 
      add_row(
        tmp,
        .before = 0
      )
    }
  ) %>% # remove single baseline models
  filter(!(models %in% c("tslm", "snaive"))) %>% 
  ungroup()

 

# Save -------------------------------------------------------------------------
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

metric_data =
  list(
  "metrics" = metrics,
  "metrics_comb" = metrics_c,
  "metrics_summary" = metrics_summary
)
saveRDS(fc_all_c, file = paste0(save_path, "forecasts_short_comb.RDS"))
saveRDS(metric_data, file = paste0(save_path, "metrics.RDS"))
