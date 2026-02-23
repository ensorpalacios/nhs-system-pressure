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

# Prepare environment ----------------------------------------------------------
# rm(list = ls())
renv::activate()
source("src/packages.R")
source("src/environment.R")

args <- commandArgs(trailingOnly = TRUE)
if (!args[1] %in% c("train", "test")) {
  stop("Invalid analysis mode argument. Must be either train or test")
}

amode <- args[1]
setup_env(amode) # define global environment variables



# Load data --------------------------------------------------------------------
split_path <- here(paste0("output/fits/", amode, "/splits_short.RDS"))
fc_path <- here(paste0("output/fits/", amode, "/forecasts_short.RDS"))
  
split_data_cv <- readRDS(file = split_path)
fc_all <- readRDS(file = fc_path)
  
# Remove aggregated data
split_data_cv <- 
  split_data_cv %>% filter(!is_aggregated(site)) %>% rec_site()
fc_all <- 
  fc_all %>% filter(!is_aggregated(site)) %>% rec_site()
  


# Compute metrics --------------------------------------------------------------
# Compute
list_models <- # select models
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpl_l",
    "arima_dadpt_l",
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


if (amode == "train") {
  metrics <- # compute metrics
    cv_wrap(
      list("all" = split_data_cv , "fc" = fc_all), 
      select_fc,
      wrap_metric,
      list_models,
      "crps"
    ) %>% 
    flatten() %>%
    bind_rows()
  
  metrics <- # add t_ax, scale by TSLM, factor(penalty)
    process_metrics(metrics)
}



# Mix forecasts and recompute metrics ------------------------------------------
# Mix fc (linear mixture, not combination)
# List models
list_best_models <-
  list(
    "BRI" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_ad2", "var_paed", "xgb"),
    "Southmead" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_paed", "var_h", "xgb")
  )

if (amode == "train") { # compute only for training data, otherwise load
  weights_comb <- 
    comp_weights(metrics, list_best_models)
} else if (amode == "test") {
  path_weights <- here(paste0("output/fits/train/weights_training.RDS"))
  if (!file.exists(path_weights)) {
    stop("running on test; no weights_training.RDS file found")
  } else {
    weights_comb <- readRDS(file = path_weights)
  }
}

fc_all_c <- 
  fc_comb_wrap(fc_all, weights_comb, list_best_models)


# Compute metrics
list_models <- # update list with combined
  c(list_models, 
    "equal",
    "crps",
    "crps_upper",
    "crps_lower")


metrics_c <- # compute crps
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all_c), 
    select_fc,
    wrap_metric,
    list_models,
    "crps"
  ) %>% 
  flatten() %>%
  bind_rows()

metrics_c <- # add t_ax, scale by TSLM, factor(penalty)
  process_metrics(metrics_c)

metrics_other <- # compute other metrics
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all_c), 
    select_fc,
    wrap_metric,
    list_models,
    "other"
  ) %>% 
  flatten() %>%
  bind_rows()



# Summarise metrics ------------------------------------------------------------
# tmp_metrics <- 
#   metrics_c
  # filter(models %in% list_modelvar_summary) # select variables for summary

metrics_summary_c <- # with combined model (CRPS metrics)
  metrics_c %>% 
  # tmp_metrics %>%  
  # filter(models %in% var_summary) %>%
  group_by(split, site, penalty, index, metric) %>% 
  summarise( # take best model
    "value_min" = min(value_s),
    "best_model" = 
      models[which.min(value_s)]# %>% 
      # {if (grepl("tslm|snaive", .)) "baseline_min" else .},
  ) %>% 
  ungroup() %>% 
  inner_join(metrics_c) %>% # join to tibble
  # inner_join(tmp_metrics) %>% # join to tibble
  # group_by(split, site, penalty, index, metric) %>% 
  # group_modify( # add baseline_model
  #   \(.x, .y) {
  #     tmp = .x %>% head(1)
  #     tmp$models = "baseline_min"
  #     tmp$value_s = 
  #       min(.x$value_s[grepl("tslm|snaive", .x$models)])
  #     .x %>% 
  #     add_row(
  #       tmp,
  #       .before = 0
  #     )
  #   }
  # ) %>% # remove single baseline models
  # filter(!(models %in% c("tslm", "snaive"))) %>% 
  ungroup()



# Save -------------------------------------------------------------------------
save_path = here(paste0("output/fits/", amode, "/"))

if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

metric_data =
  list(
  "metrics_comb" = metrics_c,
  "metrics_other" = metrics_other,
  "metrics_summary_c" = metrics_summary_c
)
if (amode == "train") { # computed only for training data
  metric_data$metrics = metrics
}

saveRDS(fc_all_c, file = paste0(save_path, "forecasts_short_comb.RDS"))
saveRDS(metric_data, file = paste0(save_path, "metrics.RDS"))
if (amode == "train") {
  saveRDS(weights_comb, file = paste0(save_path, "weights_training.RDS"))
}
