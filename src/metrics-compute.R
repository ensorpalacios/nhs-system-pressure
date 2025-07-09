#' Metrics
#'
#' Compute Wilker and crps scores on cross-validated forecasts for each model:
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

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
# and Practice; https: //www.lokad.com/continuous-ranked-probability-score/;
#' Gneiting_JBES_2011
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
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



# Helper functions -------------------------------------------------------------
crps_func <-  # Compute crps
  function(.obs, .fc, .penalty) {
    # .obs: observed value
    # .fc: forecast distribution
    # .penalty: 
    tmp_alpha = seq(0.01, 0.99, 0.01) # alpha level ([0, 1])
    tmp_weight =
      .penalty %>% 
      case_match(
        "upper" ~ expr("tmp_alpha ** 2"),
        "none" ~ expr("1"),
        "lower" ~ expr("(1 - tmp_alpha) ** 2")
      )
    
    map2(.fc, .obs, \(.dist, .obs_) {
      tmp_qf = # quantile forecast
        quantile(.dist, tmp_alpha)[[1]]
      case_when(
        .obs_ > tmp_qf ~ 
          -tmp_alpha * (tmp_qf - .obs_) * eval(parse(text = tmp_weight)),
        .obs_ <= tmp_qf ~ 
          (1 - tmp_alpha) * (tmp_qf - .obs_) * eval(parse(text = tmp_weight))
      ) %>% 
        sum() * 2 / (length(tmp_alpha) - 1)
    }) %>% 
      list_c()
    # tmp_domain = seq(0, 2000, 1) # ATTENTION: ad hoc domain common to BRI/Southmead
    # crps_p =
    #   map2(.fc, .obs, \(.dist, .obs_) {
    #   case_when(
    #     tmp_domain < .obs_ ~  cdf(.dist, tmp_domain)[[1]] ** 2,
    #     tmp_domain >= .obs_  ~ (cdf(.dist, tmp_domain)[[1]] - 1) ** 2
    #   ) %>% 
    #     sum() * 
    #        ((tail(tmp_domain, 1) - head(tmp_domain, 1)) /
    #           (length(tmp_domain) - 1))
    # }) %>% 
    #   list_c()
  }

wilker_func <- # compute Wilker score - used in wilker_wrap()
  function(.obs, .fc, .penalty) {
    # .obs: observed value
    # .fc: forecast distribution
    # .penalty: penalise more observations above or below prediction interval
    ci_width = seq(0.05, 0.95, 0.05) # width of the confidence interval
    map(ci_width, \(.width) {
      upper = .fc %>% quantile(0.5 + .width / 2) # (upper interval)
      lower = .fc %>% quantile(0.5 - .width / 2) # (lower interval)
      ci_width = upper - lower # width confidence interval
      .penalty =
        case_when(
          .penalty == "upper" ~ 2,
          .penalty == "none" ~ 1,
          .penalty == "lower" ~ .5
        )
      case_when(
        .obs > upper ~ ci_width + (2 * .penalty) /.width * (.obs - upper),
        .obs < lower ~ ci_width + (2 / .penalty) /.width * (lower - .obs),
        .default = ci_width
      ) %>% 
        as_tibble_col(.width %>% as.character())
    }) %>% 
      list_cbind() %>% 
      rowMeans()
  }

wrap_metric <- # general wrapper over metric function - used in cv_wrap()
  function(.data) {
    # Organise obs and fc distributions in one tibble
    tmp_data =
      .data$all %>% 
      filter(type == "test") %>% 
      select(split, site, index, occ) %>% 
      left_join(
        .data$test %>%
          as_tibble() %>%
          select(index, .model, occ) %>% 
          pivot_wider(names_from = .model, values_from = occ),
        by = "index"
      )
    
    # Compute metric for each model and penalty
    ls_models = tmp_data %>% names %>% tail(-4)
    penalty = c("upper", "none", "lower")
    map(penalty, \(.penalty) {
      map(ls_models, \(.model_name) {
        tmp_obs = tmp_data[["occ"]]
        tmp_fc = tmp_data[[.model_name]] # forecast distribution
        tibble(
          split = tmp_data$split,
          site = tmp_data$site,
          penalty = .penalty,
          index = tmp_data$index,
          wilker = wilker_func(tmp_obs, tmp_fc, .penalty),
          crps = crps_func(tmp_obs, tmp_fc, .penalty)
        ) %>% pivot_longer(
          cols = where(is.numeric), 
          names_to = "metric",
          values_to = .model_name)
      }) %>% 
        reduce(
          left_join, 
          by = c("split", "site", "penalty", "index", "metric")
        ) %>% 
        pivot_longer(
          cols = where(is.numeric),
          names_to = "models"
        )
    }) %>% 
      list_rbind()
  }



# Compute/summarise metrics ----------------------------------------------------
# Compute
list_models_m <- # select models
  c(
    "arima",
    "arima_dad",
    "arima_dad_l",
    "arima_dad_nof",
    "arima_dado",
    "arima_dado_l",
    "var_ad",
    "var_ad_nof",
    "var_ad2",
    "var_ad2_nof",
    "var_ad3",
    "var_ad3_nof",
    "var_BRI",
    "var_Southmead",
    "locf_arima_dad",
    "locf_arima_dad_l",
    "locf_arima_dad_l_nof_rec",
    "locf_arima_dad_nof",
    "locf_arima_dad_rec",
    "locf_arima_dado",
    "locf_arima_dado_l",
    "locf_es_ado_f",
    "rf_dado_f",
    "rf_dado_f_int",
    "mean",
    "naive",
    "snaive")

metrics <- # compute metrics
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all), 
    select_fc,
    wrap_metric,
    list_models_m
  ) %>% 
  flatten() %>%
  bind_rows()


# Wrangling
n_split <- # number of splits 
  metrics$split %>% unique() %>% tail(1) %>% as.numeric()

metrics <- # add joint time axis
  metrics %>%
  group_by(split) %>% 
  mutate(
    t_ax = as.numeric(index),
    t_ax = t_ax - (t_ax[1]),
    t_ax = t_ax + 7 * (n_split - as.numeric(split))
  ) %>% 
  ungroup()

metrics <- # scale by mean model score
  metrics %>% 
  group_by(site, penalty, metric, index) %>%
  mutate(
    value_s = value / value[models == "mean"]
  ) %>% 
  ungroup()

metrics <-   
  metrics %>% 
  mutate(
    penalty = factor(penalty) %>% fct_rev()
  )

# Summarise
var_summary <- 
  c(
    "mean", # Either one of mean, naive, snaive
    "naive", # Either one of mean, naive, snaive
    "snaive", # Either one of mean, naive, snaive
    "arima_dad_l", # looks the best (2024-06-22)
    "arima_dado_l", # looks the best (2024-06-22)
    "var_ad",
    "var_ad2",
    "var_ad3",
    "var_BRI",
    "var_Southmead",
    "locf_arima_dad",
    "locf_arima_dad_rec",
    "locf_arima_dado",
    "rf_dado_f",
    "rf_dado_f_int"
  )

tmp_metrics <- 
  metrics %>%  
  filter(models %in% var_summary) # select variables for summary

metrics_summary <- 
  tmp_metrics %>%  
  filter(models %in% var_summary) %>%
  group_by(split, site, penalty, index, metric) %>% 
  summarise( # take best model
    "value_min" = min(value_s),
    "best_model" = 
      models[which.min(value_s)] %>% 
      {if (grepl("mean|naive|snaive", .)) "baseline_min" else .},
  ) %>% 
  ungroup() %>% 
  inner_join(tmp_metrics) %>% # join to tibble
  group_by(split, site, penalty, index, metric) %>% 
  group_modify( # add baseline_model
    \(.x, .y) {
      tmp = .x %>% head(1)
      tmp$models = "baseline_min"
      tmp$value_s = 
        min(.x$value_s[grepl("mean|naive|snaive", .x$models)])
      .x %>% 
      add_row(
        tmp,
        .before = 0
      )
    }
  ) %>% # remove single baseline models
  filter(!(models %in% c("mean", "naive", "snaive")))
 
# metrics_summary <-
#   metrics %>% 
#   # mutate(
#   #   penalty = factor(penalty) %>% fct_rev()
#   # ) %>% 
#   group_by(split, site, penalty, index, metric) %>% 
#   summarise(
#     # Add time axis
#     "t_ax" = t_ax[1],
#     # Take best model
#     "value_min" = min(value),
#     "best_model" = 
#       models[which.min(value)] %>% 
#       {if (grepl("mean|naive|snaive", .)) "baseline_min" else .},
#     # Scale score by best score
#     # "arima" = value[models == "arima"] / value_min,
#     # "arima_dae" = value[models == "arima_dae"] / value_min,
#     # # "arima_dae_c" = value[models == "arima_dae_c"] / value_min,
#     # # "arima_dae_c_locf" = value[models == "arima_dae_c_locf"] / value_min,
#     # "arima_dae_f" = value[models == "arima_dae_f"] / value_min,
#     # "arima_dae_f_locf" = value[models == "arima_dae_f_locf"] / value_min,
#     # "es_ae_c" = value[models == "es_ae_c"] / value_min,
#     # "rf" = value[models == "rf"] / value_min,
#     # Pull baseline models together
#     "baseline_min" =
#       min(value[grepl("mean|naive|snaive", models)]) / value_min,
#   ) %>% 
#   inner_join(metrics)
#   pivot_longer(
#     cols = -c(split:best_model),
#     names_to = "models"
#   ) %>% 
#   ungroup()


# Save -------------------------------------------------------------------------
save_path = here("output/metrics/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

metric_data =
  list(
  "metrics" = metrics,
  "metrics_summary" = metrics_summary
)
saveRDS(metric_data, file = paste0(save_path, "metrics.RDS"))
