#' Metrics score
#'
#' Compute a modified Wilker scores on cross-validated forecasts for each model;
#' the modified Wilker score is the average score across different ci width
#' (100(1-\alpha)% confidence) for a specific time point interval. Compute the 
#' wilker score for different penalties, namely upper, symmetric and lower, 
#' where each penalty differently differently weights prediction errors for 
#' observations that fall above and below the upper and lower confidence 
#' interval, respectively. Compute also the continuous ranked probability score
#' (crps), which basically is the difference between the cumulative probability
#' distribution and the Heaviside step function (cumulative of a probability
#' centred around the observed value).

#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice; https: //www.lokad.com/continuous-ranked-probability-score/ 
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
      select(split, site, index, bed_occ) %>% 
      left_join(
        .data$test %>%
          as_tibble() %>%
          select(index, .model, bed_occ) %>% 
          pivot_wider(names_from = .model, values_from = bed_occ),
        by = "index"
      )
    
    # Compute metric for each model and penalty
    ls_models = tmp_data %>% names %>% tail(-4)
    penalty = c("upper", "none", "lower")
    map(penalty, \(.penalty) {
      map(ls_models, \(.model_name) {
        tmp_obs = tmp_data[["bed_occ"]]
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
list_models_w <- # select models
  c(
    "arima",
    "arima_d",
    "arima_dad",
    "arima_de",
    "arima_dade",
    "es_e",
    "mean",
    "naive",
    "snaive")

metrics <- # compute metrics
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all), 
    select_fc,
    wrap_metric,
    list_models_w
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
    t_ax = t_ax + 14 * (n_split - as.numeric(split))
  ) %>% 
  ungroup()

metrics <- # scale by wilker from mean model
  metrics %>% 
  group_by(site, penalty, metric, index) %>%
  mutate(
    value_s = value / value[models == "mean"]
  ) %>% 
  ungroup()

# metrics <- # discard upper/lower for crps (for now not implemented)
#   metrics %>%
#   filter(metric == "crps", ! penalty %in% c("upper", "lower"))

# Summarise
metrics_summary <-
  metrics %>% 
  mutate(
    penalty = factor(penalty) %>% fct_rev()
  ) %>% 
  group_by(split, site, penalty, index, metric) %>% 
  summarise(
    # Add time axis
    "t_ax" = t_ax[1],
    # Take best model
    "value_min" = min(value),
    "best_model" = 
      models[which.min(value)] %>% 
      {if (grepl("mean|naive|snaive", .)) "baseline_min" else .},
    # Scale score by best score
    "arima_dade" = value[models == "arima_dade"] / value_min,
    "arima_de" = value[models == "arima_de"] / value_min,
    "es_e" = value[models == "es_e"] / value_min,
    # Pull baseline models together
    "baseline_min" =
      min(value[grepl("mean|naive|snaive", models)]) / value_min,
  ) %>% 
  pivot_longer(
    cols = -c(split:best_model),
    names_to = "models"
  ) %>% 
  ungroup()


# Save -------------------------------------------------------------------------
save_path = here("output/metrics/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

wilker_data =
  list(
  "metrics" = metrics,
  "metrics_summary" = metrics_summary
)
saveRDS(wilker_data, file = paste0(save_path, "metrics.RDS"))