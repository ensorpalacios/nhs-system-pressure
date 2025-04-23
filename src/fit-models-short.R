#!/usr/bin/env Rscript
            
#' Run ARIMA model
#'
#' Run different ARIMA model, inlcuding automatic search, (d=1, D=1), arima
#' with regressors (week days, other time series)
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-10

# Import libraries -------------------------------------------------------------
library(conflicted)
library(data.table)
library(tidyverse)
library(here)
library(fable)
library(feasts)
library(tsibble)
library(xtable)
library(rsample)
library(timetk)
# library(modeltime)
library(smooth)
library(distributional)

library(ggrain)
# library(gt)
# library(knitr)
# library(kableExtra)

# Manage conflicts
conflicts_prefer(
  dplyr::filter,
  fabletools::accuracy, # used in computing metrics for fable fc
  modeltime::`:=` # used in workflow_set
)


# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ts_occ <- readRDS(file = data_path)
ts_occ <- # drop original data with missing values
  ts_occ %>% select(-(ts_occ %>% names %>% grep("_m", .)))
sites <- ts_occ$site |> unique()


# Split dataset ----------------------------------------------------------------
# Training/test set
type_data <- c("train", "test")

split_data_tt <- 
  map(sites, \(tmp_site) {
    tmp_split = ts_occ %>% # split
      as_tibble() %>% # (for using time_series_split)
      filter(site == tmp_site) %>%
      time_series_split(
        date_var = index, 
        assess = "5 months", # length test set
        cumulative = TRUE)
    tmp_train =  # extract training set
      tmp_split %>% 
      training %>% 
      mutate(type = "train", .before = 1)
    tmp_test = 
      tmp_split %>% # extract test set
      testing %>% 
      mutate(type = "test", .before = 1)
    bind_rows(tmp_train, tmp_test)
  }) %>% 
  bind_rows() %>% 
  relocate(site, .before = 2)

# Cross-validation sets (from training set)
split_data_cv <- 
  map(sites, \(tmp_site) {
    tmp_split = # split with resampling
      split_data_tt %>% 
      filter(type == "train", site == tmp_site) %>% 
      time_series_cv(
        date_var = index,
        initial = "12 weeks", # (length training set)
        assess = "2 weeks", # (length validation set)
        skip = "8 weeks" # (separation between training sets)
      )
    tmp_split = # combine training and validation sets in tibble
      map2(tmp_split$splits, tmp_split$id, \(split_data, split_name) {
        split_name = split_name %>% str_sub(-2)
        tmp_train = 
          split_data %>% 
          training() %>% 
          mutate(split = split_name, .before = 1)
        tmp_assess = 
          split_data %>% 
          testing() %>% 
          mutate(split = split_name, .before = 1, type = "test")
        bind_rows(tmp_train, tmp_assess)
    }) %>% 
      bind_rows
  }) %>% 
  bind_rows() %>% # (combine sites)
  relocate(site, .before = 3)

splits <- split_data_cv$split %>% unique() # cv splits names

# General wrapper over map(sites) and map(splits)
cv_wrap <-
  function(.data, .select, .function, ...) { 
    map(sites, \(.site) {
      map(splits, \(.split) {
        .select(.data, .site, .split, ...) %>% 
          .function()
      }) %>% set_names(splits)
    }) %>% set_names(sites)
  }


# Fit models -------------------------------------------------------------------
# Baseline, ARIMA and exponential smoothing models
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
    # Exponential smoothing model
    es = ETS(bed_occ)
  )

# Exponential smoothing with predictors (bed escalation)
select_training <- # select training set (by site and split)
  function(.data, .site, .split, .vars) { 
    # .data is list (site) of list (split) of training sets 
    # .vrs is list of outcome (..1) and predictors
    split_data_cv %>% 
      filter(type == "train", site == .site, split == .split) %>% 
      select(all_of(.vars))
  }

es_model <- # define esx fit
  function(.data)  .data %>% as.ts() %>% es(model = "ZXZ",  lags = c(1, 1, 7))

list_var <- c("bed_occ", "bed_escal")

fit_ese <- # fit ese (exponential smoothing with bed escalation)
  cv_wrap(split_data_cv, select_training,  es_model, list_var)


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


# Generate plots ---------------------------------------------------------------
# Common functions
select_fc <- # select data for plot (by site and split)
  function(.data, .site, .split, .models) {
    # .data is list (site) of list (splits) of list with
    # $all full data and $fc forecast;
    # .model is a list of model whose forecasts are plotted
    list(
      "all" = 
        .data$all %>% 
        filter(site == .site, split == .split) %>% 
        as_tsibble(index = index),
      "test" =
        .data$fc %>% 
        filter(site == .site, .model %in% .models, split == .split)
    )
  }

list_models <- # select models to plot
  c(
    "arima",
    "arima_d",
    "arima_dad",
    "arima_de",
    # "es",
    "es_e",
    "mean",
    "naive",
    "snaive")


# Plot forecasts
plot_forecast <- # plot forecast
  function(.data) {
    .data$test %>% 
      autoplot() +
      autolayer(.data$all, .vars = bed_occ) +
      facet_wrap(vars(.model), ncol = 1, strip.position = "right")
  }


plt_fc <- # generate plots 
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all), 
    select_fc,
    plot_forecast,
    list_models
  )

# Plot models' metric
wilker_fun <- # compute Wilker score
  function(.obs, .fc, .alpha, .penalty) {
    # .data: contains forecasts distribution and observations (bed_occ)
    # .alpha: (1 - alpha) width of the confidence interval;
    # .penalty: penalise more observations above or below prediction interval
    
    upper = .fc %>% quantile(0.5 + .alpha/ 2) # (upper interval)
    lower = .fc %>% quantile(0.5 - .alpha/ 2) # (lower interval)
    ci_width = upper - lower # width confidence interval
    .penalty =
      case_when(
        .penalty == "upper" ~ 2,
        .penalty == "none" ~ 1,
        .penalty == "lower" ~ .5
      )
    case_when(
      .obs > upper ~ ci_width + (2 * .penalty) /.alpha * (.obs - upper),
      .obs < lower ~ ci_width + (2 / .penalty) /.alpha * (lower - .obs),
      .default = ci_width
    ) %>% 
      as_tibble_col(.alpha %>% as.character())
  }

wilker_wrap <- 
  function(.data_w) {
    # Compute Wilker for each penalty, model and alpha;
    tmp_model = .data_w %>% names %>% tail(-4)
    penalty = c("upper", "none", "lower")
    map(penalty, \(.penalty) {
      map(tmp_model, \(.model_name) {
        tmp_wlk = 
          map(seq(0.05, 0.95, 0.05), \(.alpha) {
            tmp_obs = .data_w[["bed_occ"]]
            tmp_fc = .data_w[[.model_name]] # forecast distribution
            wilker_fun(tmp_obs, tmp_fc, .alpha, .penalty)
          }) %>%
          bind_cols() %>% 
          rowMeans()
      }) %>% set_names(tmp_model) %>% 
        as_tibble() %>% 
        mutate(
          "split" = .data_w$split, 
          "site" = .data_w$site, 
          "penalty" = .penalty,
          "index" = .data_w$index) %>% 
        pivot_longer(
          cols = -c(split, site, penalty, index), 
          names_to = "models",
          values_to = "wilker"
          )
    }) %>% 
      bind_rows()
  }


wrap_metric <- 
  function(.data) {
    # General wrapper over metric computation; currently only using
    # Wikler score, but could add others.
    tmp_data = # combine obs and fc
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
    
    wilker_wrap(tmp_data)
  }

metrics <- # compute metrics
  cv_wrap(
  list("all" = split_data_cv , "fc" = fc_all), 
  select_fc,
  wrap_metric,
  list_models
  ) %>% 
  flatten() %>% 
  bind_rows()

# Plot
plt_metric <- # plot with penalt separately
  metrics %>% 
  ggplot(aes(x = models, y = wilker, fill = penalty, colour = penalty)) +
  geom_rain(
    point.args = list(size = 0.5),
    boxplot.args = list(color = "black", outlier.shape = NA, width = 0.3),
    violin.args = list(color = "black", width = 0.5),
    point.args.pos = 
      list(position = ggpp::position_dodgenudge(x = .2)),
    boxplot.args.pos = 
      list(position = ggpp::position_dodgenudge(x = 0)),
    violin.args.pos = 
      list(position = ggpp::position_dodgenudge(x = .2)),
    rain.side = "l",
  ) +
  facet_wrap(vars(site, penalty), scales ="free_y")

plt_metric2 <- # plot with penalty together (side by side)
  metrics %>%
  ggplot(aes(x = models, y = wilker, fill = penalty, colour = penalty)) +
  geom_rain(
    # alpha = .5,
    # rain.side = "l",
    point.args = list(size = 0.5),
    boxplot.args = list(color = "black", outlier.shape = NA, width = 0.3),
    violin.args = list(color = "black", width = 0.5),
    point.args.pos =
      list(position =ggpp::position_dodgenudge(x = .2, width = .1)),
    boxplot.args.pos =
      list(position =ggpp::position_dodgenudge(x = 0, width = .2)),
    violin.args.pos =
      list(position =ggpp::position_dodgenudge(x = .2, width = 0.2)),
    rain.side = "l",
  ) +
  facet_wrap(vars(site), ncol = 1, strip.position = "right")

# plt_metric3 <- # plot penalty over time
n_split <- metrics$split %>% unique() %>% tail(1) %>% as.numeric
  
metrics <- 
  metrics %>%
  group_by(split) %>% 
  mutate(
    t_ax = as.numeric(index),
    t_ax = t_ax - (t_ax[1]),
    t_ax = t_ax + 14 * (n_split - as.numeric(split)),
    highlight = grepl("arima_de|ese", models)
  )
  
plt_metric3 <- # separate plot for sites
  map(sites, \(.site) {
    metrics %>% 
      mutate(
        penalty = factor(penalty) %>% fct_rev()
        )%>% 
      filter(site == .site) %>% 
      ggplot() +
      # geom_line() +
      geom_line(
        aes(
          x = t_ax,
          y = wilker,
          colour = models,
          group = models,
          size = highlight)) +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.75)) +
      facet_wrap(vars(penalty), ncol = 1)
  }) %>%
  set_names(sites)
plt_metric3$BRI
plt_metric3$Southmead


# Save plots -------------------------------------------------------------------
save_path <- here("output/plots/forecasts/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}.png")
    plt_fc[[.site]][[.split]] %>% 
      ggsave(file = tmp_path, width = 7, height = 4)
  })
})


# Plot residuals .... 

