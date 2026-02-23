#' Metrics plot
#'
#' Generate plots of Wilker score and crps (continuous ranked probability
#' score). Scores generated from fit-models-short.R.
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
metric_path = here(paste0("output/fits/", amode, "/metrics.RDS"))
metric_data = readRDS(metric_path)

# Unpack from list
metrics_c <- metric_data$metrics_comb # metrics with combined models
metrics_other <- metric_data$metrics_other # other metrics
metrics_summary_c <- metric_data$metrics_summary_c
sites <- metrics_c$site %>% unique()
metric_names_other <- metrics_other$metric %>% unique()


# Order metrics
metrics_c$models <- 
  metrics_c$models %>% factor(levels = names(col_models))
metrics_other$model <- 
  metrics_other$model %>% factor(levels = names(col_models))
metrics_summary_c$models <- 
  metrics_summary_c$models %>% factor(levels = names(col_models))



# Generate plots ---------------------------------------------------------------
# Boxplot metrics with combined models
plt_metric_c <-
  metrics_c %>%
  # filter(metric == .metric, models != "tslm") %>%
  # ggplot(aes(x = models, y = value_s, colour = models)) +
  ggplot(aes(x = models, y = value, colour = models)) +
  # geom_boxplot(outliers = FALSE) +
  ggdist::stat_interval(
    aes(alpha = after_stat(level), ),
    linewidth = 5,
    point_interval = "median_qi",
    .width = c(.10, .50, .8)
  ) +
  ggdist::stat_pointinterval(
    color = "black",
    fatten_point = 1,
    .width = c(0)
  ) +
  # geom_hline(yintercept = 1, lty = "dotted") +
  scale_colour_manual(name = "models", values = col_models) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(site, penalty), scales = "free_y")


# Scores time series
max_y <- 4
plt_metric_time_summary_c <- # time series (summary data)
  map(sites, \(.site) {
    metrics_summary_c %>%
      mutate(
        value_s = if_else(value_s > max_y, max_y, value_s) # cap y
      ) %>%
      filter(site == .site) %>%
      ggplot(aes(x = t_ax, y = value_s, colour = models)) +
      geom_line(linewidth = 1) +
      geom_line(
        aes(x = t_ax, y = -0.5, colour = best_model, group = 1),
        linewidth = 5
      ) +
      scale_colour_manual(name = "models", values = col_models) +
      facet_wrap(vars(penalty), ncol = 1, strip.position = "right") +
      labs(title = .site) +
      ylim(-2, max_y) +
      geom_hline(yintercept = max_y, linetype = "dashed", color = "black")
  }) %>%
  reduce(`+`) +
  plot_layout(ncol = 1, guides = "collect", axes = "collect")


# Plot other metrics
plot_other_metrics <-
  lapply(metric_names_other, \(.metric) {
    tmp_interval = if_else(grepl("cover", .metric), "mean_qi", "median_qi")
    metrics_other |>
      filter(metric == .metric) |>
      ggplot(aes(x = model, y = value, colour = model)) +
      ggdist::stat_interval(
        aes(alpha = after_stat(level),),
        linewidth = 10,
        point_interval = "median_qi", #  mean_qi also returns quantile intervals
        .width = c(.10, .50, .8)
        ) +
      ggdist::stat_pointinterval(
        point_interval = tmp_interval,
        color = "black",
        fatten_point = 2,
        .width = c(0)
      ) +
      scale_colour_manual(name = "models", values = col_models) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(title = .metric) +
      facet_wrap(vars(site), ncol = 1)
  }) |> 
  set_names(metric_names_other)



# Generate tables --------------------------------------------------------------
# Best models across metrics
all_metrics <- # combine CRPS and other metrics
  rbindlist(
    list(
      as.data.table(metrics_c)[,
        .SD,
        .SDcols = -c("index", "t_ax", "value_s")
      ][,
        c("metric", "model") := .(paste0("crps", "_", penalty), models)
      ][,
        .SD,
        .SDcols = -c("penalty", "models")
      ],
      as.data.table(metrics_other)
    ),
    use.names = TRUE
  )

all_metrics <- # remove ensemble forecasts from table
  all_metrics[!grepl("crps|equal|baseline_min", model)]

newmap <- c( # map names based on table in paper
  arima            = "ARIMA (1)",
  arima_dad_l      = "ARIMA (2)",
  arima_dadp_l     = "ARIMA (3)",
  arima_dadpt_l    = "ARIMA (4)",
  arima_dadpl_l    = "ARIMA (5)",
  arima_dadplt_l   = "ARIMA (6)",
  arima_dad_rec    = "ARIMA rec. (1)",
  arima_dadp_re    = "ARIMA rec. (2)",
  arima_dadpl_rec  = "ARIMA rec. (3)",
  arima_dadplt_rec = "ARIMA rec. (4)",
  var_ad           = "VAR (1)",
  var_ad2          = "VAR (2)",
  var_paed         = "VAR (3)",
  var_los          = "VAR (4)",
  var_h            = "VAR (5)",
  nn               = "NNAR",
  es               = "ES",
  rf               = "rf (1)",
  rf_int           = "rf (2)",
  rf_int_not       = "rf (3)",
  xgb              = "XGBoost (1)",
  xgb_not          = "XGBoost (2)",
  tslm             = "linear model",
  snaive           = "s. naive"
)

all_metrics[, # rename models
  model := 
    factor(newmap[model], levels = newmap)
]

all_metrics_best <- # prepare table with models sorted for each metric
  dcast(
    all_metrics[,
      stat_fun(metric, value),
      by = c("site", "metric", "model")
    ],
    site + model ~ metric,
    value.var = c("stat")
  )[,
    mapply(
      order_fun,
      .SD,
      names(.SD),
      MoreArgs = list(.model = model),
      SIMPLIFY = FALSE
    ),
    by = c("site"),
    .SDcols = -c("model")
  ] |>
  setcolorder(c("cover80", "cover95"), after = "pball90")

tbl_metric_best_crps <- # generate table (CRPS)
  all_metrics_best[, .SD, .SDcols = grepl("site|crps", names(all_metrics_best))] |> 
  gt(groupname_col = "site", row_group_as_column = T)

tbl_metric_best_nocover <- # generate table (other)
  all_metrics_best[, .SD, .SDcols = !grepl("crps|cover", names(all_metrics_best))] |> 
  gt(groupname_col = "site", row_group_as_column = T)

tbl_metric_best_cover <- # generate table (other)
  all_metrics_best[, .SD, .SDcols = grepl("site|cover", names(all_metrics_best))] |> 
  gt(groupname_col = "site", row_group_as_column = T)


# Models' metric median and iqr - with combined models
tbl_metric_c <- 
  metrics_c %>% # attention: values not scaled by tslm!
  group_by(site, penalty, metric, models) %>%
  summarise( # Compute median and iqr
    median = median(value),
    iqr = IQR(value)
  ) %>% 
  ungroup() %>%
  mutate( # prepare data for table
    site = factor(site), 
    median = sprintf("%.2f", median),
    iqr = sprintf("%.2f", iqr),
    `median (iqr)` = str_glue("{median} ({iqr})"),
    median = NULL,
    iqr = NULL
  ) %>% 
  arrange(metric, penalty) %>% # reorder for hierarchical columns
  pivot_wider( # convert to wide for hierarchical columns
    names_from = c(metric, penalty),
    values_from = `median (iqr)`,
    names_sep = "-"
  ) %>%
  mutate( # reorder by crps-upper
    ref_median = as.numeric(substr(`crps-upper`, 1, 4)),
    ref_iqr = as.numeric(substr(`crps-upper`, 7, 10))
  ) %>% group_by(site) %>% arrange(ref_median, ref_iqr) %>%
  ungroup() %>% select(-ref_median, -ref_iqr) %>%
  arrange(site) %>% 
  gt(
    rowname_col = "models", groupname_col = "site", row_group_as_column = TRUE
  ) %>% 
  tab_spanner_delim(delim = "-")


# Get lighter colour palette and add to table
col_models_l <- bright_col(col_models, 0.6)

for (i in seq_along(col_models_l)) { # add colour to table column
  .color = col_models_l[i]
  .model = names(col_models_l)[i]

  tbl_metric_c =
    tbl_metric_c %>% 
    tab_style(
      style = cell_fill(.color),
      # location = cells_stub(rows = models == .model)
      location = cells_stub(rows = models == .model[[1]])
    ) %>% 
    tab_style(
      style = cell_fill(.color),
      location = cells_body(rows = models == .model)
    )
}


# Models' frequency as top model (scaled metrics) - with combined models
tbl_freq_best_c <- 
  metrics_summary_c %>% 
  select(split, site, penalty, metric, best_model, models) %>% 
  group_by(site, penalty, metric, models) %>% 
  summarise(
    freq = sum(best_model == models)
  ) %>% ungroup() %>% arrange(metric, penalty) %>% 
  pivot_wider(
    names_from = c(metric, penalty), 
    values_from = freq,
    names_sep = "-"
  ) %>% 
  mutate( # reorder by crps-upper
    site = factor(site), # required for groupname_col = "site"
    ref_freq = as.numeric(`crps-upper`),
  ) %>% group_by(site) %>% arrange(site, -ref_freq) %>% 
  ungroup() %>% select(-ref_freq) %>%
  gt(
    rowname_col = "models", groupname_col = "site", row_group_as_column = TRUE
  ) %>%
  tab_spanner_delim(delim = "-") %>% 
  opt_vertical_padding(scale = 0.65)# %>% 


for (i in seq_along(col_models_l)) { # add raw colours by model
  .color = col_models_l[i]
  .model = names(col_models_l)[i]
  tbl_freq_best_c =
    tbl_freq_best_c %>% 
    tab_style(
      style = cell_fill(.color),
      location = cells_stub(rows = models == .model)
    ) %>%
    tab_style(
      style = cell_fill(.color),
      location = cells_body(rows = models == .model)
    )
}



# Save plots -------------------------------------------------------------------
save_path_m <- here(paste0("output/plots/metrics/", amode, "/"))
if (!file.exists(save_path_m)) {
  dir.create(save_path_m, recursive = TRUE)
}

plt_metric_c %>% 
    ggsave(
      file = str_glue("{save_path_m}crps_boxplot.svg"),
      width = 15, height = 8.88
    )
  
walk(metric_names_other, \(.metric){
  plot_other_metrics |> pluck(.metric) |> 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_boxplot.svg"),
      width = 15, height = 8.88
    )
})
  
plt_metric_time_summary_c %>%
  ggsave(
    file = str_glue("{save_path_m}crps_ts_summary.svg"),
    width = 15, height = 8.88
  )

tbl_metric_best_crps |> gtsave(str_glue("{save_path_m}table_metric_best_crps.tex"))
tbl_metric_best_nocover |> gtsave(str_glue("{save_path_m}table_metric_best_nocover.tex"))
tbl_metric_best_cover |> gtsave(str_glue("{save_path_m}table_metric_best_cover.tex"))
html2pdf(tbl_metric_c, str_glue("{save_path_m}table_metrics"))
html2pdf(tbl_freq_best_c, str_glue("{save_path_m}table_freq_best"))
# tbl_metric %>% 
#   gtsave(filename = str_glue("{save_path_m}table_metrics.html"))
# tbl_freq_best %>% 
#   gtsave(filename = str_glue("{save_path_m}table_freq_best.html"))
