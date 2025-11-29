#' Metrics plot
#'
#' Generate plots of Wilker score and crps (continuous ranked probability
#' score). Scores generated from fit-models-short.R.
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-14

# Prepare environment ----------------------------------------------------------
rm(list = ls())
source("src/environment.R")



# Load data --------------------------------------------------------------------
metric_path = here("output/fits/metrics.RDS")
metric_data = readRDS(metric_path)

# Unpack from list
metrics_c <- metric_data$metrics_comb # metrics with combined models
metrics_summary_c <- metric_data$metrics_summary_c
sites <- metrics_c$site %>% unique()
metric_names <- metrics_c$metric %>% unique()


# Order metrics
metrics_c$models <- 
  metrics_c$models %>% factor(levels = names(col_models))
metrics_summary_c$models <- 
  metrics_summary_c$models %>% factor(levels = names(col_models))



# Generate plots ---------------------------------------------------------------
plt_metric_c <- # boxplot metrics with combined models
  map(metric_names, \(.metric) {
    metrics_c %>%
      filter(metric == .metric, models != "tslm") %>%
      ggplot(aes(x = models, y = value_s, colour = models)) +
      # geom_boxplot(outliers = FALSE) +
      ggdist::stat_interval(
        aes(alpha = after_stat(level),),
        linewidth = 5,
        point_interval = "median_qi",
        .width = c(.10, .50, .8)
        ) +
      ggdist::stat_pointinterval(
        color = "black",
        fatten_point = 1,
        .width = c(0)) +
      geom_hline(yintercept = 1, lty = "dotted") +
      scale_colour_manual(name = "models", values = col_models) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      facet_wrap(vars(site, penalty), scales = "free_y")
  }) %>%
  set_names(metric_names)



# Scores time series
max_y <- 4
plt_metric_time_summary_c <- # time series (summary data)
  map(metric_names, \(.metric) {
    map(sites, \(.site) {
      metrics_summary_c %>% 
        mutate(
        value_s = if_else(value_s > max_y, max_y, value_s) # cap y
        ) %>%
        filter(metric == .metric, site == .site) %>%
        ggplot(aes(x = t_ax, y = value_s, colour = models)) +
        geom_line(linewidth = 1) +
        geom_line(
          aes(x = t_ax, y = -0.5, colour = best_model, group = 1),
          linewidth = 5) +
        scale_colour_manual(name = "models", values = col_models) + 
        facet_wrap(vars(penalty), ncol = 1, strip.position = "right") +
        labs(title = .site) +
        ylim(-2, max_y) +
        geom_hline(yintercept = max_y,  linetype = "dashed", color = "black")
    }) %>%
      reduce(`+`) +
      plot_layout(ncol = 1, guides = "collect", axes = "collect")
  }) %>%
  set_names(metric_names)



# Generate tables --------------------------------------------------------------
# Models' metric mode and iqr - with combined models
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
save_path_m <- here("output/plots/metrics/")
if (!file.exists(save_path_m)) {
  dir.create(save_path_m, recursive = TRUE)
}

walk(metric_names, \(.metric) {
  plt_metric_c %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_boxplot.svg"),
      width = 15, height = 8.88,
      dpi = 500
    )
  
  plt_metric_time_summary_c %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_ts_summary.svg"),
      width = 15, height = 8.88,
      dpi = 500
    )
})

html2pdf(tbl_metric_c, str_glue("{save_path_m}table_metrics"))
html2pdf(tbl_freq_best_c, str_glue("{save_path_m}table_freq_best"))
# tbl_metric %>% 
#   gtsave(filename = str_glue("{save_path_m}table_metrics.html"))
# tbl_freq_best %>% 
#   gtsave(filename = str_glue("{save_path_m}table_freq_best.html"))
