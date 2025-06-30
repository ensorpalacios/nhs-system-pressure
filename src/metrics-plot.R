#' Metrics plot
#'
#' Generate plots of Wilker score and crps (continuous ranked probability
#' score). Scores generated from fit-models-short.R.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-14

# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")
source("src/colour-mapping.R")



# Load data --------------------------------------------------------------------
metric_path = here("output/metrics/metrics.RDS")
metric_data = readRDS(metric_path)

# Unpack from list
metrics <- metric_data$metrics
metrics_summary <- metric_data$metrics_summary
sites <- metrics$site %>% unique()
metric_names <- metrics$metric %>% unique()

# Get subset models
metrics <- 
  metrics %>% 
  filter(!grepl("locf.*_l", models)) # exclude locf lagged models
  # filter(!grepl("var_(?!ad)", models, perl = TRUE)) # exclude VAR

metrics_summary <- 
  metrics_summary %>% filter(!grepl("locf.*_l", models))



# Generate plots ---------------------------------------------------------------
plt_metric <- # boxplot
  map(metric_names, \(.metric) {
    metrics %>% 
      filter(metric == .metric, models != "mean") %>% 
      ggplot(aes(x = models, y = value_s)) +
      geom_boxplot(outliers = FALSE) +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      facet_wrap(vars(site, penalty), scales = "free_y")
  }) %>%
  set_names(metric_names)


# Scores time series
max_y <- 4
# plt_metric_time <- # time series scores
#   map(metric_names, \(.metric) {
#     map(sites, \(.site) {
#       metrics %>% 
#         filter(metric == .metric, models != "mean") %>% 
#         mutate(
#           penalty = penalty %>% fct_rev(),
#           value_s = if_else(value_s > max_y, max_y, value_s) # cap y
#         ) %>% 
#         filter(site == .site) %>% 
#         ggplot(aes(x = t_ax, y = value_s, colour = models)) +
#         # geom_line() +
#         geom_line(linewidth = 1) +
#         scale_colour_manual(name = "models", values = col_models) + 
#         facet_wrap(vars(penalty), ncol = 1, strip.position = "right") +
#         labs(title = .site) +
#         ylim(0, max_y) +
#         geom_hline(yintercept = max_y,  linetype = "dashed", color = "black")
#     }) %>%
#       reduce(`+`) +
#       plot_layout(ncol = 1, guides = "collect")
#   }) %>%
#   set_names(metric_names)
plt_metric_time_summary <- # time series (summary data)
  map(metric_names, \(.metric) {
    map(sites, \(.site) {
      metrics_summary %>% 
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



# Save plots -------------------------------------------------------------------
save_path_m <- here("output/plots/metrics/")
if (!file.exists(save_path_m)) {
  dir.create(save_path_m, recursive = TRUE)
}

walk(metric_names, \(.metric) {
  plt_metric %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_boxplot.eps"),
      width = 15, height = 8.88,
      dpi = 500
    )
  
  # plt_metric_time %>% 
  #   pluck(.metric) %>% 
  #   ggsave(
  #     file = str_glue("{save_path_m}{.metric}_ts.eps"),
  #     width = 15, height = 8.88,
  #     dpi = 500
  #   )
  plt_metric_time_summary %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_ts_summary.eps"),
      width = 15, height = 8.88,
      dpi = 500
    )
})