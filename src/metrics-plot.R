#' Metrics plot
#'
#' Generate plots of Wilker score and crps (continuous ranked probability
#' score). Scores generated from fit-models-short.R.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
library(conflicted)
library(ggplot2)
library(scales)
library(purrr)
library(patchwork)
library(dplyr)
import::from(here, here)
import::from(magrittr, "%>%")
import::from(rlang, set_names)
import::from(forcats, fct_rev)
import::from(stringr, str_glue)
source(here("src/colour-mapping.R"))

conflicts_prefer(
  dplyr::filter,
)

# Load data --------------------------------------------------------------------
metric_path = here("output/metrics/metrics.RDS")
metric_data = readRDS(metric_path)

# Unpack from list
metrics <- metric_data$metrics
metrics_summary <- metric_data$metrics_summary
sites <- metrics$site %>% unique()
metric_names <- metrics$metric %>% unique()

# Generate plots ---------------------------------------------------------------
plt_metric <- # boxplot
  map(metric_names, \(.metric) {
    metrics %>% 
      filter(metric == .metric, models != "mean") %>% 
      ggplot(aes(x = models, y = value_s)) +
      geom_boxplot(outliers = FALSE) +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      facet_wrap(vars(site, penalty), scales ="free_y")
  }) %>%
  set_names(metric_names)


# Plot penalty over time
plt_metric_time <- # time series Wilker score
  map(metric_names, \(.metric) {
    map(sites, \(.site) {
      metrics %>% 
        filter(metric == .metric, models != "mean") %>% 
        mutate(
          penalty = factor(penalty) %>% fct_rev()
        )%>% 
        filter(site == .site) %>% 
        ggplot(aes(x = t_ax, y = value_s, colour = models)) +
        # geom_line() +
        geom_line(linewidth = 1) +
        scale_colour_manual(name = "models", values = col_models) + 
        facet_wrap(vars(penalty), ncol = 1, strip.position = "right") +
        labs(title = .site)
    }) %>%
      reduce(`+`) +
      plot_layout(ncol = 1, guides = "collect")
  }) %>%
  set_names(metric_names)
  
plt_metric_time_summary <- # time series (summary data)
  map(metric_names, \(.metric) {
    map(sites, \(.site) {
      metrics_summary %>% 
        filter(metric == .metric, site == .site) %>%
        ggplot(aes(x = t_ax, y = value, colour = models)) +
        geom_line(linewidth = 1) +
        geom_line(
          aes(x = t_ax, y = -2, colour = best_model, group = 1),
          linewidth = 5) +
        scale_colour_manual(name = "models", values = col_models) + 
        facet_wrap(vars(penalty), ncol = 1, strip.position = "right") +
        labs(title = .site)
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
  
  plt_metric_time %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_ts.eps"),
      width = 15, height = 8.88,
      dpi = 500
    )
  plt_metric_time_summary %>% 
    pluck(.metric) %>% 
    ggsave(
      file = str_glue("{save_path_m}{.metric}_ts_summary.eps"),
      width = 15, height = 8.88,
      dpi = 500
    )
})

# Plot residuals .... 