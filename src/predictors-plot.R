#' Plot predictors data

#' Generate plot to describe predictors data and their relationship with bed occupancy
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#' CI auto-/cross-corraltion is 1−α/2 quantile * standard deviation of 
#' autocorrelation (sqrt(var)=1/sqrt(n))
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-08

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript

# Import libraries ------------------------------------------------------------
library(conflicted)
library(data.table)
library(tidyverse)
library(here)
library(patchwork)
library(feasts)
library(dplyr)
library(knitr)
library(kableExtra)
library(imputeTS)

conflicts_prefer(dplyr::filter)

# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ts_occ <- readRDS(file = data_path)
sites <- ts_occ$site %>% unique


# Plot single predictors (highlight imputations) ------------------------------
# Admissions
plot_adm_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$adm_m,
    title = "Admissions",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$adm_m,
    ts_occ %>% filter(site == "Southmead") %>% .$adm,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")

# Discharges
plot_dis_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$dis_m,
    title = "Discharges",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$dis_m,
    ts_occ %>% filter(site == "Southmead") %>% .$dis,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")

# Bed escalation
plot_escal_miss <-
  ggplot_na_distribution(
    ts_occ %>% filter(site == "BRI") %>%  .$bed_escal_m,
    title = "Escalation bed",
    subtitle = "BRI"
    ) +
  ggplot_na_imputations(
    ts_occ %>% filter(site == "Southmead") %>% .$bed_escal_m,
    ts_occ %>% filter(site == "Southmead") %>% .$bed_escal,
    size_imputations = 5,
    title = NULL,
    subtitle = "Southmead"
    ) +
  plot_layout(ncol = 1, axis = "collect_x")


# Plot bed occupancy and predictors -------------------------------------------
# Time series
ts_occ_l <- # convert in long format
  ts_occ %>% 
    pivot_longer(
      cols = c(bed_occ, adm, dis, bed_escal),
      names_to = "var"
    ) %>% 
      mutate(
        var = factor(var, levels = c("bed_occ", "adm", "dis", "bed_escal"))
      )
plot_together <- # plot
  ts_occ_l |>
  as.data.frame() |> 
  ggplot(aes(x = index, y = value, colour = site)) +
  geom_line() +
  facet_wrap(vars(var), ncol = 1, scales = "free")

# Plot cross-correlation (! positive values means var_ccf leads bed occupancy)
var_ccf = c("adm", "dis", "bed_escal")
tbl_ccf = # compute ccf
  map(var_ccf, \(x) {
    tmp_ccf = ts_occ |> 
      CCF(bed_occ, !!as.symbol(x)) |>
      mutate(var = x) |>
      update_tsibble(key = c(site, var))
  }) |> bind_rows()

alpha_= 0.05
ci_lim <- # confidence interval (see descriptive-plot.R) 
  qnorm((1 + (1 - alpha_)) /2) / sqrt(nrow(ts_occ) / 2) #

plot_ccf <-  # plot ccf
  tbl_ccf |>
    ggplot(aes(x = lag, y = ccf, group = var)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(
      aes(yintercept = ci_lim), 
      linetype = 2, 
      colour = 'blue') +
    geom_hline(
      aes(yintercept = -ci_lim), 
      linetype = 2, 
      colour = 'blue') +
    labs(x = "lag (days)") +
    facet_wrap(
      factor(var, levels = c("adm", "dis", "bed_escal")) ~., 
      ncol = 1)
    # xlim(0, NA) # causes warning


# Save plots ------------------------------------------------------------------
save_path <- here("output/plots/predictors/")

if (!file.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
}

ls_plots <- list(
  "adm_miss" = plot_adm_miss, 
  "dis_miss" = plot_dis_miss, 
  "escal_miss" = plot_escal_miss,
  "all" = plot_together, 
  "ccf" = plot_ccf
  )

pwalk(list(ls_plots, ls_plots %>% names), \(tmp_plot, tmp_title) {
  tmp_plot %>% 
    ggsave(
      filename = paste(save_path, tmp_title, ".eps"),
      width = 35, 
      height = 20, 
      units = "cm", 
      device = cairo_ps)
})