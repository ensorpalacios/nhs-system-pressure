#' Plot descriptive data
#'
#' Generate plot to describe bed occupancy: data, trend, seasonality,
#' stationary process, correlation function.
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles and Practice
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-01-08

# Shebang ---------------------------------------------------------------------
# !/usr/loca/bin/Rscript

# Import libraries ------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(patchwork)
# library(ggrain)
library(fable)
library(feasts)
# library(kable)
library(knitr)
library(magick)
library(kableExtra)


# Load data -------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/descriptive_analysis.RDS")
ls_des <- readRDS(file = data_path)
list2env(ls_des, env = .GlobalEnv)

# Helper functions ------------------------------------------------------------
# ACF/PCF plot function
plot_acf = function(ts_tbl, ...){
  # Default/set other argument values (from ...)
  lag_ = 50
  alpha_= 0.05
  list2env(list(...)[[1]], env = environment())

  # Compute acf and pacf
  tmp_acf = ts_tbl |> ACF(!!as.symbol(label), lag_max = lag_)
  corfun = "acf"
  tmp_pacf = ts_tbl |> PACF(!!as.symbol(label), lag_max = lag_)
  corfun = "pacf"

  # Confidence interval
  ci_lim = qnorm((1 + (1 - alpha_)) /2) / sqrt(nrow(ts_tbl) / 2)

  # Generate plot
  plt_acf = tmp_acf |>
    ggplot(aes(x = lag, y = acf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
    geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
    facet_wrap(
      ~ site,
      nrow = 2,
      scales = "free_y") +
    labs(x = "lag (days)")
  plt_pacf = tmp_pacf |>
    ggplot(aes(x = lag, y = pacf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
    geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
    facet_wrap(
      ~ site,
      nrow = 2,
      scales = "free_y") +
    labs(x = "lag (days)")
  plt_acf + plt_pacf + plot_layout(axis_title="collect")
}

# CCF plot function (BRI-Southmead)
plot_ccf = function(ts_tbl, ...){
  # Default/set other argument values
  lag_ = 50
  alpha_= 0.05
  list2env(list(...)[[1]], env = environment())

  # Compute ccf
  tmp_ccf = ts_tbl |> 
    pivot_wider(id_cols = index, names_from= site, values_from = !!as.symbol(label)) |> 
    CCF(BRI, Southmead, lag_max = lag_)

  # Confidence interval
  ci_lim = qnorm((1 + (1 - alpha_)) /2) / sqrt(nrow(ts_tbl) / 2)

  # Generate plot (! positive values means BRI lags behind Southmead)
  plt_ccf = tmp_ccf |>
    ggplot(aes(x = lag, y = ccf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ci_lim), linetype = 2, colour = 'blue') +
    geom_hline(aes(yintercept = -ci_lim), linetype = 2, colour = 'blue') +
    labs(x = "lag (days)") +
    facet_grid(. ~"BRI - Southmead")
}

# Wrapper on map(plot_function)
plot_wrapper <- function(list_data, plot_fun, ...) {
  var_arg= list(...)
  list_plot = map(list_data, ~plot_fun(.x, var_arg))
}


# Plot bed occupancy ----------------------------------------------------------
# Raw data
plot_occ_fun <- function(data_occ, ...) {
  data_occ |> 
    ggplot(aes( x = index, y = bed_occ, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_occ <- plot_wrapper(ts_occ, plot_occ_fun)

# ACF/CCF
plot_occ_acf_200 = plot_wrapper(ts_occ, plot_acf, label = "bed_occ", lag_ = 200)
plot_occ_acf = plot_wrapper(ts_occ, plot_acf, label = "bed_occ")
plot_occ_ccf = plot_wrapper(ts_occ, plot_ccf, label = "bed_occ")

# Scatterplot matrix of lagged values
provider <- c("provider_level", "frontier")
sites <- c("BRI", "Southmead")

plot_lag <- map(provider, \(prov) {
  map(sites, \(x) {
    tmp_plot = ts_occ[[prov]] |>
      filter(site == x) |>
      gg_lag(
        bed_occ,
        geom = "point",
        size = 2
      )
  }) |> set_names(sites)
}) |> set_names(provider)

plot_lag7 <- map(provider, \(prov) {
  map(sites, \(site_) {
    tmp_plot = ts_occ[[prov]] |>
      filter(site == site_) |>
      gg_lag(
        bed_occ,
        geom = "point",
        lags = seq(7, 42, by=7),
        size = 2
      )
  }) |> set_names(sites)
}) |> set_names(provider)


# Weekly seasonality (z-scored data)
plot_seasonality_fun <- function(data_occ, ...) {
  data_occ |> gg_season(bed_occ_z, period = 7) +
    data_occ |> gg_subseries(y=bed_occ_z, period = 7) + 
    plot_layout(axis_titles = "collect")
}
plot_seasonality <- plot_wrapper(ts_occ, plot_seasonality_fun)

# Plot STL decomposition ------------------------------------------------------
# STL decomposition (trend, seasonality, stationary process)
plot_stl_fun <- function(data_stl, ...) {
  tmp_plot <- data_stl |> 
    names() |>
    tail(-4) |>
    head(-1) |> 
    map(\(x) {
      data_stl |> 
        ggplot(aes(x = index, colour = site)) +
        geom_line(
          aes(y = !!as.symbol(x)),
          show.legend = FALSE) +
        facet_wrap(~site, nrow = 2, scales = "free_y") +
        theme(
          legend.position="none",
          axis.title.x = element_blank()
        )
    })
  tmp_plot <- tmp_plot[[1]] + 
    tmp_plot[[2]] + 
    tmp_plot[[3]] + 
    plot_layout(ncol = 1, axis_title="collect")
}
plot_stl <- plot_wrapper(stl_occ, plot_stl_fun)

# ACF-CCF (BRI-Southmead)
plot_stl_res_acf = plot_wrapper(stl_occ, plot_acf, label = "remainder")
plot_stl_res_ccf = plot_wrapper(stl_occ, plot_ccf, label = "remainder")
plot_stl_deseasoned_acf = plot_wrapper(stl_occ, plot_acf, label = "season_adjust")
plot_stl_deseasoned_ccf = plot_wrapper(stl_occ, plot_ccf, label = "season_adjust")
plot_stl_detrended_acf = plot_wrapper(stl_occ, plot_acf, label = "trend_adjust")
plot_stl_detrended_ccf = plot_wrapper(stl_occ, plot_ccf, label = "trend_adjust")


# Plot trends -----------------------------------------------------------------
# Weekly mooving average
plot_ma_fun <- function(data_ma, ...) {
  data_ma |> 
    ggplot(aes(x = index, y = bed_occ, colour=site)) +
    geom_line() +
    geom_line(aes(y = `7-ma`), linewidth = 1) + 
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_ma <- plot_wrapper(trend_ma, plot_ma_fun)
plot_ma_acf = plot_wrapper(trend_ma, plot_acf, label = "7-ma")

# Weekly mooving average - residuals
plot_ma_res_fun <- function(data_ma, ...) {
  data_ma |> 
    ggplot(aes(x = index, y = residuals, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "residuals bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_ma_res <- plot_wrapper(trend_ma, plot_ma_res_fun)
plot_ma_res_acf = plot_wrapper(trend_ma, plot_acf, label = "residuals")

# Lowess fit
plot_lowess_fun <- function(data_lowess, ...) {
  data_lowess |> 
    ggplot(aes(x = index, y = bed_occ, colour=site)) +
    geom_line() +
    geom_line(aes(y = lowess, linetype = "lowess"), linewidth = 1) + 
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    theme(
      legend.position="none",
      axis.title.x = element_blank())
}
plot_lowess <- plot_wrapper(trend_lowess, plot_lowess_fun)
plot_lowess_acf = plot_wrapper(trend_lowess, plot_acf, label = "lowess")

# Lowess fit - residuals
plot_lowess_res_fun <- function(data_lowess, ...) {
  data_lowess |> 
    ggplot(aes(x = index, y = residuals, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    theme(
      legend.position="none",
      axis.title.x = element_blank())
}
plot_lowess_res <- plot_wrapper(trend_lowess, plot_lowess_res_fun)
plot_lowess_res_acf = plot_wrapper(trend_lowess, plot_acf, label = "residuals")

# Polynomial fit (2nd order)
plot_tpolyfit_fun <- function(data_poly, ...) {
  data_poly |> 
    ggplot(aes(x = index, y = bed_occ, colour=site)) +
    geom_line() +
    geom_line(aes(y = fit)) +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_tpolyfit <- plot_wrapper(trend_poly, plot_tpolyfit_fun)
plot_tpolyfit_acf = plot_wrapper(trend_poly, plot_acf, label = "fit")

# Polynomial fit (2nd order) - residuals
plot_tpoly_res_fun <- function(data_poly, ...) {
  data_poly |> 
    ggplot(aes(x = index, y = res, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_tpoly_res <- plot_wrapper(trend_poly, plot_tpoly_res_fun)
plot_tpoly_res_acf = plot_wrapper(trend_poly, plot_acf, label = "res")


# Plot seasonality ------------------------------------------------------------
# Weekly linear regression
plot_week_fit_fun <- function(data_week, ...) {
  data_week |> 
    ggplot(aes(x = index, y = bed_occ, colour=site)) +
    geom_line() +
    geom_line(aes(y = fit)) +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_week_fit <- plot_wrapper(season_week, plot_week_fit_fun)
plot_week_fit_acf = plot_wrapper(season_week, plot_acf, label = "fit")

# Weekly linear regression - residuals
plot_week_res_fun <- function(data_week, ...) {
  data_week |> 
    ggplot(aes(x = index, y = res, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y = "bed occupancy") +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_week_res <- plot_wrapper(season_week, plot_week_res_fun)
plot_week_res_acf = plot_wrapper(season_week, plot_acf, label = "res")
plot_week_res_ccf = plot_wrapper(season_week, plot_ccf, label = "res")


# Plot differencing -----------------------------------------------------------
# First (trend) difference - 1st order
plot_tdiff_fun <- function(data_diff, ...) {
  data_diff |> 
    ggplot(aes(x = index, y = diff, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y =bquote(1^st~order~"trend-differenced"~occupancy)) +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_tdiff  <- plot_wrapper(trend_diff, plot_tdiff_fun)
plot_tdiff_acf = plot_wrapper(trend_diff, plot_acf, label = "diff")

# Seasonal difference - 1st order
plot_sdiff_fun <- function(data_diff, ...) {
  data_diff |> 
    ggplot(aes(x = index, y = diff, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, 
      scales = "free_y") +
    labs(y =bquote(1^st~order~"season-differenced"~occupancy)) +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_sdiff <- plot_wrapper(season_diff, plot_sdiff_fun)
plot_sdiff_acf <- plot_wrapper(season_diff, plot_acf, label = "diff")
plot_sdiff_ccf <- plot_wrapper(season_diff, plot_ccf, label = "diff")

# Twice-differenced data (first and seasonal)
plot_2diff_fun <- function(data_diff, ...) {
  data_diff |> 
    ggplot(aes(x = index, y = diff, colour=site)) +
    geom_line() +
    facet_wrap(
      ~site,
      nrow = 2, scales = "free_y") +
    labs(y =bquote(2^nd~"differenced"~occupancy)) +
    theme(
      legend.position="none",
      axis.title.x = element_blank()
    )
}
plot_2diff <- plot_wrapper(second_diff, plot_2diff_fun)
plot_2diff_acf <- plot_wrapper(second_diff, plot_acf, label = "diff")
plot_2diff_ccf <- plot_wrapper(second_diff, plot_ccf, label = "diff")


# Save plots ------------------------------------------------------------------
save_path <- here("output/plots/descriptive/")

for (prov in c("provider_level", "frontier")) {
  tmp_path = paste0(save_path, prov)
  if (!file.exists(tmp_path)) {
    dir.create(tmp_path, recursive = TRUE)
  }
}

ls_plot <- list(
  "plot_occ" = plot_occ,
  "plot_occ_acf" = plot_occ_acf,
  "plot_occ_acf_200" = plot_occ_acf_200,
  "plot_occ_ccf" = plot_occ_ccf,
  "plot_lag" = plot_lag,
  "plot_lag7" = plot_lag7,
  "plot_seasonality" = plot_seasonality,
  "plot_stl" = plot_stl,
  "plot_stl_res_acf" = plot_stl_res_acf,
  "plot_stl_res_ccf" = plot_stl_res_ccf,
  "plot_stl_deseasoned_acf" = plot_stl_deseasoned_acf,
  "plot_stl_deseasoned_ccf" = plot_stl_deseasoned_ccf,
  "plot_stl_detrended_acf" = plot_stl_detrended_acf,
  "plot_stl_detrended_ccf" = plot_stl_detrended_ccf,
  "plot_ma" = plot_ma,
  "plot_ma_acf" = plot_ma_acf,
  "plot_ma_res" = plot_ma_res,
  "plot_ma_res_acf" = plot_ma_res_acf,
  "plot_lowess" = plot_lowess,
  "plot_lowess_acf" = plot_lowess_acf,
  "plot_lowess_res" = plot_lowess_res,
  "plot_lowess_res_acf" = plot_lowess_res_acf,
  "plot_tpolyfit" = plot_tpolyfit,
  "plot_tpolyfit_acf" = plot_tpolyfit_acf,
  "plot_tpoly_res" = plot_tpoly_res,
  "plot_tpoly_res_acf" = plot_tpoly_res_acf,
  "plot_week_fit" = plot_week_fit,
  "plot_week_fit_acf" = plot_week_fit_acf,
  "plot_week_res" = plot_week_res,
  "plot_week_res_acf" = plot_week_res_acf,
  "plot_week_res_ccf" = plot_week_res_ccf,
  "plot_tdiff" = plot_tdiff,
  "plot_tdiff_acf" = plot_tdiff_acf,
  "plot_sdiff" = plot_sdiff,
  "plot_sdiff_acf" = plot_sdiff_acf,
  "plot_sdiff_ccf" = plot_sdiff_ccf,
  "plot_2diff" = plot_2diff,
  "plot_2diff_acf" = plot_2diff_acf
)

iwalk(ls_plot, \(x, y) {
  for (prov in c("provider_level", "frontier")) {
    if (grepl("plot_lag", y)) {
      walk(sites, \(site_) {
        tmp_path = paste0(save_path, prov, "/", y, "_", site_, ".eps")
        ggsave(
          x[[prov]][[site_]], 
          file = tmp_path, 
          width = 35, 
          height = 20, 
          units = "cm", 
          device = "eps")
      })
    }
    else {
      tmp_path = paste0(save_path, prov, "/", y, ".eps")
      ggsave(
        x[[prov]], 
        file = tmp_path, 
        width = 35, 
        height = 20, 
        units = "cm", 
        device = "eps")
    }
  }
})


# Save table of features ------------------------------------------------------
loop_over <- expand.grid(
  tbl_format = c("html","latex"), 
  prov = c("provider_level","frontier"), 
  stringsAsFactors = FALSE)

pwalk(loop_over, \(tbl_format, prov) {
  tmp_tbl = data.table::transpose(
    ts_features[[prov]], keep.names="var", make.names = "site"
  ) |>
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  if (tbl_format == "html") {
    tmp_tbl |> 
      kable(tbl_format) |>
      save_kable(
        file = paste0(save_path, prov, "/tbl_features.", tbl_format))
  } else {
    tmp_tbl |> 
      xtable() |>
      print(
        file = paste0( save_path, prov, "/tbl_features.", tbl_format)
      )
  }
})
