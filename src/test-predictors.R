#' Test predictors
#'
#' Fit an ARIMA model with different predictors to time series of variable
#' length; compare standard errors of the estimated coefficients as length of 
#' training set varies; predictors include days of the week and bed escalation.
#' 
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-04-23

# Import packages --------------------------------------------------------------
library(conflicted)
library(data.table)
library(tidyverse)
library(here)
library(fable)
library(tsibble)

source("src/functions.R")


# Manage conflicts
conflicts_prefer(
  dplyr::filter,
)

# Load data --------------------------------------------------------------------
data_path <- paste0(here(), "/data/processed/bed_occupancy.RDS")
ts_occ <- readRDS(file = data_path)
ts_occ <- # drop original data with missing values
  ts_occ %>% select(-(ts_occ %>% names %>% grep("_m", .)))
sites <- ts_occ$site |> unique()


# Split data -------------------------------------------------------------------
# Train/test sets
split_data_tt <-
  split_tt(ts_occ)

# Cv train/validation sets
initial <- # list of training set lengths
  map(seq(6, 20), \(.n) paste(.n, "weeks"))
assess <- "2 weeks"
skip <- "8 weeks"
split_data_cv <-
  map(initial, \(.initial) {
    split_cv(split_data_tt, .initial, assess, skip)
  }) %>%
  set_names(initial)

# Force equal number of training splits across cv sets
ls_split <- # common splits
  split_data_cv %>% 
  pluck(-1) %>% # (from set with longest .initial)
  select(split) %>% 
  unique() %>% 
  .[[1]]

split_data_cv_equal <- # drop additional splits
  map(initial, \(.initial) {
    ls_split %>% str
    split_data_cv %>% 
      pluck(.initial) %>% 
      filter(split %in% ls_split)
  }) %>% 
  set_names(initial)


# Coefficients -----------------------------------------------------------------
# Fit/load fits (load if already exists)
fit_path = here("output/fits/test_predictors_fit.RDS")
if (file.exists(fit_path)) {
  fit_arima <- readRDS(fit_path) 
} else {
  fit_arima <-
    map(initial, \(.initial) {
      split_data_cv_equal %>% 
        pluck(.initial) %>% 
        filter(type == "train") %>% 
        tsibble(index = index, key = c(split, site)) %>%
        model(
          arima_d = ARIMA(bed_occ ~ days_),
          arima_e = ARIMA(bed_occ ~ bed_escal),
        )
    }) %>% set_names(initial)
}

# Extract coefficients and std errors
coef_arima <- 
  map(initial, \(.initial) {
    fit_arima %>% 
      pluck(.initial) %>% 
      coef()
  }) %>% set_names(initial) %>% 
  bind_rows(.id = "length_training") %>% 
  mutate(
    length_training = fct_relevel(length_training, 
                                  length_training %>% unique())
  ) %>% 
  drop_na()

coef_arima_d <- # coef days
  coef_arima %>%
  filter(grepl("days_", term)) %>% 
  group_by(length_training, site, .model, term) %>%
  summarise(
    "estimate" = mean(estimate),
    "std.error" = mean(std.error), 
    .groups = "drop")

coef_arima_e <- # coef escalation
  coef_arima %>% 
  filter(grepl("bed_escal", term)) %>% 
  group_by(length_training, site, .model, term) %>%
  summarise(
    "estimate" = mean(estimate),
    "std.error" = mean(std.error),
    .groups = "drop")


# Plot -------------------------------------------------------------------------
estimate_plot_d <- 
  ggplot() +
  geom_line(
    data = coef_arima_d,
    aes(y = estimate, x = length_training, colour = term, group = term)) +
  stat_summary(
    data = coef_arima %>% filter(grepl("days_", term)),
    aes(y = estimate, x = length_training, colour = term, group = term)) +
  facet_wrap(vars(site), ncol = 1)

stderr_plot_d <- 
  ggplot() +
  geom_line(
    data = coef_arima_d,
    aes(y = std.error, x = length_training, colour = term, group = term)) +
  stat_summary(
    data = coef_arima %>% filter(grepl("days_", term)),
    aes(y = std.error, x = length_training, colour = term, group = term)) +
  facet_wrap(vars(site), ncol = 1)

estimate_plot_e <- 
  ggplot() +
  geom_line(
    data = coef_arima_e,
    aes(y = estimate, x = length_training, colour = term, group = term)) +
  stat_summary(
    data = coef_arima %>% filter(grepl("bed_escal", term)),
    aes(y = estimate, x = length_training, colour = term, group = term)) +
  facet_wrap(vars(site), ncol = 1)

stderr_plot_e <- 
  ggplot() +
  geom_line(
    data = coef_arima_e,
    aes(y = std.error, x = length_training, colour = term, group = term)) +
  stat_summary(
    data = coef_arima %>% filter(grepl("bed_escal", term)),
    aes(y = std.error, x = length_training, colour = term, group = term)) +
  facet_wrap(vars(site), ncol = 1)


# Save output ------------------------------------------------------------------
# Fits if not exist
if (!file.exists(fit_path)) {
  dir.create(fit_path, recursive = TRUE)
  saverds(fit_arima, file = fit_path)
}

# Plots
plot_path <- here("output/plots/test_predictors/")
if (!file.exists(plot_path)) {
    dir.create(plot_path, recursive = TRUE)
}

estimate_plot_d %>% 
    ggsave(
      file = paste0(plot_path, "estimate_days.eps"),
      width = 20, height = 11.85
    )
stderr_plot_d %>% 
    ggsave(
      file = paste0(plot_path, "stderr_d.eps"),
      width = 20, height = 11.85
    )
estimate_plot_e %>% 
    ggsave(
      file = paste0(plot_path, "estimate_bedescal.eps"),
      width = 20, height = 11.85
    )
stderr_plot_e %>% 
    ggsave(
      file = paste0(plot_path, "stderr_bedescal.eps"),
      width = 20, height = 11.85
    )
