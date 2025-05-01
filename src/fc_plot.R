#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

# Import packages ----------------------------------------------------------------
library(ggplot2)
library(scales)
library(patchwork)
library(tibble)
library(purrr)
library(dplyr)
library(fable)
import::from(here, here)
import::from(tsibble, tsibble)
source(here("src/colour-mapping.R"))
source(here("src/split-data.R"))


# Load data --------------------------------------------------------------------
split_path = here("output/fits/splits_short.RDS")
fc_path = here("output/fits/forecasts_short.RDS")
split_data_cv = readRDS(split_path)
fc_all = readRDS(fc_path)


# Generate plots ---------------------------------------------------------------
# Plot forecasts
list_models_f <- # select models for forecast plot
  c(
    "arima",
    "arima_d",
    "arima_dad",
    "arima_de",
    "arima_dade",
    "es_e",
    "mean",
    # "naive", # don't plot - ci too wide
    "snaive")


plot_forecast <- # plot forecast
  function(.data) {
    .data$all = # reduce length observations
      .data$all %>% 
      group_by(split, type, site) %>% 
      mutate(
        start_index = 
          case_when(
            type == "train" ~
              head(index, 1) + 
              (tail(index, 1) - head(index, 1)) / 1.7,
            type == "test" ~
              head(index, 1)
          )
      ) %>% 
      ungroup() %>% 
      filter(index >= start_index)
    
    .data$test %>% # plot
      autoplot() +
      autolayer(.data$all, .vars = bed_occ) +
      scale_colour_manual(name = "models", values = col_models) + 
      scale_fill_manual(name = "models", values = col_models) + 
      scale_y_continuous(breaks = c(600, 700)) +
      facet_wrap(vars(.model), ncol = 1, strip.position = "right")
  }


plt_fc <- # generate plots 
  cv_wrap(
    list("all" = split_data_cv , "fc" = fc_all), 
    select_fc,
    plot_forecast,
    list_models_f
  )


# Save plots -------------------------------------------------------------------
save_path <- here("output/plots/forecasts/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}.eps")
    plt_fc[[.site]][[.split]] %>% 
      ggsave(file = tmp_path, width = 11, height = 7)
  })
})



