#' Plot forecasts
#'
#' Generate plots of 2-week forecasts generated from baseline, ARIMA and 
#' exponential smoothing models with predictors. Fits and forecasts generated 
#' from fit-models-short.R.
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-01

# Import packages ----------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")
source("src/colour-mapping.R")


# Load data --------------------------------------------------------------------
split_path <- here("output/fits/splits_short.RDS")
fc_path <- here("output/fits/forecasts_short_comb.RDS")

split_data_cv <- readRDS(split_path)
fc_all <- readRDS(fc_path)

# Recode sites
split_data_cv <- 
  split_data_cv %>% rec_site()
fc_all <-
  fc_all %>% rec_site()



# Generate plots ---------------------------------------------------------------
# Plot forecasts
list_models_f <- # select models for forecast plot
  c(
    "tslm",
    "var_ad2",
    "var_h",
    "arima_dad_l",
    "arima_dad_rec",
    "nn",
    "es",
    "rf",
    "rf_int",
    "xgb",
    "crps",
    "equal",
    "wilker"
    )


fc_all <- 
  fc_all %>% filter(.model %in% list_models_f)


plot_forecast <- # plot forecast function
  function(.data) {
    .data$all = # reduce length observations (x axis)
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
      autolayer(.data$all, .vars = occ) +
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

sites = fc_all$site %>% unique() %>% as.character()
splits = fc_all$split %>% unique() %>% as.character()

walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}.eps")
    plt_fc[[.site]][[.split]] %>% 
      ggsave(file = tmp_path, width = 11, height = 7)
  })
})
