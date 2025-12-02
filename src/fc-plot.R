#' Plot forecasts (level and trend)
#'
#' Generate plots of 1-week forecasts generated from models listed in
#'  fit-models-short.R. Generating plots for both occupancy level (fast changes)
#'  and trend (smoothed occupancy). For level forecasts, including fc from 
#'  combined models - run after metrics-compute.R.
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-01

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



# Load/prepare data ------------------------------------------------------------
fc_path <- here(paste0("output/fits/", amode, "/forecasts_short_comb.RDS"))
split_path <- here(paste0("output/fits/", amode, "/splits_short.RDS"))
thr_path <- here("output/fits/thresholds.RDS")


fc_all <- readRDS(fc_path)
split_data_cv <- readRDS(split_path)
alarm_thr <- readRDS(thr_path)


# Recode sites
fc_all <-
  fc_all %>% rec_site()
split_data_cv <-
  split_data_cv %>% rec_site() %>% filter(site != "aggregate")



# Generate plots ---------------------------------------------------------------
# Plot level forecasts
list_models_f <- # select models for forecast plot
  c(
    "tslm",
    "var_paed",
    "var_h",
    "arima_dad_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "nn",
    # "es",
    # "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "crps",
    "crps_upper",
    "equal"
    # "wilker"
    )


fc_all <- 
  fc_all %>% filter(.model %in% list_models_f)


plt_fc_level <- # generate plots 
  cv_wrap(
    list("all" = split_data_cv, "fc" = fc_all), 
    select_fc,
    plot_forecast,
    list_models_f,
    trend = FALSE
  )


# Add alarm threshold
sites <- alarm_thr[, site]
plt_fc_level <- 
  map(sites, \(.site){
    map(plt_fc_level[[.site]], 
        ~ .x + geom_hline(yintercept = alarm_thr[site == .site, thr], lty = 2)
    )
  }) %>% set_names(sites)



# Save plots -------------------------------------------------------------------
save_path <- here(paste0("output/plots/forecasts/", amode, "/"))

if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

sites = fc_all$site %>% unique() %>% as.character()
splits = fc_all$split %>% unique() %>% as.character()

walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}")
    plt_fc_level[[.site]][[.split]] %>%
      ggsave(file = paste0(tmp_path, "_level.svg"), width = 9, height = 5)
  })
})

