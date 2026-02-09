#' Plot feature importance/effects
#'
#' Plot feature importance/effect for from ARIMA and random forest.
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-02-05

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
split_path <- here(paste0("output/fits/", amode, "/splits_short.RDS"))
fit_path <- here(paste0("output/fits/", amode, "/fits_short.RDS"))

split_data_cv <- readRDS(file = split_path)
fit_all <- readRDS(file = fit_path)

# Remove aggregated data
split_data_cv <-
  split_data_cv %>% filter(!is_aggregated(site)) %>% rec_site()


# Get feature importance -------------------------------------------------------
# T-statistics arima model
arima_importance <-
  as.data.table(
    select(fit_all$fable, arima_dadplt_l) |>
      mutate(site = as.character(site)) |>
      tidy()
  )[
    !grepl("days_", term),
    .(term, statistic),
    by = c("split", "site")
  ]


# Permutation-based importance (oob RMSE difference, already normalised)
rf_importance <-
  fit_all$rf_int_fit |>
  lapply(\(.x) {
    lapply(.x, \(.y) {
      as.data.table(.y$importance, keep.rownames = TRUE)[
        !grepl("days", rn),
        .(rn, "incMSE" = `%IncMSE`)
      ]
    }) |>
      rbindlist(idcol = "split")
  }) |>
  rbindlist(idcol = "site")


# Plot -------------------------------------------------------------------------
arima_imp <-
  arima_importance[grepl("ad_diff|paed|los|tvar", term)] |>
  ggplot(aes(x = term, y = statistic)) +
  ggdist::stat_interval(
    point_interval = "median_qi",
    .width = c(.10, .50, .8)
  ) +
  theme_minimal() +
  scale_color_brewer() +
  facet_wrap(vars(site), ncol = 1)


rf_imp <-
  rf_importance[grepl("lag|ad_diff|paed|los|tvar", rn)] |>
  ggplot(aes(x = rn, y = incMSE)) +
  ggdist::stat_interval(
    point_interval = "median_qi",
    .width = c(.10, .50, .8)
  ) +
  theme_minimal() +
  scale_color_brewer() +
  facet_wrap(vars(site), ncol = 1)


# Save plots -------------------------------------------------------------------
save_path_f <- here(paste0("output/plots/feature-importance/", amode, "/"))
if (!file.exists(save_path_f)) {
  dir.create(save_path_f, recursive = TRUE)
}

arima_imp |>
  ggsave(
    file = str_glue("{save_path_f}arima_importance.svg"),
    width = 15,
    height = 8.88
  )

rf_imp |>
  ggsave(
    file = str_glue("{save_path_f}rf_importance.svg"),
    width = 15,
    height = 8.88
  )
