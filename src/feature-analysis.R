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
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
split_cv_path <- here(paste0("output/fits/", amode, "/splits_short.RDS"))
fit_path <- here(paste0("output/fits/", amode, "/fits_short.RDS"))

ts_occ <- readRDS(file = data_path)
split_data_cv <- readRDS(file = split_cv_path)
fit_all <- readRDS(file = fit_path)

# Remove aggregated data
ts_occ <-
  ts_occ %>% filter(!is_aggregated(site)) %>% rec_site()
split_data_cv <-
  split_data_cv %>% filter(!is_aggregated(site)) %>% rec_site()



# Feature dependence -----------------------------------------------------------
# Data wrangling (as in fit-fc.R without lag expansion and splits)
ts_occ <-
  ts_occ %>%
  select(
    # exclude
    -(ts_occ %>% names %>% grep("_m", .)), # original data with missing values
    -occ_wx,
    -adm,
    -dis,
    -ad_diff_original,
    -escal,
    -core,
    -ad_diff,
    -ad_diff2,
    -ad_diff3,
    -ad_diff3_f
  ) |>
  factorise_temp(0) |> # 0 lag
  as.data.table()

ts_occ <- ts_occ[, -c("index", "occ_other", "t_ax")]
var_names <- ts_occ[, names(.SD), .SDcols = !("site")]
n_var <- ts_occ |> length() - 1
sites <- ts_occ[, unique(site)]
ts_occ[, c(2, 3)]
ts_occ_split <- split(ts_occ, by="site")

plot_feat_dep <- # not best way (does not really create 7x7 patchwork)
  lapply(sites, \(.site) {
    lapply(seq_len(n_var), \(.col) {
      lapply(seq_len(n_var), \(.row) {
        tmp_ts = ts_occ_split[[.site]]
        tmp_tbl =
          tmp_ts[, -c("site")][, c(...col, ...row)]
        tmp_names = tmp_tbl |> names()
        tmp_cclass = class(tmp_tbl[[1]])
        tmp_rclass = class(tmp_tbl[[2]])

        if (.col > .row) {
          tmp_plot =
            tmp_tbl |>
            ggplot(aes(x = !!sym(tmp_names[1]), y = !!sym(tmp_names[2]))) +
            labs(x = tmp_names[[1]], y = tmp_names[[2]]) +
            theme_minimal()
          if (tmp_cclass == "numeric" & tmp_rclass == "numeric") {
            tmp_plot +
              geom_point() +
              geom_smooth(method = "lm", formula = y ~ x) +
              theme(aspect.ratio = 1)
          } else if (tmp_cclass == "factor" & tmp_rclass == "factor") {
            tmp_plot +
              geom_col(aes(fill = !!sym(tmp_names[[2]]))) +
              theme(
                legend.position = "none",
                # legend.position.inside = c(0.9, 0.9),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = 90)
              )
          } else {
            tmp_plot +
              geom_boxplot(outliers = FALSE) +
              theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90))
          }
        } else if (.col < .row) {
          tmp_tbl |>
            ggplot(aes(x = !!sym(tmp_names[1]), y = !!sym(tmp_names[2]))) +
            labs(x = tmp_names[[1]], y = tmp_names[[2]]) +
            theme_minimal()
        } else {
          tmp_plot =
            tmp_tbl[, 1] |>
            ggplot() +
            labs(x = tmp_names[[1]], y = tmp_names[[2]]) + 
            theme_minimal()

          if (tmp_cclass == "numeric") {
            tmp_plot +
              ggdist::stat_slab(aes(x = !!sym(tmp_names[1])))
          } else {
            tmp_plot +
              geom_bar(aes(x = !!sym(tmp_names[1]))) +
              theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90))
          }
        }
      }) |>
        wrap_plots(ncol = 1)
    }) |>
      wrap_plots(nrow = 1)
  }) |>
  set_names(sites)



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
  rf_importance[grepl("occ|lag|ad_diff|paed|los|tvar", rn)] |>
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

walk(sites, \(.site) {
  plot_feat_dep[[.site]] |> 
    ggsave(
    file = str_glue("{save_path_f}fit_dependence_{.site}.svg"),
    width = 15,
    height = 8.88
    )
})