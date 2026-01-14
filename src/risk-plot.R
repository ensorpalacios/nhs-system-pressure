# Plot risk predictions & metrics
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-07-02

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
path_risk <- here(paste0("output/fits/", amode, "/risk.RDS"))
path_freq <- here(paste0("output/fits/", amode, "/freq_high.RDS"))
path_curves <- here(paste0("output/fits/", amode, "/risk_curves.RDS"))
path_auc <- here(paste0("output/fits/", amode, "/risk_auc.RDS"))
path_fc <- here(paste0("output/fits/", amode, "/forecasts_short_comb.RDS"))


list_risk <- readRDS(path_risk)
list_freq <- readRDS(path_freq)
list_curves <- readRDS(path_curves)
list_auc <- readRDS(path_auc)
list_fc <- readRDS(path_fc)



# Plot risk fc (for each splits) -----------------------------------------------
risk_d <- list_risk$risk_d
risk_ws <- list_risk$risk_ws
risk_w <- list_risk$risk_w

list_models <- # select models
  c(
    "arima_dadpl_rec",
    # "var_h",
    # "arima_dad_l",
    # "arima_dad_rec",
    "crps_upper",
    "crps"
    )

splits <- risk_d$split %>% unique()
sites <- risk_d$site %>% unique()

plt_risk <- 
  map(splits, \(.split) {
    map(sites, \(.site) {
      # Prepare data
      tmp_tbl = # daily risks
        risk_d[
          split == .split & site == .site & .model %in% list_models
        ]
      
      tmp_thr = tmp_tbl[1, thr] # threshold (all equals)
      
      risk_close = # week split close risk
        risk_ws[
          split == .split & site == .site & .model %in% list_models &
          week_split == "close"
          ][ # add x-axis position
            , x_axis := 1
          ]
      risk_far = # week split far risk
        risk_ws[
          split == .split & site == .site & .model %in% list_models &
          week_split == "far"
          ][ # add x-axis position
            , x_axis := 2
          ]
      risk_weeks = # join close/far
        rbind(risk_close, risk_far)

      risk_week = # week (whole) risk
        risk_w[
          split == .split & site == .site & .model %in% list_models
          ][ # add x-axis position
            , x_axis := 3
          ]

      tmp_fc =
        list_fc %>% 
        filter(split == .split, site == .site, .model %in% list_models)
      
      # Plot
      p1 = # predicted risk weekly (split and whole)
        ggplot() +
        geom_col(
          data = risk_weeks,
          aes(x = x_axis, y = risk_ws, fill = .model),
          position = "dodge"
          ) +
        geom_col(
          data = risk_week,
          aes(x = x_axis, y = risk_w, fill = .model),
          position = "dodge"
          ) +
        geom_hline(yintercept = 0.5, color = "red", lty = "11", linewidth = 1) +
        # ylim(0, .8) +
        # scale_colour_manual(name = "models", values = col_models) + 
        scale_fill_manual(name = "models", values = col_models) +
        scale_x_continuous(
          breaks = c(1, 2, 3), labels = c("1-3", "4-7", "week")
          )
        
      p2 = # predicted risk daily
        tmp_tbl %>% 
        ggplot(aes(x = index, y = risk_day, fill = .model)) + 
        geom_col(position = "dodge", width = 0.5) +
        geom_hline(yintercept = 0.5, color = "red", lty = "11", linewidth = 1) +
        # ylim(0, .8) +
        # scale_colour_manual(name = "models", values = col_models) + 
        scale_fill_manual(name = "models", values = col_models)
        
      p3 = # time series
        tmp_fc %>% 
        autoplot() +
        geom_line(
          data = tmp_tbl[.model == tmp_tbl$.model[1]],
          aes(x = index, y = occ_obs, group = 1), linewidth = 2
        ) +
        geom_hline(
          yintercept = tmp_thr, color = "red", lty = "f8", linewidth = 2
          )
      # p3 = # time series
        # tmp_tbl[.model == tmp_tbl$.model[1]] %>% 
        # ggplot(aes(x = index, y = occ_obs, group = 1)) + 
        # geom_line(linewidth = 2) +
        # geom_hline(yintercept = tmp_thr, color = "red", lty = "f8", linewidth = 2)
      
      # Join
      p1 / p2 / p3 + 
        plot_layout(
          ncol = 1, axes = "collect_x", guides = "collect"
          )
    }) %>% 
      set_names(sites)
  }) %>% 
  set_names(splits)



# Plot curves (aggregate splits) -----------------------------------------------
pcurve_fun <- 
  function(.ax_data, .auc_data) {
    .names = names(.ax_data)
    .y = .names[ncol(.ax_data) - 1]
    .x = .names[ncol(.ax_data)]
    
    if ("week_split" %in% .names) {
      .auc_data[ # filter by top auc 
        .ax_data, on = c("nboot", "site", ".model", "week_split")][
          ,
          {
            lapply(.SD, \(.c) {
              mean = mean(.c)
              q20 = quantile(.c, 0.2)
              q80 = quantile(.c, 0.8)
              c(mean = mean, q20, q80)
            }) %>% 
              unlist(use.names = T) %>% 
              as.list()
          },
          by = c("site", ".model", "week_split", "db"),
          .SDcols = !"nboot" 
        ][
          ,
          .SD[auc.mean >= quantile(auc.mean)[["75%"]]],
          # .SD[auc >= 0],
          by = c("site", "week_split")
        ] %>% # plot
        ggplot(
          aes(
            x = .data[[sprintf("%s.mean", .x)]], 
            y = .data[[sprintf("%s.mean", .y)]],
            colour = .model, fill = .model)) +
        geom_ribbon(
          aes(
            ymin = .data[[sprintf("%s.20%%", .y)]], 
            ymax = .data[[sprintf("%s.80%%", .y)]]),
          colour = NA,
          alpha = 0.3,
        ) +
        geom_line(linewidth = 1) +
        geom_line(
          data = data.frame(x = c(0, 1), y = c(0, 1), .model = "I-line"), 
          aes(x = x, y = y)
        ) +
        scale_colour_manual(name = ".model", values = col_models) + 
        scale_fill_manual(name = ".model", values = col_models) + 
        facet_wrap(vars(site, week_split), ncol = 2)
    } else {
      .auc_data[ # filter by top auc 
        .ax_data, on = c("nboot", "site", ".model")][
          ,
          {
            lapply(.SD, \(.c) {
              mean = mean(.c)
              q20 = quantile(.c, 0.2)
              q80 = quantile(.c, 0.8)
              c(mean = mean, q20, q80)
            }) %>% 
              unlist(, use.names = T) %>% 
              as.list()
          },
          by = c("site", ".model", "db"),
          .SDcols = !"nboot"
        ][
          ,
          .SD[auc.mean >= quantile(auc.mean)[["75%"]]],
          # .SD[auc.mean >= 0],
          by = site
        ] %>% # plot
        ggplot(
          aes(
            x = .data[[sprintf("%s.mean", .x)]], 
            y = .data[[sprintf("%s.mean", .y)]],
            colour = .model, fill = .model)) +
        geom_ribbon(
          aes(
            ymin = .data[[sprintf("%s.20%%", .y)]], 
            ymax = .data[[sprintf("%s.80%%", .y)]]),
          colour = NA,
          alpha = 0.3,
        ) +
        geom_line(linewidth = 1) +
        geom_line(
          data = data.frame(x = c(0, 1), y = c(0, 1), .model = "I-line"), 
          aes(x = x, y = y)
          ) +
        scale_colour_manual(name = ".model", values = col_models) + 
        scale_fill_manual(name = ".model", values = col_models) + 
        facet_wrap(vars(site), ncol = 1) +
        theme(aspect.ratio = 1)
    }
  }
  
plt_curves <- 
  map2(list_curves, list_auc, pcurve_fun)



# Table AUC --------------------------------------------------------------------
# Generate table
tbl_auc <- 
  map(list_auc, \(.tbl){
    .groups = names(.tbl)[!grepl("nboot|auc|.model",names(.tbl))]
    tmp = # Get mean (sd) of auc
      .tbl[
        , .(.mean = mean(auc), .sd = sd(auc)), by = c(.groups, ".model")
      ][
        order(-.mean, .sd), .SD, by = .groups
      ][
        , `mean (sd)` := sprintf("%.3f (%.3f)", .mean, .sd)
      ][
        , c(".mean", ".sd") := NULL
      ] 
    tmp %>% setorderv(.groups) # reorder by group
    
    if ("week_split" %in% .groups) { # prepare for table
      dcast(tmp, ... ~ week_split, value.var = "mean (sd)")
    }
    
    tmp %>% # generate table
      gt(
        rowname_col = ".model", groupname_col = "site", row_group_as_column = T
        )  
  })


# Get lighter colour palette and add to table
col_models_l <- bright_col(col_models, 0.5)

tbl_auc <-  # add raw colours by model
  map(tbl_auc, \(.tbl) {
  for (i in seq_along(col_models_l)) {
    .color = col_models_l[i]
    .models = names(col_models_l)[i]
    .tbl =
      .tbl %>% 
      tab_style(
        style = cell_fill(.color),
        location = cells_stub(rows = .model == .models)
      ) %>%
      tab_style(
        style = cell_fill(.color),
        location = cells_body(rows = .model == .models)
      )
  }
  .tbl
})



# Table frequency high ---------------------------------------------------------
tbl_freq <-
  rbind(
    list_freq$freq_d %>%
      pivot_wider(names_from = "site", values_from = "Prevalence") |>
      mutate(`time horizon` = as.character(`time horizon`)),
    list_freq$freq_ws %>%
      pivot_wider(names_from = "site", values_from = "Prevalence") |>
      mutate(
        `time horizon` = if_else(
          `time horizon` == "close",
          "1-3 days",
          "4-7 days"
        )
      ),
    list_freq$freq_w %>%
      pivot_wider(names_from = "site", values_from = "Prevalence") |>
      mutate(`time horizon` = "week")
  ) |>
  gt(rowname_col = "time horizon") %>%
  tab_stubhead(label = "Time horizon") %>%
  tab_spanner(
    label = "Prevalence threshold crossing",
    columns = c("BRI", "Southmead")
  )



# Save plots -------------------------------------------------------------------
save_path <- here(paste0("output/plots/risk_fc/" , amode, "/"))

if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

# Single fc
walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}.svg")
    plt_risk %>% pluck(.split, .site) %>% 
      ggsave(file = tmp_path, width = 6, height = 3)
  })
})


# Aggregated measures
iwalk(plt_curves, \(.plt, .name) {
  tmp_path = paste0(save_path, .name, ".svg")
  plt_curves[[.name]] %>% 
    ggsave(file = tmp_path, width = 6, height = 5)
  
})


# Table auc
iwalk(tbl_auc, \(.tbl, .name) {
  file_name = str_glue("{save_path}table_{.name}")
  html2pdf(.tbl, file_name)
})


# Table freq
iwalk(tbl_freq, \(.tbl, .name) {
  file_name = str_glue("{save_path}{.name}.tex")
  gtsave(.tbl, file_name)
})
