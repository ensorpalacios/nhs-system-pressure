#' Predict risk high system pressure
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-07-02

# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")
source("src/colour-mapping.R")


# Load data --------------------------------------------------------------------
# Original ts, train/test split, cv split, fc (with combined models)
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
tt_path <- here("output/fits/tt_split.RDS")
split_path <- here("output/fits/splits_short.RDS")
fc_path <- here("output/fits/forecasts_short_comb.RDS")

ts_occ <- readRDS(file = data_path)
split_data_tt <- readRDS(file = tt_path)
split_data_cv <- readRDS(split_path)
fc_all <- readRDS(fc_path)


# Recode sites
ts_occ <- 
  ts_occ %>% rec_site()
split_data_tt <- 
  split_data_tt %>% rec_site()
split_data_cv <- 
  split_data_cv %>% rec_site()
fc_all <-
  fc_all %>% rec_site()



# Compute alarm threshold ------------------------------------------------------
alarm_thr <- # compute threshold on (all) training data
  split_data_tt %>% as.data.table() %>%
  .[
    type == "train" & site != "aggregate",
    .(thr = quantile(occ, probs = 0.9)), by = site
    ]

fc_threshold <- # fc table with thresholds
  alarm_thr[
    fc_all %>% select(split, site, .model, index, occ, .mean, h),  on = "site"
    ]

fc_threshold <- # Join observed and fc occ
  split_data_cv %>% # observed
  filter(type == "test", site != "aggregate") %>% 
  select(split, site, index, occ) %>% 
  rename(occ_obs = occ) %>% 
  as.data.table() %>% 
  .[fc_threshold, on = c("split", "site", "index")] # fc
  
  
# alarm_thr <- # Compute from whole training set
#   split_data_tt %>%
#   filter(type == "train") %>% 
#   group_by(site) %>% 
#   summarise(threshold = quantile(occ, probs = 0.9))



# Compute threshold-crossing probabilities -------------------------------------
risk_h <- # risk by h
  copy(fc_threshold)[ 
  , risk_day := 1 - cdf(occ, thr), by = .(split, site, .model, h)
  ]

risk_h[ # add categorise h
  , week_split := ifelse(h <= 3, "close", "far")
  ]

risk_ws <- 
  risk_h[ # risk by week_split (1-3h, 4-7h)
  , .(index, risk_ws = 1 - prod(1 - risk_day)), 
  by = .(split, site, .model, week_split) 
  ] %>% 
  .[ # prepare for plotting
    , .(risk_ws = paste(.model[1], week_split[1], ":", round(risk_ws[1], 3))), 
    by = .(split, site, .model, week_split)
  ]
  

risk_w <- 
  risk_h[ # risk by week
  , .(index, risk_w = 1 - prod(1 - risk_day)), by = .(split, site, .model)
  ] %>% 
  .[ # prepare for plotting
    , .(risk_w = paste(.model[1], ":", round(risk_w[1], 3))), 
    by = .(split, site, .model)
  ]



# Plot risk --------------------------------------------------------------------
list_models <- # select models
  c(
    "crps",
    "equal",
    "wilker",
    "tslm")

splits <- risk_h$split %>% unique()
sites <- risk_h$site %>% unique()
plt_risk <- 
  map(splits, \(.split) {
    map(sites, \(.site) {
      # Prepare data
      tmp_tb = risk_h[split == .split & site == .site & .model %in% list_models]
      tmp_tb[, index := as.character(index)] %>% 
        .[, index := factor(index, ordered = TRUE)] 
      tmp_thr = tmp_tb[1, thr]
      
      x_close = tmp_tb[2, index]
      risk_close = 
        risk_ws[ # filter risk close
          split == .split & site == .site & .model %in% list_models &
          week_split == "close", risk_ws
          ] %>% 
        paste(collapse = "\n")
      
      x_far = tmp_tb[6, index]
      risk_far = 
        risk_ws[ # filter risk close
          split == .split & site == .site & .model %in% list_models &
          week_split == "far", risk_ws
          ] %>% 
        paste(collapse = "\n")
      
      x_week = tmp_tb[4, index]
      risk_week =
        risk_w[ # filter risk close
          split == .split & site == .site & .model %in% list_models, risk_w
          ] %>% 
        paste(collapse = "\n") 
      
      # Plot
      p1 =
        tmp_tb %>% 
        ggplot(aes(x = index, y = risk_day, fill = .model)) + 
        geom_col(position = "dodge", width = 0.5) +
        geom_hline(yintercept = 0.5, color = "red", lty = "11", linewidth = 1) +
        ylim(0, 1) +
        annotate("text", x = x_close, y = 0.6, label = risk_close) +
        annotate("text", x = x_far, y = 0.6, label = risk_far) +
        annotate("text", x = x_week, y = 0.8, label = risk_week)
        
      p2 = 
        tmp_tb[.model == tmp_tb$.model[1]] %>% 
        ggplot(aes(x = index, y = occ_obs, group = 1)) + 
        geom_line(linewidth = 2) +
        geom_hline(yintercept = tmp_thr, color = "red", lty = "f8", linewidth = 2)
      
      p1 / p2 + 
        plot_layout(ncol = 1, height = c(3, 1), guides = "collect") & 
        theme_minimal()
        theme(plot.tag = element_text(size = 10))
        # theme(axis.text.x = element_text(), axis.title.x = element_text())
      
      p1 / p2 + 
        plot_layout(
          axes = "collect_x", guides = "collect", height = c(3, 1)
        )
    }) %>% 
      set_names(sites)
  }) %>% 
  set_names(splits)



# Save plots -------------------------------------------------------------------
save_path <- here("output/plots/risk_fc/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

walk(sites, \(.site) {
  walk(splits, \(.split) {
    tmp_path = str_glue("{save_path}{.site}_split{.split}.png")
    plt_risk %>% pluck(.split, .site) %>% 
      ggsave(file = tmp_path, width = 6, height = 3)
  })
})
