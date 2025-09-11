#' Predict risk high system pressure
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-07-02

# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/split-data.R")


# Load data --------------------------------------------------------------------
# Original ts, train/test split, cv split, fc (with combined models)
split_path <- here("output/fits/splits_short.RDS")
fc_path <- here("output/fits/forecasts_short_comb.RDS")
thr_path <- here("output/fits/thresholds.RDS")

split_data_cv <- readRDS(split_path)
fc_all <- readRDS(fc_path)
alarm_thr <- readRDS(thr_path)


# Preprocessing ----------------------------------------------------------------
# Recode sites
split_data_cv <- 
  split_data_cv %>% rec_site()
fc_all <-
  fc_all %>% rec_site()


# Add alarm thresholds to fc
fc_threshold <-
  alarm_thr[
    fc_all %>% select(split, site, .model, index, occ, .mean, h),  on = "site"
    ]

# Join observed and fc occ
fc_threshold <-
  split_data_cv %>% # observed
  filter(type == "test", site != "aggregate") %>% 
  select(split, site, index, occ) %>% 
  rename(occ_obs = occ) %>% 
  as.data.table() %>% 
  .[fc_threshold, on = c("split", "site", "index")] # fc



# Compute threshold-crossing probabilities -------------------------------------
# Risk by days
risk_d <-  # compute risk
  copy(fc_threshold)[
  , risk_day := 1 - cdf(occ, thr), by = .(split, site, .model, h)
  ]
risk_d[ # add observer threshold-crossing indicator (for roc curve)
  , obs_cross := thr < occ_obs]


# Risk by week split (1-3h, 4-7h)
risk_d[ # split week in two
  , week_split := ifelse(h <= 3, "close", "far")
  ]
risk_ws <- 
  risk_d[ # compute risk & add observer threshold-crossing indicator
  , .(risk_ws = 1 - prod(1 - risk_day), obs_cross = any(obs_cross)), 
  by = .(split, site, .model, week_split) 
  ]


# Risk by week
risk_w <- 
  risk_d[ # compute risk & add thr-crossing indicator
  , .(risk_w = 1 - prod(1 - risk_day), obs_cross = any(obs_cross)), 
  by = .(split, site, .model)
  ]



# Compute ROC/PR curves --------------------------------------------------------
# Set decision boundary (db) from 1-99% prob
risk_d_db <- 
  risk_d[ 
  , 
  c(.SD,
    setNames(lapply(seq(0.99, 0.01, -0.01), function(.x) {risk_day >= .x}),
      sprintf("db_%.2f", seq(0.99, 0.01, -0.01)))
  ),
  by = .(split, site, .model),
  .SDcols = "obs_cross"
]

risk_ws_db <- 
  risk_ws[ # set threshold crossing by decision boundary (db)
    , 
    c(.SD,
      setNames(lapply(seq(0.99, 0.01, -0.01), function(.x) {risk_ws >= .x}),
               sprintf("db_%.2f", seq(0.99, 0.01, -0.01)))
    ),
    by = .(split, site, .model, week_split),
    .SDcols = "obs_cross"
  ]

risk_w_db <- 
  risk_w[
    ,
    c(
      .SD, 
      setNames(lapply(seq(0.99, 0.01, -0.01), function(.x) {risk_w >= .x}),
               sprintf("db_%.2f", seq(0.99, 0.01, -0.01)))
    ), 
    by = .(split, site, .model), 
    .SDcols = "obs_cross"
  ]

# Compute x/y axis of roc curve (bootstrapped splits)
risk_d_roc <- 
  risk_d_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "roc"), by = .(nboot, site, .model)]
risk_ws_roc <- 
  risk_ws_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "roc"), by = .(nboot, site, .model, week_split)]
risk_w_roc <- 
  risk_w_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "roc"), by = .(nboot, site, .model)]


risk_d_pr <- 
  risk_d_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "pr"), by = .(nboot, site, .model)]
risk_ws_pr <- 
  risk_ws_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "pr"), by = .(nboot, site, .model, week_split)]
risk_w_pr <- 
  risk_w_db %>% 
  boots_curves() %>% 
  .[, ax_curve_fun(.SD, "pr"), by = .(nboot, site, .model)]



# ROC AUC ----------------------------------------------------------------------
curve_fun <- 
  function(.SD){
    .Nr = nrow(.SD)
    .Nc = ncol(.SD) # 2nd to last is y, last is x
    dx <- diff(.SD[[.Nc]])
    y <- .SD[[.Nc - 1]]
    height <- (y[2:.Nr] + y[1:.Nr - 1]) / 2
    list(auc = sum(dx * height))
  } 

risk_d_roc_auc <- 
  risk_d_roc[, curve_fun(.SD), by = .(nboot, site, .model)][
    order(nboot, site, -auc)]
risk_ws_roc_auc <- 
  risk_ws_roc[, curve_fun(.SD), by = .(nboot, site, .model, week_split)][
  order(site, week_split, -auc)]
risk_w_roc_auc <- 
  risk_w_roc[, curve_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]

risk_d_pr_auc <- 
  risk_d_pr[, curve_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]
risk_ws_pr_auc <- 
  risk_ws_pr[, curve_fun(.SD), by = .(nboot, site, .model, week_split)][
  order(site, week_split, -auc)]
risk_w_pr_auc <- 
  risk_w_pr[, curve_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]



# Save -------------------------------------------------------------------------
save_path <- here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


list_risk <- 
  list(
    "risk_d" = risk_d,
    "risk_ws" = risk_ws,
    "risk_w" = risk_w
  )

list_curves <-
  list(
    "risk_d_roc" = risk_d_roc,
    "risk_ws_roc" = risk_ws_roc,
    "risk_w_roc" = risk_w_roc,
    "risk_d_pr" = risk_d_pr,
    "risk_ws_pr" = risk_ws_pr,
    "risk_w_pr" = risk_w_pr
  )

list_auc <-
  list(
    "risk_d_roc_auc" = risk_d_roc_auc,
    "risk_ws_roc_auc" = risk_ws_roc_auc,
    "risk_w_roc_auc" = risk_w_roc_auc,
    "risk_d_pr_auc" = risk_d_pr_auc,
    "risk_ws_pr_auc" = risk_ws_pr_auc,
    "risk_w_pr_auc" = risk_w_pr_auc
  )


saveRDS(list_risk, file = paste0(save_path, "risk.RDS"))
saveRDS(list_curves, file = paste0(save_path, "risk_curves.RDS"))
saveRDS(list_auc, file = paste0(save_path, "risk_auc.RDS"))
