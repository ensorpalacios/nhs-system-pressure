#' Predict risk high system pressure
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
split_path <- here(paste0("output/fits/", amode, "/splits_short.RDS"))
fc_path <- here(paste0("output/fits/", amode, "/forecasts_short_comb.RDS"))
thr_path <- here("output/fits/thresholds.RDS")

split_data_cv <- readRDS(split_path)
fc_all <- readRDS(fc_path)
alarm_thr <- readRDS(thr_path)


# Reproducible analysis for bootstrapping splits
set.seed(321)



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



# Compute threshold-crossing probabilities -----------------------------------
# Risk by days
risk_d <-  # compute risk
  copy(fc_threshold)[
    , risk_day := 1 - cdf(occ, thr), by = .(split, site, .model, h)
  ]
risk_d[ # add observed threshold-crossing indicator (for roc/pr curve)
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



# Save frequency high risk -----------------------------------------------------
freq_d <- 
  risk_d[
    .model == unique(.model)[1], 
    .(Prevalence = round(sum(obs_cross)/length(obs_cross), 3)), 
    by = c("site", "h")]
setnames(freq_d, "h", "time horizon")

freq_ws <- 
  risk_ws[
    .model == unique(.model)[1], 
    .(Prevalence = round(sum(obs_cross)/length(obs_cross), 3)),
    by = c("site", "week_split")]
setnames(freq_ws, "week_split", "time horizon")

freq_w <- 
  risk_w[
    .model == unique(.model)[1], 
    .(Prevalence = round(sum(obs_cross)/length(obs_cross), 3)),
    by = c("site")]



# Compute ROC/PR curves ------------------------------------------------------
# Set decision boundary (db) from 1-99% prob
risk_d_db <- 
  risk_d[ 
    , 
    c(.SD,
      setNames(lapply(seq(0.995, 0, -0.005), function(.x) {risk_day >= .x}),
               sprintf("db_%.3f", seq(0.995, 0, -0.005)))
    ),
    by = .(split, site, .model),
    .SDcols = "obs_cross"
  ]

risk_ws_db <- 
  risk_ws[ # set threshold crossing by decision boundary (db)
    , 
    c(.SD,
      setNames(lapply(seq(0.995, 0.000, -0.005), function(.x) {risk_ws >= .x}),
               sprintf("db_%.3f", seq(0.995, 0.000, -0.005)))
    ),
    by = .(split, site, .model, week_split),
    .SDcols = "obs_cross"
  ]

risk_w_db <- 
  risk_w[
    ,
    c(
      .SD, 
      setNames(lapply(seq(0.995, 0.000, -0.005), function(.x) {risk_w >= .x}),
               sprintf("db_%.3f", seq(0.995, 0.000, -0.005)))
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



# ROC AUC --------------------------------------------------------------------
risk_d_roc_auc <- 
  risk_d_roc[, auc_fun(.SD), by = .(nboot, site, .model)][
    order(nboot, site, -auc)]
risk_ws_roc_auc <- 
  risk_ws_roc[, auc_fun(.SD), by = .(nboot, site, .model, week_split)][
    order(site, week_split, -auc)]
risk_w_roc_auc <- 
  risk_w_roc[, auc_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]

risk_d_pr_auc <- 
  risk_d_pr[, auc_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]
risk_ws_pr_auc <- 
  risk_ws_pr[, auc_fun(.SD), by = .(nboot, site, .model, week_split)][
    order(site, week_split, -auc)]
risk_w_pr_auc <- 
  risk_w_pr[, auc_fun(.SD), by = .(nboot, site, .model)][order(site, -auc)]


# Save -----------------------------------------------------------------------
save_path <- here(paste0("output/fits/", amode, "/"))

if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


list_risk <- 
  list(
    "risk_d" = risk_d,
    "risk_ws" = risk_ws,
    "risk_w" = risk_w
  )

list_freq <- 
  list(
    "freq_d" = freq_d,
    "freq_ws" = freq_ws,
    "freq_w" = freq_w
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
saveRDS(list_freq, file = paste0(save_path, "freq_high.RDS"))
saveRDS(list_curves, file = paste0(save_path, "risk_curves.RDS"))
saveRDS(list_auc, file = paste0(save_path, "risk_auc.RDS"))
