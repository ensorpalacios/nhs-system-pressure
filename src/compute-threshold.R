#' Compute threshold
#' 
#' Compute threshold above which consider bed occupancy dangerous
#'
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-07-02

# Load data --------------------------------------------------------------------
# Original ts, train/test split, cv split, fc (with combined models)
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)

split_data_tt <- # Train/test set split
  split_tt(ts_occ, len_test)

split_data_tt <- # recode sites
  split_data_tt %>% rec_site()



# Compute alarm threshold ------------------------------------------------------
alarm_thr <- # compute threshold on (all) training data
  split_data_tt %>% as.data.table() %>%
  .[
    type == "train" & site != "aggregate",
    .(thr = quantile(occ, probs = threshold_prob)), by = site
    ]



# Save threshold ---------------------------------------------------------------
save_path <- here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

saveRDS(alarm_thr, file = paste0(save_path, "thresholds.RDS"))