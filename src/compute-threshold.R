#' Compute threshold
#' 
#' Compute threshold above which consider bed occupancy dangerous
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
# Original ts, train/test split, cv split, fc (with combined models)
tt_path <- here("output/fits/train/tt_split.RDS") # always use training split!
split_data_tt <- readRDS(file = tt_path)

# Recode sites
split_data_tt <- 
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