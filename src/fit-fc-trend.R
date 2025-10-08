#' Fit and forecast trend
#' 
#' Fit structural state space model to smoothed bed occupancy to predict trend.
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-10-01



# Import packages --------------------------------------------------------------
source("src/packages.R")
source("src/functions.R")



# Load data and set seed -------------------------------------------------------
data_path <- paste0(here(), "/data/processed/tbl_occ.RDS")
ts_occ <- readRDS(file = data_path)
sites <- ts_occ$site |> unique()


# Select relevant variables
ts_occ <- 
  ts_occ %>%  select(occ, occ_s, occ_wt, days_, t_ax)



# Split dataset ------------------------------------------------------------
split_data_tt <- # Train/test set
  split_tt(ts_occ)


initial <- "16 weeks" 
assess <- "1 weeks"
skip <- "9 days"
split_data_cv <- # Cv train/validation sets
  split_cv(split_data_tt, initial, assess, skip)


splits <- split_data_cv$split %>% unique() # save cv splits names
idx_start_test <- split_data_cv$type %>% grep("test", .) %>% head(1)
# # Smooth ts --------------------------------------------------------------------
# split_data_cv_s <- # smooth train of each split
#   cv_wrap(
#     split_data_cv,
#     select_training,
#     smooth_fun,
#     .vars = c("index", "t_ax", "occ"),
#     .type = "all"
#     )
# 
# split_data_cv_s <- # put together in 
#   cv_wrap(split_data_cv_s, select_smooth, identity) %>% # identity returns input
#   unlist(recursive = F) %>% rbindlist() %>% as_tibble()



# Fit model --------------------------------------------------------------------
invisible(
  capture.output(
    fc_trend <- # smooth train of each split
      cv_wrap(
        split_data_cv,
        select_training,
        fit_trend,
        .vars = c("index", "t_ax", "occ_wt", "occ_s", "occ"),
        .type = "all"
      )
  )
)

fc_trend <- # put together in fable
  cv_wrap(fc_trend, select_smooth, identity) %>% # (identity returns input)
  unlist(recursive = F) %>% rbindlist() %>% 
  as_tsibble(index = index, key = c("split", "site")) %>% as_fable("occ", "occ")



# Save smoothed and fc data
save_path = here("output/fits/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


saveRDS(fc_trend, file = paste0(save_path, "forecast_trend.RDS"))
