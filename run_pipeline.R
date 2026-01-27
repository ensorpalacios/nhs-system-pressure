source("src/packages.R")
source("target/R/target_functions.R")
source("target/R/target_helper.R")
source("target/R/target_global_var.R")

path_weight <- 
  "target/data/weights_training.RDS"
ls_model_comb <-
  list(
    "BRI" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_paed", "var_h", "xgb"),
    "Southmead" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_paed", "var_los", "xgb")
  )

save_path <- here("target/data")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

hosp_data <- load_hosp()
weights <- readRDS(path_weight)
ts_file <- prepare_data(data_hosp)
thr_file <- compute_threshold(ts_file)
fc_file <- forecast_occ(ts_file)
fcc_file <- fc_combination(fc_file, weights, ls_model_comb)
risk_file <- compute_risk(fcc_file, thr_file)

