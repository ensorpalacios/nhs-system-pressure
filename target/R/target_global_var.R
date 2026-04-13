#' Prepare environment
#' 
#' Source to prepare global variables
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-26

assign("horizon", 7, envir = .GlobalEnv) # Forecast horizon & lag
assign("train_length", "16 weeks", envir = .GlobalEnv) # length of training data
assign("threshold_prob", c(0.85, 0.9, 0.95), envir = .GlobalEnv) # percentile of total occ.
    
# # Colour mapping
# col_models = viridis_pal(option = "turbo")(28) # Colour mapping
# names(col_models) =
#   c(
#     "arima",
#     "arima_dad_l",
#     "arima_dadp_l",
#     "arima_dadpl_l",
#     "arima_dadpt_l",
#     "arima_dadplt_l",
#     "arima_dad_rec",
#     "arima_dadp_rec",
#     "arima_dadpl_rec",
#     "arima_dadplt_rec",
#     "var_ad",
#     "var_ad2",
#     "var_paed",
#     "var_los",
#     "var_h",
#     "nn",
#     "es",
#     "rf",
#     "rf_int",
#     "rf_int_not",
#     "xgb",
#     "xgb_not",
#     "tslm",
#     "snaive",
#     "baseline_min",
#     "crps",
#     "equal",
#     "crps_upper"
#   )
# assign("col_models", col_models, envir = .GlobalEnv)
