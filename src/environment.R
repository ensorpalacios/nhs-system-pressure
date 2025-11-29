#' Prepare environment
#' 
#' Run this after refreshing environment at the beginning of each script.
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-26

# Load global parameters/functions ---------------------------------------------
# Load custom functions
source("src/functions.R")

# Lag/split data parameters
horizon = 7 # Forecast horizon & lag
len_test = "9 months" # Train/test
initial <- "16 weeks" # Cv split - training set
assess <- "1 weeks" # Cv split - validation set
skip <- "9 days" # Cv split - separation between training sets

# Threshold for "dangerous" occupancy
threshold_prob = 0.9 # percentile of total observed occupancy

# Colour mapping
col_models = viridis_pal(option = "turbo")(28) # Colour mapping
names(col_models) =
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpl_l",
    "arima_dadpt_l",
    "arima_dadplt_l",
    "arima_dad_rec",
    "arima_dadp_rec",
    "arima_dadpl_rec",
    "arima_dadplt_rec",
    "var_ad",
    "var_ad2",
    "var_paed",
    "var_los",
    "var_h",
    "nn",
    "es",
    "rf",
    "rf_int",
    "rf_int_not",
    "xgb",
    "xgb_not",
    "tslm",
    "snaive",
    "baseline_min",
    "crps",
    "equal",
    "crps_upper"
  )