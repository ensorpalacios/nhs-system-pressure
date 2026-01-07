#' Prepare environment
#' 
#' Function to prepare the environment for other scripts.
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-26

#' @param amode Analysis mode defining data for analysis
setup_env <- 
  function(amode) {
    # Load global parameters/functions -----------------------------------------
    # Load custom functions
    source("src/functions.R")
    
    # Lag/split data parameters
    assign("horizon", 7, envir = .GlobalEnv) # Forecast horizon & lag
    assign("len_test", "10 months", envir = .GlobalEnv) # Train/test
    assign("initial", "16 weeks", envir = .GlobalEnv) # Cv split - training set
    assign("assess", "1 weeks", envir = .GlobalEnv) # Cv split - validation set
    if (amode == "train") {
      assign("type", "train", envir = .GlobalEnv) # Run cv split on train/test
      assign("skip", "9 days", envir = .GlobalEnv) # Cv separaion
    } else if (amode == "test") {
      assign("type", "test", envir = .GlobalEnv)
      assign("skip", "4 days", envir = .GlobalEnv)
    }
    
    # Threshold for "dangerous" occupancy
    assign("threshold_prob", 0.9, envir = .GlobalEnv) # percentile of total occ.
    
    # Colour mapping
    col_models = viridis_pal(option = "turbo")(29) # Colour mapping
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
        "crps_upper",
        "crps_lower"
      )
    assign("col_models", col_models, envir = .GlobalEnv)
  }