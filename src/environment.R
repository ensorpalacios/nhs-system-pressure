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
    assign("len_test", "13 months", envir = .GlobalEnv) # Train/test
    assign("initial", "16 weeks", envir = .GlobalEnv) # Cv split - training set
    assign("assess", "1 weeks", envir = .GlobalEnv) # Cv split - validation set
    if (amode == "train") {
      assign("type", "train", envir = .GlobalEnv) # Run cv split on train/test
      assign("skip", "9 days", envir = .GlobalEnv) # Cv separaion
    } else if (amode == "test") {
      assign("type", "test", envir = .GlobalEnv)
      assign("skip", "5 days", envir = .GlobalEnv)
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

    # Name mapping

    newmap <- c(
      # map names based on table in paper
      arima = "ARIMA (1)",
      arima_dad_l = "ARIMA (2)",
      arima_dadp_l = "ARIMA (3)",
      arima_dadpt_l = "ARIMA (4)",
      arima_dadpl_l = "ARIMA (5)",
      arima_dadplt_l = "ARIMA (6)",
      arima_dad_rec = "ARIMA rec. (1)",
      arima_dadp_rec = "ARIMA rec. (2)",
      arima_dadpl_rec = "ARIMA rec. (3)",
      arima_dadplt_rec = "ARIMA rec. (4)",
      var_ad = "VAR (1)",
      var_ad2 = "VAR (2)",
      var_paed = "VAR (3)",
      var_los = "VAR (4)",
      var_h = "VAR (5)",
      nn = "NNAR",
      es = "ES",
      rf = "rf (1)",
      rf_int = "rf (2)",
      rf_int_not = "rf (3)",
      xgb = "XGBoost (1)",
      xgb_not = "XGBoost (2)",
      tslm = "linear model",
      snaive = "s. naive",
      crps = "ensemble (crps)",
      crps_lower = "ensemble (l. crps)",
      crps_upper = "ensemble (r. crps)",
      equal = "ensemble (equal)"
    )
    assign("newmap", newmap, envir = .GlobalEnv)
  }