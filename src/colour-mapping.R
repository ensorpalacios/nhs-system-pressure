#' Model colours
#' 
#' Define colour mapping for different models.
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-05-01

# Import packages --------------------------------------------------------------
source(here("src/packages.R"))


# Define colour mapping --------------------------------------------------------
# Alternative colour scales
# show_col(rainbow(12))
# show_col(hue_pal()(12))
# show_col(viridis_pal(option="turbo")(12))

col_models = viridis_pal(option = "turbo")(28) # Colour mapping
names(col_models) = 
  c(
    "arima",
    "arima_dad_l",
    "arima_dadp_l",
    "arima_dadpt_l",
    "arima_dadpl_l",
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
    "wilker"
  ) %>% as.factor()


